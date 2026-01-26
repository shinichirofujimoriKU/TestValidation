source("common_flags.R")

args <- commandArgs(trailingOnly = TRUE)
arg_kv <- strsplit(args, "=", fixed = TRUE)
arg_map <- setNames(
  vapply(arg_kv, function(x) if (length(x) >= 2) x[[2]] else "", character(1)),
  vapply(arg_kv, function(x) x[[1]], character(1))
)

apply_args(arg_map)

# -----------------------------
# Load data
# -----------------------------
iamc_train <- read_iamc(IAMC_FILE_TRAIN) %>% normalize_iamc()

flags_raw <- read_csv(FLAGS_FILE, show_col_types = FALSE) %>%
  mutate(
    flag = tolower(flag),
    flag = factor(flag, levels = FLAG_LEVELS)
  )

# -----------------------------
# Build features
# -----------------------------
features_train_all <- make_features(iamc_train)

# -----------------------------
# Build training table with implicit green (①)
# -----------------------------
train_df <- features_train_all %>%
  left_join(
    flags_raw %>% select(run_id, scenario, region, variable, flag),
    by = c("run_id", "scenario", "region", "variable")
  ) %>%
  mutate(
    label_source = ifelse(is.na(flag), "implicit_green", "expert"),
    flag = ifelse(is.na(flag), "green", as.character(flag)),
    flag = factor(flag, levels = FLAG_LEVELS),
    case_weight = compute_case_weight(flag, label_source)
  ) %>%
  filter(n_years >= 3)

# Sanity check
cat("Training samples:", nrow(train_df), "\n")
print(table(train_df$flag))
print(table(train_df$label_source))

write_csv(train_df, OUT_TRAIN_CSV)

# -----------------------------
# Train model
# -----------------------------
id_cols <- c("run_id","model","scenario","region","variable","unit","flag","label_source","case_weight")
x_cols  <- setdiff(names(train_df), id_cols)

train_mat <- train_df %>%
  select(all_of(c("flag", "case_weight", x_cols))) %>%
  mutate(across(where(is.character), as.factor))

set.seed(42)
if (MODEL_TYPE == "rf") {
  fit <- ranger(
    dependent.variable.name = "flag",
    data = train_mat %>% select(-case_weight),
    probability = TRUE,
    num.trees = 800,
    mtry = max(2, floor(sqrt(length(x_cols)))),
    min.node.size = 5,
    importance = "impurity",
    case.weights = train_mat$case_weight,
    class.weights = CLASS_WEIGHTS
  )
} else if (MODEL_TYPE == "nn") {
  cc_idx <- stats::complete.cases(train_df %>% select(all_of(x_cols)))
  if (sum(!cc_idx) > 0) {
    cat("NN training: dropping", sum(!cc_idx), "rows with NA in features\n")
  }
  train_df_nn <- train_df[cc_idx, ]
  x_train <- make_design_matrix(train_df_nn, x_cols)
  finite_idx <- apply(x_train, 1, function(r) all(is.finite(r)))
  if (sum(!finite_idx) > 0) {
    cat("NN training: dropping", sum(!finite_idx), "rows with NA/NaN/Inf in features\n")
  }
  x_train <- x_train[finite_idx, , drop = FALSE]
  train_df_nn <- train_df_nn[finite_idx, ]
  y_train <- train_df_nn$flag
  y_mat <- nnet::class.ind(y_train)
  w <- train_df_nn$case_weight * unname(CLASS_WEIGHTS[as.character(y_train)])

  fit <- nnet::nnet(
    x = x_train,
    y = y_mat,
    size = NN_HIDDEN,
    decay = NN_DECAY,
    maxit = NN_MAXIT,
    softmax = TRUE,
    trace = FALSE,
    weights = w
  )
  attr(fit, "class_levels") <- levels(y_train)
  attr(fit, "x_cols") <- colnames(x_train)
} else {
  stop("Unknown MODEL_TYPE: ", MODEL_TYPE)
}

saveRDS(fit, OUT_MODEL_RDS)
cat("Saved model:", OUT_MODEL_RDS, "\n")
