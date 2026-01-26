source("common_flags.R")

args <- commandArgs(trailingOnly = TRUE)
arg_kv <- strsplit(args, "=", fixed = TRUE)
arg_map <- setNames(
  vapply(arg_kv, function(x) if (length(x) >= 2) x[[2]] else "", character(1)),
  vapply(arg_kv, function(x) x[[1]], character(1))
)

MODEL_TYPE <- ""
apply_args(arg_map)

# -----------------------------
# Load model + columns
# -----------------------------
fit <- readRDS(OUT_MODEL_RDS)

if (MODEL_TYPE == "") {
  if (inherits(fit, "ranger")) {
    MODEL_TYPE <- "rf"
  } else if (inherits(fit, "nnet")) {
    MODEL_TYPE <- "nn"
  } else {
    stop("Cannot detect MODEL_TYPE from model. Specify MODEL_TYPE=rf or MODEL_TYPE=nn.")
  }
}

train_cols_df <- read_csv(paste0(Datadir, OUT_TRAIN_CSV), n_max = 0, show_col_types = FALSE)
id_cols <- c("run_id","model","scenario","region","variable","unit","flag","label_source","case_weight")
x_cols  <- setdiff(names(train_cols_df), id_cols)

# -----------------------------
# Load data
# -----------------------------
iamc_score <- read_iamc(paste0(Datadir,"Score/", IAMC_FILE_SCORE)) %>% normalize_iamc()

# -----------------------------
# Build features
# -----------------------------
features_score_all <- make_features(iamc_score)

# -----------------------------
# Score new data
# -----------------------------
score_mat <- features_score_all %>%
  select(all_of(x_cols)) %>%
  mutate(across(where(is.character), as.factor))

if (MODEL_TYPE == "rf") {
  probs <- predict(fit, data = score_mat)$predictions
} else {
  x_train_cols <- attr(fit, "x_cols")
  if (is.null(x_train_cols)) {
    x_train_cols <- colnames(score_mat)
  }
  cc_score <- stats::complete.cases(features_score_all %>% select(all_of(x_cols)))
  if (sum(!cc_score) > 0) {
    cat("NN scoring: dropping", sum(!cc_score), "rows with NA in features\n")
  }
  x_score <- make_design_matrix(features_score_all[cc_score, ], x_cols)
  x_score <- align_design_matrix(x_train_cols, x_score)
  finite_idx <- apply(x_score, 1, function(r) all(is.finite(r)))
  if (sum(!finite_idx) > 0) {
    cat("NN scoring: dropping", sum(!finite_idx), "rows with NA/NaN/Inf in features\n")
  }
  x_score <- x_score[finite_idx, , drop = FALSE]
  probs_cc <- predict(fit, x_score, type = "raw")
  colnames(probs_cc) <- attr(fit, "class_levels")
  probs <- matrix(NA_real_, nrow = nrow(features_score_all), ncol = ncol(probs_cc))
  colnames(probs) <- colnames(probs_cc)
  cc_idx <- which(cc_score)
  cc_idx <- cc_idx[finite_idx]
  probs[cc_idx, ] <- probs_cc
}

p_green  <- get_prob(probs, "green")
p_yellow <- get_prob(probs, "yellow")
p_red    <- get_prob(probs, "red")

pred_df <- features_score_all %>%
  mutate(
    p_green = p_green,
    p_yellow = p_yellow,
    p_red = p_red,
    pred_flag = decide_flag(p_green, p_yellow, p_red),
    p_risky = p_red + 0.7 * p_yellow
  ) %>%
  arrange(desc(p_risky))

write_csv(pred_df, OUT_PRED_CSV)
cat("Saved predictions:", OUT_PRED_CSV, "\n")

cat("\nTop 20 risky candidates:\n")
print(
  pred_df %>%
    select(run_id, scenario, region, variable, unit, var_group, pred_flag, p_green, p_yellow, p_red, p_risky) %>%
    head(20)
)

cat("\nTop 20 feature importances:\n")
if (MODEL_TYPE == "rf") {
  imp <- sort(fit$variable.importance, decreasing = TRUE)
  print(head(imp, 20))
} else {
  cat("(NN model: feature importance not available)\n")
}
