suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(future)
  library(furrr)
  library(data.table)
  library(nnet)
  library(lubridate)
  library(ranger)
})

# -----------------------------
# Settings (shared)
# -----------------------------
outputdir <- "../output/"
Datadir <- "../data/"

IAMC_FILE_TRAIN <- paste0(Datadir,"iamc_timeseries_train.csv")
IAMC_FILE_TRAIN <- paste0(Datadir,"iamc_timeseries_train.xlsx")
IAMC_FILE_TRAIN <- paste0(Datadir,"TrainData/validation_2026-01-24-17-55.csv")
IAMC_FILE_SCORE <- "iamc_timeseries_score.csv"
IAMC_FILE_SCORE <- "iamc_timeseries_train.xlsx"
IAMC_FILE_SCORE <- "250903ScenarioMIPFinal.csv"
FLAGS_FILE      <- paste0(Datadir,"Flag/validation_woheader_2026-01-24-17-55.csv")

OUT_MODEL_RDS   <- paste0(outputdir,"flag_classifier_ranger_v2.rds")
OUT_TRAIN_CSV   <- paste0(outputdir,"training_table_v2.csv")
OUT_PRED_CSV    <- paste0(outputdir,"predicted_flags_v2.csv")
OUT_PRED_CSV    <- paste0(outputdir,"predicted_flags_v2",IAMC_FILE_SCORE)

YEAR_MIN <- 2010
YEAR_MAX <- 2100

USE_PARALLEL <- TRUE
N_CORES <- max(1, parallel::detectCores(logical = FALSE) - 1)
USE_DATATABLE <- TRUE
DT_THREADS <- N_CORES
MODEL_TYPE <- "rf"  # default
SLOPE_PARALLEL <- TRUE

if (.Platform$OS.type == "windows") {
  SLOPE_PARALLEL <- FALSE
}

NN_HIDDEN <- 16
NN_DECAY  <- 1e-4
NN_MAXIT  <- 300

FLAG_LEVELS <- c("green", "yellow", "red")

# Threshold-based decision (③)
TH_RED    <- 0.60
TH_YELLOW <- 0.60

# Weights
# ① implicit_green を弱く扱う（ケースウェイト）
W_EXPERT_GREEN    <- 1.0
W_IMPLICIT_GREEN  <- 0.01
W_EXPERT_YELLOW   <- 2.0
W_EXPERT_RED      <- 3.0

# class.weights（クラス不均衡対策）
CLASS_WEIGHTS <- c(green = 1, yellow = 5, red = 10)

# -----------------------------
# Args helper
# -----------------------------
apply_args <- function(arg_map) {
  if ("N_CORES" %in% names(arg_map) && nzchar(arg_map[["N_CORES"]])) {
    n_arg <- suppressWarnings(as.integer(arg_map[["N_CORES"]]))
    if (!is.na(n_arg) && n_arg >= 1) {
      N_CORES <<- n_arg
      DT_THREADS <<- N_CORES
    }
  }

  if ("MODEL_TYPE" %in% names(arg_map) && nzchar(arg_map[["MODEL_TYPE"]])) {
    mt <- tolower(arg_map[["MODEL_TYPE"]])
    if (mt %in% c("rf", "nn")) {
      MODEL_TYPE <<- mt
    }
  }

  if (USE_DATATABLE) {
    data.table::setDTthreads(DT_THREADS)
    cat("data.table threads:", data.table::getDTthreads(), "\n")
  }
}

# -----------------------------
# Helper: feature engineering
# -----------------------------
make_features <- function(iamc_df) {

  df <- iamc_df %>%
    mutate(
      year = as.integer(year),
      value = as.numeric(value)
    ) %>%
    filter(!is.na(value), year >= YEAR_MIN, year <= YEAR_MAX)

  data.table::setDTthreads(DT_THREADS)
  dt <- as.data.table(df)
  setkey(dt, run_id, model, scenario, region, variable, unit, year)

  compute_dt_feats <- function(dt_part) {
    dt_part[, {
      yy <- year[order(year)]
      vv <- value[order(year)]
      n <- length(vv)

      first_val <- vv[which.min(yy)]
      last_val  <- vv[which.max(yy)]
      min_val   <- min(vv, na.rm = TRUE)
      max_val   <- max(vv, na.rm = TRUE)
      mean_val  <- mean(vv, na.rm = TRUE)
      sd_val    <- sd(vv, na.rm = TRUE)

      max_abs_step <- if (n >= 2) max(abs(diff(vv)), na.rm = TRUE) else NA_real_
      max_rel_step <- if (n >= 2) {
        denom <- pmax(abs(head(vv, -1)), 1e-9)
        max(abs(diff(vv)) / denom, na.rm = TRUE)
      } else NA_real_

      mean_log_growth <- if (n >= 2) {
        vv2 <- ifelse(vv <= 0, NA_real_, vv)
        lg <- diff(log(vv2))
        mean(lg, na.rm = TRUE)
      } else NA_real_

      max_log_growth <- if (n >= 2) {
        vv2 <- ifelse(vv <= 0, NA_real_, vv)
        lg <- diff(log(vv2))
        suppressWarnings(max(lg, na.rm = TRUE))
      } else NA_real_

      mean_abs_2nd_diff <- if (n >= 3) mean(abs(diff(vv, differences = 2)), na.rm = TRUE) else NA_real_

      .(
        n_years = n,
        first_year = min(yy),
        last_year  = max(yy),
        first_val = first_val,
        last_val = last_val,
        min_val = min_val,
        max_val = max_val,
        mean_val = mean_val,
        sd_val = sd_val,
        max_abs_step = max_abs_step,
        max_rel_step = max_rel_step,
        mean_log_growth = mean_log_growth,
        max_log_growth = max_log_growth,
        mean_abs_2nd_diff = mean_abs_2nd_diff
      )
    }, by = .(run_id, model, scenario, region, variable, unit)]
  }

  feats <- compute_dt_feats(dt)
  print("slope cal")

  slope_summary <- dt[, .(
    n = .N,
    sum_x = sum(year),
    sum_y = sum(value),
    sum_xy = sum(year * value),
    sum_x2 = sum(year * year)
  ), by = .(run_id, model, scenario, region, variable, unit)]

  print("slope calc")
  slope_dt <- as.data.table(slope_summary)
  slope_dt[, denom := n * sum_x2 - sum_x^2]
  slope_dt[, slope := ifelse(n >= 3 & denom != 0, (n * sum_xy - sum_x * sum_y) / denom, NA_real_)]
  slope_dt[, denom := NULL]

  print("slope para join")
  feats <- feats %>%
    left_join(
      slope_dt %>% select(run_id, model, scenario, region, variable, unit, slope),
      by = c("run_id", "model", "scenario", "region", "variable", "unit")
    )

  feats <- feats %>%
    mutate(
      range_val = max_val - min_val,
      abs_first_val = abs(first_val),
      abs_last_val  = abs(last_val),
      log1p_mean_abs = log1p(abs(mean_val)),
      log1p_max_abs  = log1p(pmax(abs(min_val), abs(max_val))),
      log1p_range    = log1p(abs(range_val)),
      sd_val = ifelse(is.na(sd_val), 0, sd_val),
      mean_log_growth = ifelse(is.na(mean_log_growth), 0, mean_log_growth),
      max_log_growth = ifelse(is.na(max_log_growth), 0, max_log_growth),
      max_abs_step = ifelse(is.na(max_abs_step), 0, max_abs_step),
      max_rel_step = ifelse(is.na(max_rel_step), 0, max_rel_step),
      mean_abs_2nd_diff = ifelse(is.na(mean_abs_2nd_diff), 0, mean_abs_2nd_diff),
      slope = ifelse(is.na(slope), 0, slope)
    )

  # ④ variable を弱い特徴量として入れる：粗いカテゴリ化
  feats %>%
    mutate(
      var_group = case_when(
        str_detect(variable, regex("^Emi|Emissions|^Emissions\\|", ignore_case = TRUE)) ~ "emissions",
        str_detect(variable, regex("Price|CarbonPrice|Cost", ignore_case = TRUE)) ~ "price_cost",
        str_detect(variable, regex("Final|Energy|Elec|Power|Heat", ignore_case = TRUE)) ~ "energy",
        str_detect(variable, regex("CCS|Sequestration|CDR|DAC|BECCS", ignore_case = TRUE)) ~ "cdr_ccs",
        str_detect(variable, regex("Land|Crop|Forest|Bio", ignore_case = TRUE)) ~ "land",
        TRUE ~ "other"
      ) %>% factor(levels = c("emissions","price_cost","energy","cdr_ccs","land","other"))
    )
}

# -----------------------------
# Helper: normalize IAMC input
# -----------------------------
pick_first <- function(candidates, df) {
  found <- candidates[candidates %in% names(df)]
  if (length(found) == 0) return(NULL)
  found[[1]]
}

normalize_iamc <- function(df) {
  names(df) <- names(df) %>%
    str_trim() %>%
    str_replace_all("\\s+", "_") %>%
    str_replace_all("[.\\-]", "_") %>%
    str_to_lower()

  rename_map <- list(
    model = pick_first(c("model", "model_name", "modelname"), df),
    scenario = pick_first(c("scenario", "scenario_name", "scen", "scenarioid"), df),
    region = pick_first(c("region", "region_name", "reg"), df),
    variable = pick_first(c("variable", "var", "variable_name"), df),
    unit = pick_first(c("unit", "units"), df),
    run_id = pick_first(c("run_id", "runid", "run"), df),
    year = pick_first(c("year", "yr", "time"), df),
    value = pick_first(c("value", "val", "values"), df)
  )

  for (nm in names(rename_map)) {
    src <- rename_map[[nm]]
    if (!is.null(src) && src != nm) {
      df <- df %>% rename(!!nm := all_of(src))
    }
  }

  has_year_value <- all(c("year", "value") %in% names(df))
  year_cols <- names(df)[str_detect(names(df), "^x?\\d{4}$")]

  if (!has_year_value && length(year_cols) > 0) {
    df <- df %>%
      pivot_longer(
        cols = all_of(year_cols),
        names_to = "year",
        values_to = "value"
      ) %>%
      mutate(year = as.integer(str_remove(year, "^x")))
  }

  required <- c("model", "scenario", "region", "variable", "unit", "year", "value")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  if (!"run_id" %in% names(df)) {
    df <- df %>% mutate(run_id = paste(model, scenario, sep = " | "))
  }

  df
}

read_iamc <- function(path) {
  if (str_detect(tolower(path), "\\.xlsx$")) {
    read_excel(path, sheet = "data")
  } else {
    read_csv(path, show_col_types = FALSE)
  }
}

# -----------------------------
# Shared helpers
# -----------------------------
get_prob <- function(probs, cls) {
  if (!is.null(dim(probs)) && cls %in% colnames(probs)) probs[, cls] else rep(0, nrow(probs))
}

# Threshold decision (③)
decide_flag <- function(p_green, p_yellow, p_red, th_red = TH_RED, th_yellow = TH_YELLOW) {
  out <- ifelse(p_red >= th_red, "red",
                ifelse(p_yellow >= th_yellow, "yellow", "green"))
  factor(out, levels = FLAG_LEVELS)
}

# ① case weights: treat implicit_green weakly
compute_case_weight <- function(flag, label_source) {
  ifelse(label_source == "implicit_green" & flag == "green", W_IMPLICIT_GREEN,
         ifelse(label_source == "expert" & flag == "green", W_EXPERT_GREEN,
                ifelse(flag == "yellow", W_EXPERT_YELLOW, W_EXPERT_RED)))
}

make_design_matrix <- function(df, x_cols) {
  mm <- model.matrix(~ . - 1, data = df %>% select(all_of(x_cols)))
  as.matrix(mm)
}

align_design_matrix <- function(x_train_cols, x_score) {
  missing_in_score <- setdiff(x_train_cols, colnames(x_score))
  if (length(missing_in_score) > 0) {
    add <- matrix(0, nrow = nrow(x_score), ncol = length(missing_in_score))
    colnames(add) <- missing_in_score
    x_score <- cbind(x_score, add)
  }
  extra_in_score <- setdiff(colnames(x_score), x_train_cols)
  if (length(extra_in_score) > 0) {
    x_score <- x_score[, setdiff(colnames(x_score), extra_in_score), drop = FALSE]
  }
  x_score <- x_score[, x_train_cols, drop = FALSE]
  x_score
}


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
