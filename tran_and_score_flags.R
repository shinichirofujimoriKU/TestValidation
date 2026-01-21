# train_and_score_flags.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(lubridate)
  library(ranger)
})

# -----------------------------
# Settings
# -----------------------------
IAMC_FILE_TRAIN <- "iamc_timeseries_train.csv"
IAMC_FILE_SCORE <- "iamc_timeseries_score.csv"
FLAGS_FILE      <- "expert_flags.csv"
OUT_MODEL_RDS    <- "flag_classifier_ranger.rds"
OUT_FEATURES_CSV <- "features_for_training.csv"
OUT_PRED_CSV     <- "predicted_flags.csv"

# 学習に使う年（例：2030-2100）
YEAR_MIN <- 2020
YEAR_MAX <- 2100

# 予測のラベル順
FLAG_LEVELS <- c("green", "yellow", "red")

# -----------------------------
# Helper: feature engineering
# -----------------------------
# IAMC long format expected:
# run_id, model, scenario, region, variable, unit, year, value
make_features <- function(iamc_df) {
  
  df <- iamc_df %>%
    mutate(
      year = as.integer(year),
      value = as.numeric(value)
    ) %>%
    filter(!is.na(value), year >= YEAR_MIN, year <= YEAR_MAX)
  
  # Group by scenario-region-variable and compute time-series features
  feats <- df %>%
    group_by(run_id, model, scenario, region, variable, unit) %>%
    arrange(year, .by_group = TRUE) %>%
    summarise(
      n_years = n(),
      first_year = min(year),
      last_year  = max(year),
      first_val  = value[which.min(year)],
      last_val   = value[which.max(year)],
      min_val    = min(value, na.rm = TRUE),
      max_val    = max(value, na.rm = TRUE),
      mean_val   = mean(value, na.rm = TRUE),
      sd_val     = sd(value, na.rm = TRUE),
      
      # step changes
      max_abs_step = {
        if (n() >= 2) max(abs(diff(value)), na.rm = TRUE) else NA_real_
      },
      max_rel_step = {
        if (n() >= 2) {
          denom <- pmax(abs(head(value, -1)), 1e-9)
          max(abs(diff(value)) / denom, na.rm = TRUE)
        } else NA_real_
      },
      
      # growth rates (log-diff) for positive values
      mean_log_growth = {
        if (n() >= 2) {
          vv <- ifelse(value <= 0, NA_real_, value)
          lg <- diff(log(vv))
          mean(lg, na.rm = TRUE)
        } else NA_real_
      },
      max_log_growth = {
        if (n() >= 2) {
          vv <- ifelse(value <= 0, NA_real_, value)
          lg <- diff(log(vv))
          suppressWarnings(max(lg, na.rm = TRUE))
        } else NA_real_
      },
      
      # trend: simple linear slope (value ~ year)
      slope = {
        if (n() >= 3) {
          tryCatch({
            coef(lm(value ~ year))[["year"]]
          }, error = function(e) NA_real_)
        } else NA_real_
      },
      
      # non-smoothness: mean absolute second difference
      mean_abs_2nd_diff = {
        if (n() >= 3) {
          mean(abs(diff(value, differences = 2)), na.rm = TRUE)
        } else NA_real_
      },
      
      .groups = "drop"
    )
  
  # add some generic transformed features (stabilize scale)
  feats %>%
    mutate(
      range_val = max_val - min_val,
      abs_first_val = abs(first_val),
      abs_last_val  = abs(last_val),
      log1p_mean_abs = log1p(abs(mean_val)),
      log1p_max_abs  = log1p(abs(max(abs(min_val), abs(max_val)))),
      log1p_range    = log1p(abs(range_val)),
      # avoid NaN
      sd_val = ifelse(is.na(sd_val), 0, sd_val)
    )
}

# -----------------------------
# Load data
# -----------------------------
iamc_train <- read_csv(IAMC_FILE_TRAIN, show_col_types = FALSE)
iamc_score <- read_csv(IAMC_FILE_SCORE, show_col_types = FALSE)

flags_raw <- read_csv(FLAGS_FILE, show_col_types = FALSE) %>%
  mutate(
    flag = tolower(flag),
    flag = factor(flag, levels = FLAG_LEVELS)
  )


# -----------------------------
# Make features and join labels
# -----------------------------
features_train_all <- make_features(iamc_train)
features_score_all <- make_features(iamc_score)


train_df <- features_train_all %>%
  left_join(
    flags_raw %>% select(run_id, scenario, region, variable, flag),
    by = c("run_id", "scenario", "region", "variable")
  ) %>%
  mutate(
    flag = ifelse(is.na(flag), "green", as.character(flag)),
    flag = factor(flag, levels = FLAG_LEVELS)
  ) %>%
  filter(n_years >= 3)


# Save features snapshot (optional)
write_csv(train_df, OUT_FEATURES_CSV)

# -----------------------------
# Train model
# -----------------------------
# Define predictors: exclude identifiers and unit text fields
id_cols <- c("run_id", "model", "scenario", "region", "variable", "unit", "flag")
x_cols <- setdiff(names(train_df), id_cols)

# Ranger requires no character predictors (convert remaining chars if any)
train_mat <- train_df %>%
  select(all_of(c("flag", x_cols))) %>%
  mutate(across(where(is.character), as.factor))

set.seed(42)
fit <- ranger(
  dependent.variable.name = "flag",
  data = train_mat,
  probability = TRUE,
  num.trees = 600,
  mtry = max(2, floor(sqrt(length(x_cols)))),
  min.node.size = 5,
  importance = "impurity"
)

saveRDS(fit, OUT_MODEL_RDS)

cat("\nSaved model to:", OUT_MODEL_RDS, "\n")

# -----------------------------
# Score: predict on all rows (including unlabeled)
# -----------------------------
score_mat <- features_score_all %>%
  select(all_of(x_cols)) %>%
  mutate(across(where(is.character), as.factor))

probs <- predict(fit, data = score_mat)$predictions
# probs is matrix: columns = levels(flag)

get_prob <- function(probs, cls) {
  if (cls %in% colnames(probs)) {
    probs[, cls]
  } else {
    rep(0, nrow(probs))
  }
}

pred_flag <- colnames(probs)[max.col(probs, ties.method = "first")]

pred_df <- features_score_all %>%
  mutate(
    pred_flag = factor(pred_flag, levels = FLAG_LEVELS),
    p_green  = get_prob(probs, "green"),
    p_yellow = get_prob(probs, "yellow"),
    p_red    = get_prob(probs, "red"),
    p_risky  = p_red + 0.7 * p_yellow
  ) %>%
  arrange(desc(p_risky))

# Export predictions
write_csv(pred_df, OUT_PRED_CSV)
cat("Saved predictions to:", OUT_PRED_CSV, "\n")

# Show top 20 risky items
cat("\nTop 20 risky candidates:\n")
print(
  pred_df %>%
    select(run_id, scenario, region, variable, unit, pred_flag, p_green, p_yellow, p_red, p_risky) %>%
    head(20)
)

# Show variable importance
cat("\nTop 20 feature importances:\n")
imp <- sort(fit$variable.importance, decreasing = TRUE)
print(head(imp, 20))
