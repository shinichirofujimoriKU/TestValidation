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
# Settings
# -----------------------------
IAMC_FILE_TRAIN <- "iamc_timeseries_train.csv"
IAMC_FILE_TRAIN <- "iamc_timeseries_train.xlsx"
IAMC_FILE_SCORE <- "iamc_timeseries_score.csv"
FLAGS_FILE      <- "expert_flags.csv"

OUT_MODEL_RDS   <- "flag_classifier_ranger_v2.rds"
OUT_PRED_CSV    <- "predicted_flags_v2.csv"
OUT_TRAIN_CSV   <- "training_table_v2.csv"

YEAR_MIN <- 2000
YEAR_MAX <- 2100

USE_PARALLEL <- TRUE
N_CORES <- max(1, parallel::detectCores(logical = FALSE) - 1)
USE_DATATABLE <- TRUE
DT_THREADS <- N_CORES

MODEL_TYPE <- "rf"  # "rf" or "nn"
MODEL_TYPE <- "nn"

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
W_IMPLICIT_GREEN  <- 0.15
W_EXPERT_YELLOW   <- 2.0
W_EXPERT_RED      <- 3.0

# class.weights（クラス不均衡対策）
CLASS_WEIGHTS <- c(green = 1, yellow = 5, red = 10)

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

  compute_one <- function(d) {
    d <- d %>% arrange(year)
    tibble(
      n_years = nrow(d),
      first_year = min(d$year),
      last_year  = max(d$year),
      first_val  = d$value[which.min(d$year)],
      last_val   = d$value[which.max(d$year)],
      min_val    = min(d$value, na.rm = TRUE),
      max_val    = max(d$value, na.rm = TRUE),
      mean_val   = mean(d$value, na.rm = TRUE),
      sd_val     = sd(d$value, na.rm = TRUE),

      max_abs_step = if (nrow(d) >= 2) max(abs(diff(d$value)), na.rm = TRUE) else NA_real_,
      max_rel_step = if (nrow(d) >= 2) {
        denom <- pmax(abs(head(d$value, -1)), 1e-9)
        max(abs(diff(d$value)) / denom, na.rm = TRUE)
      } else NA_real_,

      mean_log_growth = if (nrow(d) >= 2) {
        vv <- ifelse(d$value <= 0, NA_real_, d$value)
        lg <- diff(log(vv))
        mean(lg, na.rm = TRUE)
      } else NA_real_,

      max_log_growth = if (nrow(d) >= 2) {
        vv <- ifelse(d$value <= 0, NA_real_, d$value)
        lg <- diff(log(vv))
        suppressWarnings(max(lg, na.rm = TRUE))
      } else NA_real_,

      slope = if (nrow(d) >= 3) {
        tryCatch(coef(lm(value ~ year, data = d))[["year"]], error = function(e) NA_real_)
      } else NA_real_,

      mean_abs_2nd_diff = if (nrow(d) >= 3) mean(abs(diff(d$value, differences = 2)), na.rm = TRUE) else NA_real_
    )
  }

  if (USE_DATATABLE) {
    data.table::setDTthreads(DT_THREADS)
    dt <- as.data.table(df)
    setkey(dt, run_id, model, scenario, region, variable, unit, year)

    feats <- dt[, {
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

      slope <- if (n >= 3) {
        tryCatch(coef(lm(vv ~ yy))[["yy"]], error = function(e) NA_real_)
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
        slope = slope,
        mean_abs_2nd_diff = mean_abs_2nd_diff
      )
    }, by = .(run_id, model, scenario, region, variable, unit)]
  } else if (USE_PARALLEL && N_CORES > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = N_CORES)

    grouped <- df %>% group_by(run_id, model, scenario, region, variable, unit)
    keys_df <- group_keys(grouped)
    groups <- group_split(grouped)
    feats <- furrr::future_map(groups, compute_one, .options = furrr::furrr_options(seed = TRUE)) %>%
      bind_rows() %>%
      bind_cols(keys_df)
  } else {
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

        max_abs_step = if (n() >= 2) max(abs(diff(value)), na.rm = TRUE) else NA_real_,
        max_rel_step = if (n() >= 2) {
          denom <- pmax(abs(head(value, -1)), 1e-9)
          max(abs(diff(value)) / denom, na.rm = TRUE)
        } else NA_real_,

        mean_log_growth = if (n() >= 2) {
          vv <- ifelse(value <= 0, NA_real_, value)
          lg <- diff(log(vv))
          mean(lg, na.rm = TRUE)
        } else NA_real_,

        max_log_growth = if (n() >= 2) {
          vv <- ifelse(value <= 0, NA_real_, value)
          lg <- diff(log(vv))
          suppressWarnings(max(lg, na.rm = TRUE))
        } else NA_real_,

        slope = if (n() >= 3) {
          tryCatch(coef(lm(value ~ year))[["year"]], error = function(e) NA_real_)
        } else NA_real_,

        mean_abs_2nd_diff = if (n() >= 3) mean(abs(diff(value, differences = 2)), na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )
  }

  feats <- feats %>%
    mutate(
      range_val = max_val - min_val,
      abs_first_val = abs(first_val),
      abs_last_val  = abs(last_val),
      log1p_mean_abs = log1p(abs(mean_val)),
      log1p_max_abs  = log1p(pmax(abs(min_val), abs(max_val))),
      log1p_range    = log1p(abs(range_val)),
      sd_val = ifelse(is.na(sd_val), 0, sd_val)
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

# probs helper
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
iamc_score <- read_iamc(IAMC_FILE_SCORE) %>% normalize_iamc()

flags_raw <- read_csv(FLAGS_FILE, show_col_types = FALSE) %>%
  mutate(
    flag = tolower(flag),
    flag = factor(flag, levels = FLAG_LEVELS)
  )

# -----------------------------
# Build features
# -----------------------------
features_train_all <- make_features(iamc_train)
features_score_all <- make_features(iamc_score)

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
  x_train <- make_design_matrix(train_df, x_cols)
  y_train <- train_df$flag
  y_mat <- nnet::class.ind(y_train)
  w <- train_df$case_weight * unname(CLASS_WEIGHTS[as.character(y_train)])

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
# Score new data
# -----------------------------
score_mat <- features_score_all %>%
  select(all_of(x_cols)) %>%
  mutate(across(where(is.character), as.factor))

if (MODEL_TYPE == "rf") {
  probs <- predict(fit, data = score_mat)$predictions
} else {
  x_train_cols <- attr(fit, "x_cols")
  x_score <- make_design_matrix(features_score_all, x_cols)
  x_score <- align_design_matrix(x_train_cols, x_score)
  probs <- predict(fit, x_score, type = "raw")
  colnames(probs) <- attr(fit, "class_levels")
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
