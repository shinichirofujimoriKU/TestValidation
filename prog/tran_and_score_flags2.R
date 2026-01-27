suppressPackageStartupMessages({
  library(reticulate)
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
  library(keras)
  library(lubridate)
  library(ranger)
})

#Sys.setenv(RETICULATE_PYTHON = "C:/Users/sfuji/AppData/Local/Programs/Python/Python311/python.exe")
#reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)
#source("../prog/util_python_setup.R")
#setup_python()
py_require("tensorflow")
py_config()
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


RUN_TRAIN <- TRUE
RUN_SCORE <- TRUE
MODEL_ID <- ""

YEAR_MIN <- 2010
YEAR_MAX <- 2100

USE_PARALLEL <- TRUE
N_CORES <- max(1, parallel::detectCores(logical = FALSE) - 1)
USE_DATATABLE <- TRUE
DT_THREADS <- N_CORES
MODEL_TYPE <- "rf"  # default: "rf", "nn", "rnn"
SLOPE_PARALLEL <- TRUE

if (.Platform$OS.type == "windows") {
  SLOPE_PARALLEL <- FALSE
}

FLAG_LEVELS <- c("green", "yellow", "red")

# -----------------------------
# Args helper
# -----------------------------
parse_args <- function(args) {
  arg_kv <- strsplit(args, "=", fixed = TRUE)
  keys <- vapply(arg_kv, function(x) x[[1]], character(1))
  vals <- vapply(arg_kv, function(x) if (length(x) >= 2) x[[2]] else "", character(1))
  setNames(vals, keys)
}

get_arg <- function(arg_map, key, default = "") {
  if (!key %in% names(arg_map)) return(default)
  val <- arg_map[[key]]
  if (nzchar(val)) val else default
}

get_arg_bool <- function(arg_map, key, default = NULL) {
  val <- get_arg(arg_map, key, "")
  if (!nzchar(val)) return(default)
  tolower(val) %in% c("1", "true", "yes", "y")
}

apply_args <- function(arg_map) {
  n_cores_arg <- suppressWarnings(as.integer(get_arg(arg_map, "N_CORES", "")))
  if (!is.na(n_cores_arg) && n_cores_arg >= 1) {
    N_CORES <<- n_cores_arg
    DT_THREADS <<- N_CORES
  }

  mt <- tolower(get_arg(arg_map, "MODEL_TYPE", ""))
  if (mt %in% c("rf", "nn", "rnn")) {
    MODEL_TYPE <<- mt
  }

  rt <- get_arg_bool(arg_map, "RUN_TRAIN")
  if (!is.null(rt)) RUN_TRAIN <<- rt

  rs <- get_arg_bool(arg_map, "RUN_SCORE")
  if (!is.null(rs)) RUN_SCORE <<- rs

  print(MODEL_TYPE)
  if (USE_DATATABLE) {
    data.table::setDTthreads(DT_THREADS)
    cat("data.table threads:", data.table::getDTthreads(), "\n")
  }
}

arg_map <- parse_args(commandArgs(trailingOnly = TRUE))
apply_args(arg_map)

MODEL_ID <- MODEL_TYPE
OUT_MODEL_RDS <- paste0(outputdir, "flag_classifier_", MODEL_ID,IAMC_FILE_SCORE, ".rds")
OUT_TRAIN_CSV <- paste0(outputdir, "training_table_", MODEL_ID,IAMC_FILE_SCORE, ".csv")
OUT_PRED_CSV  <- paste0(outputdir, "predicted_flags_", MODEL_ID,IAMC_FILE_SCORE, ".csv")

print(c("MODEL_TYPE",MODEL_TYPE," RUN_TRAIN",RUN_TRAIN," RUN_SCORE",RUN_SCORE))

#PARAMETER
NN_HIDDEN <- 16
NN_DECAY  <- 1e-4
NN_MAXIT  <- 300

RNN_UNITS <- 32
RNN_EPOCHS <- 30
RNN_BATCH_SIZE <- 64

# Threshold-based decision ()
  TH_RED    <- 0.3
  TH_YELLOW <- 0.3

if (MODEL_TYPE == "rnn") {
  TH_RED    <- 0.2
  TH_YELLOW <- 0.2
} else if (MODEL_TYPE=="nn") {
  TH_RED    <- 0.35
  TH_YELLOW <- 0.35
}

# Weights
# week assumptions on implicit_green 
W_EXPERT_GREEN    <- 1.0
W_IMPLICIT_GREEN  <- 0.01
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

one_hot <- function(y, classes) {
  y_int <- match(y, classes)
  if (any(is.na(y_int))) {
    stop("Unknown class labels found in y.")
  }
  mat <- matrix(0, nrow = length(y_int), ncol = length(classes))
  mat[cbind(seq_along(y_int), y_int)] <- 1
  mat
}

compute_n_years <- function(iamc_df) {
  dt <- as.data.table(iamc_df)
  dt <- dt[year >= YEAR_MIN & year <= YEAR_MAX]
  dt[, .(n_years = .N), by = .(run_id, model, scenario, region, variable, unit)]
}

build_sequences <- function(iamc_df, years = YEAR_MIN:YEAR_MAX) {
  years <- sort(unique(as.integer(years)))
  df <- iamc_df %>%
    mutate(
      year = as.integer(year),
      value = as.numeric(value)
    ) %>%
    filter(year >= YEAR_MIN, year <= YEAR_MAX)

  dt <- as.data.table(df)
  key_cols <- c("run_id", "model", "scenario", "region", "variable", "unit")
  dt <- dt[, .(value = mean(value, na.rm = TRUE)), by = c(key_cols, "year")]

  keys_dt <- unique(dt[, ..key_cols])
  full_dt <- keys_dt[, .(year = years), by = key_cols]
  full_dt <- full_dt[dt, on = c(key_cols, "year")]

  full_dt[, missing := is.na(value)]
  full_dt[is.na(value), value := 0]

  setorder(full_dt, run_id, model, scenario, region, variable, unit, year)
  full_dt[, grp := .GRP, by = key_cols]

  n_groups <- nrow(keys_dt)
  timesteps <- length(years)
  x <- array(0, dim = c(n_groups, timesteps, 2))

  for (g in seq_len(n_groups)) {
    sub <- full_dt[grp == g]
    idx <- match(years, sub$year)
    v <- rep(0, timesteps)
    m <- rep(1, timesteps)
    hit <- which(!is.na(idx))
    if (length(hit) > 0) {
      v[hit] <- sub$value[idx[hit]]
      m[hit] <- as.numeric(sub$missing[idx[hit]])
    }
    x[g, , 1] <- v
    x[g, , 2] <- m
  }

  list(x = x, keys = keys_dt, years = years)
}


if (RUN_TRAIN) {
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
  } else if (MODEL_TYPE == "rnn") {
    seq_train <- build_sequences(iamc_train)
    train_keys <- as.data.frame(seq_train$keys)
    train_keys$seq_id <- seq_len(nrow(train_keys))

    n_years_df <- compute_n_years(iamc_train)

    train_df <- train_keys %>%
      left_join(n_years_df, by = c("run_id", "model", "scenario", "region", "variable", "unit")) %>%
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

    seq_ids <- train_df$seq_id
    x_train <- seq_train$x[seq_ids, , , drop = FALSE]

    finite_idx <- apply(x_train, 1, function(r) all(is.finite(r)))
    if (sum(!finite_idx) > 0) {
      cat("RNN training: dropping", sum(!finite_idx), "rows with NA/NaN/Inf in sequences\n")
    }
    x_train <- x_train[finite_idx, , , drop = FALSE]
    train_df <- train_df[finite_idx, ]

    y_train <- factor(train_df$flag, levels = FLAG_LEVELS)
    y_mat <- one_hot(y_train, FLAG_LEVELS)
    w <- train_df$case_weight * unname(CLASS_WEIGHTS[as.character(y_train)])

    inputs <- layer_input(shape = c(dim(x_train)[2], dim(x_train)[3]))
    x <- inputs %>%
      layer_masking(mask_value = 0) %>%
      layer_lstm(units = RNN_UNITS) %>%
      layer_dense(units = length(FLAG_LEVELS), activation = "softmax")
    model <- keras_model(inputs = inputs, outputs = x)

    model$compile(
      optimizer = "adam",
      loss = "categorical_crossentropy",
      metrics = list("accuracy")
    )

    np <- reticulate::import("numpy", delay_load = TRUE)
    tf <- reticulate::import("tensorflow", delay_load = TRUE)
    x_train_np <- np$array(x_train, dtype = "float32")
    y_mat_np <- np$array(y_mat, dtype = "float32")
    w_np <- np$array(w, dtype = "float32")

    ds <- tf$data$Dataset$from_tensor_slices(
      reticulate::tuple(x_train_np, y_mat_np, w_np)
    )$shuffle(as.integer(dim(x_train_np)[1]))$batch(as.integer(RNN_BATCH_SIZE))

    model$fit(
      x = ds,
      epochs = as.integer(RNN_EPOCHS),
      verbose = 1
    )

    fit <- model
    attr(fit, "class_levels") <- levels(y_train)
    attr(fit, "years") <- seq_train$years
  } else {
    stop("Unknown MODEL_TYPE: ", MODEL_TYPE)
  }

  if (MODEL_TYPE == "rnn") {
    OUT_MODEL_KERAS <- paste0(outputdir, "flag_classifier_", MODEL_ID, ".keras")
    OUT_MODEL_META <- paste0(outputdir, "flag_classifier_", MODEL_ID, "_meta.rds")
    kpy <- reticulate::import("keras", delay_load = TRUE)
    kpy$models$save_model(fit, OUT_MODEL_KERAS)
    saveRDS(list(class_levels = attr(fit, "class_levels"), years = attr(fit, "years")), OUT_MODEL_META)
    cat("Saved model:", OUT_MODEL_KERAS, "\n")
  } else {
    saveRDS(fit, OUT_MODEL_RDS)
    cat("Saved model:", OUT_MODEL_RDS, "\n")
  }
}

# -----------------------------
# Load model + columns
# -----------------------------
if (RUN_SCORE) {
  if (MODEL_TYPE == "rnn") {
    OUT_MODEL_KERAS <- paste0(outputdir, "flag_classifier_", MODEL_ID, ".keras")
    OUT_MODEL_META <- paste0(outputdir, "flag_classifier_", MODEL_ID, "_meta.rds")
    kpy <- reticulate::import("keras", delay_load = TRUE)
    fit <- kpy$models$load_model(OUT_MODEL_KERAS)
    rnn_meta <- readRDS(OUT_MODEL_META)
  } else {
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
  } else if (MODEL_TYPE == "nn") {
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
  } else if (MODEL_TYPE == "rnn") {
    seq_score <- build_sequences(iamc_score, years = rnn_meta$years)
    x_score <- seq_score$x

    finite_idx <- apply(x_score, 1, function(r) all(is.finite(r)))
    if (sum(!finite_idx) > 0) {
      cat("RNN scoring: dropping", sum(!finite_idx), "rows with NA/NaN/Inf in sequences\n")
    }
    x_score <- x_score[finite_idx, , , drop = FALSE]

    np <- reticulate::import("numpy", delay_load = TRUE)
    tf <- reticulate::import("tensorflow", delay_load = TRUE)
    x_score_np <- np$array(x_score, dtype = "float32")
    ds_pred <- tf$data$Dataset$from_tensor_slices(x_score_np)$batch(as.integer(RNN_BATCH_SIZE))
    probs_cc <- fit$predict(ds_pred)
    colnames(probs_cc) <- rnn_meta$class_levels

    probs <- matrix(NA_real_, nrow = nrow(seq_score$keys), ncol = ncol(probs_cc))
    colnames(probs) <- colnames(probs_cc)
    probs[finite_idx, ] <- probs_cc

    probs_df <- as.data.frame(seq_score$keys)
    probs_df <- cbind(probs_df, probs)

    pred_df <- features_score_all %>%
      left_join(probs_df, by = c("run_id", "model", "scenario", "region", "variable", "unit")) %>%
      mutate(
        p_green = get_prob(as.matrix(select(., all_of(FLAG_LEVELS))), "green"),
        p_yellow = get_prob(as.matrix(select(., all_of(FLAG_LEVELS))), "yellow"),
        p_red = get_prob(as.matrix(select(., all_of(FLAG_LEVELS))), "red"),
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
    cat("(RNN model: feature importance not available)\n")
    quit(save = "no")
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
}
