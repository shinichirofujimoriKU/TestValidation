# Validation

R-based workflow for training and scoring flag classifiers on IAMC-style time-series data.

## Contents
- `prog/`: R scripts (training, scoring, end-to-end pipeline).
- `data/`: Input datasets (training/scoring tables, model files, etc.).
- `output/`: Generated models and prediction outputs.
- `shell/`: Execution scripts and job templates.

## Requirements
- R (4.x recommended)
- R packages:
  - `dplyr`, `tidyr`, `readr`, `readxl`, `stringr`, `purrr`, `data.table`
  - `nnet`, `ranger`
  - `keras`, `reticulate`, `tensorflow` (for RNN)

## Quick start
1. Prepare input files under `data/`.
2. Run the main pipeline:
   - `prog/tran_and_score_flags2.R`

## Key scripts
- `prog/tran_and_score_flags2.R`: End-to-end training and scoring (RF/NN/RNN).
- `prog/train_flags.R`: Train-only pipeline (shared logic in `prog/common_flags.R`).
- `prog/score_flags.R`: Score-only pipeline (shared logic in `prog/common_flags.R`).

## Outputs
- Models: `output/flag_classifier_*.rds` or `.keras`
- Training tables: `output/training_table_*.csv`
- Predictions: `output/predicted_flags_*.csv`

## Notes
- RNN mode relies on a Python TensorFlow environment configured via `reticulate`.
- Some scripts accept runtime args like `MODEL_TYPE`, `RUN_TRAIN`, `RUN_SCORE`, `N_CORES`.
