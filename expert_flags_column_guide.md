# expert_flags.csv: Column Meanings (Input Guide)

This file records **elements reviewed by experts (or flagged as problematic)** in IAM (Integrated Assessment Model) scenarios
and is intended for use in the downstream machine-learning model (expert-in-the-loop).

As a rule, please enter values that **exactly match the key columns of the IAMC template**.

---

## Column descriptions

### `run_id`
- ID that uniquely identifies an IAM run
- Must **exactly match** the IAMC template `run_id`
- Examples: `2026.1.20`, `2026-01-20_v3`

---

### `model`
- Name of the integrated assessment model
- Must match the IAMC template `model` column
- Examples: `AIMHub`, `MESSAGEix`, `REMIND`

---

### `scenario`
- Scenario name
- Must **exactly match** the IAMC template `scenario` column
- Example: `SSP1 - Medium Emissions`

---

### `region`
- Region code or region name
- Must match the IAMC template `region` column
- Examples: `USA`, `JPN`, `World`

---

### `variable`
- Variable name being evaluated
- Must **exactly match** the IAMC template `variable` column
- Example: `Emi_CO2_Ene_and_Ind_Pro`

⚠ Please watch out for naming variations (case, spaces, symbols).

---

### `flag`
- Expert evaluation result (required)
- Enter one of the following:
  - `green`: no particular issues / realistic
  - `yellow`: caution / needs checking
  - `red`: unrealistic / reject or needs correction

⚠ Do not include unreviewed items in this file.

---

### `issue_type`
- Type of issue (selectable, optional)
- Recommended examples:
  - `implausible_transition_speed`
  - `implausible_cost_or_price`
  - `implausible_resource_or_land`
  - `discontinuity_or_jump`
  - `inconsistent_with_narrative`
  - `constraint_violation`
  - `model_convergence_or_solver_issue`
  - `data_pipeline_or_postprocess_bug`
  - `sign_or_unit_problem`
  - `other`

---

### `severity`
- Severity of the issue (optional)
- Numeric scale: 1 (minor) to 5 (critical)

---

### `reason_text`
- Free-text rationale (most important)
- A concise 1–2 sentence explanation is sufficient
- Example:
  > Emissions drop sharply in 2035–2040, making the technology rollout speed implausible.

---

### `reviewer`
- Reviewer identifier (can be anonymous)
- Examples: `rev01`, `SF`, `expert_A`

---

### `reviewed_at`
- Review date
- Format: `YYYY-MM-DD`

---

### `evidence_ref`
- Evidence or references (optional)
- Figure numbers, log IDs, note IDs, etc.
- Examples: `fig3`, `log_20260120_02`

---

### `notes_private`
- Private notes (optional)
- Supplementary information not for external release or publications

---

## Operational notes
- Record only reviewed items
- When unsure, use `yellow`
- Always fill in `reason_text`
- This file serves as **both a decision log and training data**
