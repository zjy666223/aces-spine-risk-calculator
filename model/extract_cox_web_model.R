#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(survival)
  library(jsonlite)
})

args_all <- commandArgs(FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
if (length(file_arg) > 0) {
  this_file <- normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  this_file <- normalizePath("model/extract_cox_web_model.R", mustWork = TRUE)
}

app_dir <- dirname(dirname(this_file))
project_dir <- dirname(app_dir)
input_csv <- file.path(
  project_dir,
  "04_通用可扩展代码",
  "00_共享上游_机器学习建模",
  "输出",
  "ml_modeling_dataset_raw.csv"
)
cox_qc_csv <- file.path(
  project_dir,
  "04_通用可扩展代码",
  "00_共享上游_机器学习建模",
  "输出",
  "cox_nomogram_qc.csv"
)
ml_metrics_csv <- file.path(
  project_dir,
  "04_通用可扩展代码",
  "00_共享上游_机器学习建模",
  "输出",
  "ml_test_metrics_raw.csv"
)
out_json <- file.path(app_dir, "model", "cox_risk_model.json")
out_js <- file.path(app_dir, "assets", "model-data.js")

dt <- fread(input_csv, showProgress = FALSE)
dt[, outcome := factor(outcome, levels = c("No", "Yes"))]

set.seed(42)
positive_ids <- dt[outcome == "Yes", sample_id]
negative_ids <- dt[outcome == "No", sample_id]
train_pos <- sample(positive_ids, size = floor(length(positive_ids) * 0.75))
train_neg <- sample(negative_ids, size = floor(length(negative_ids) * 0.75))
train_ids <- c(train_pos, train_neg)
dt[, split := fifelse(sample_id %in% train_ids, "train", "test")]
dt[, split := factor(split, levels = c("train", "test"))]

continuous_cols <- c("aces_score", "age", "bmi", "tdi")
for (col in continuous_cols) {
  train_median <- median(dt[split == "train"][[col]], na.rm = TRUE)
  if (!is.finite(train_median)) train_median <- 0
  dt[is.na(get(col)), (col) := train_median]
}

dt[, education_nom := fifelse(education == "College", "College",
  fifelse(education == "Other levels", "Other", "Missing")
)]
dt[, income_nom := fifelse(household_income %in% c("<18,000", "18,000-30,999"), "Low",
  fifelse(household_income %in% c("31,000-51,999", "52,000-100,000"), "Middle",
    fifelse(household_income == ">100,000", "High", "Missing")
  )
)]
dt[, physical_activity_nom := fifelse(physical_activity_cat %in% c("Low", "Moderate", "High"),
  physical_activity_cat, "Missing"
)]
dt[, smoking_nom := fifelse(smoking_status == "Previous", "Former",
  fifelse(smoking_status %in% c("Never", "Current"), smoking_status, "Missing")
)]
dt[, sex_nom := fifelse(sex %in% c("Female", "Male"), sex, "Missing")]

nom_data <- dt[, .(
  sample_id,
  split,
  followup_years = as.numeric(followup_days) / 365.25,
  event = as.integer(outcome_binary),
  aces_score = as.numeric(aces_score),
  age = as.numeric(age),
  sex = factor(sex_nom, levels = c("Female", "Male", "Missing")),
  bmi = as.numeric(bmi),
  education = factor(education_nom, levels = c("College", "Other", "Missing")),
  smoking = factor(smoking_nom, levels = c("Never", "Former", "Current", "Missing")),
  household_income = factor(income_nom, levels = c("Low", "Middle", "High", "Missing")),
  physical_activity = factor(physical_activity_nom, levels = c("Low", "Moderate", "High", "Missing")),
  tdi = as.numeric(tdi)
)]
nom_data <- nom_data[is.finite(followup_years) & followup_years >= 0]

train_data <- as.data.frame(nom_data[split == "train"])
test_data <- as.data.frame(nom_data[split == "test"])

fit <- survival::coxph(
  survival::Surv(followup_years, event) ~
    aces_score + age + sex + bmi + education + smoking +
      household_income + physical_activity + tdi,
  data = train_data,
  x = TRUE
)

base_haz <- as.data.table(survival::basehaz(fit, centered = FALSE))
hazard_at <- function(year) {
  available <- base_haz[time <= year]
  if (nrow(available) == 0) return(0)
  available[.N, hazard]
}
risk_times <- c(5, 10, 15)
baseline_cumhaz <- setNames(lapply(risk_times, hazard_at), as.character(risk_times))

coefs <- stats::coef(fit)
coefs <- coefs[is.finite(coefs)]

test_matrix <- stats::model.matrix(stats::delete.response(stats::terms(fit)), test_data)
test_matrix <- test_matrix[, names(coefs), drop = FALSE]
lp_test <- as.numeric(test_matrix %*% coefs)
risk_matrix <- sapply(risk_times, function(year) {
  1 - exp(-as.numeric(baseline_cumhaz[[as.character(year)]]) * exp(lp_test))
})
colnames(risk_matrix) <- paste0("risk_", risk_times, "y")
risk_quantiles <- lapply(seq_along(risk_times), function(i) {
  probs <- c(0.1, 0.25, 0.33, 0.5, 0.67, 0.75, 0.9, 0.95)
  values <- stats::quantile(risk_matrix[, i], probs = probs, na.rm = TRUE, names = FALSE)
  setNames(as.list(round(as.numeric(values), 6)), paste0("q", probs * 100))
})
names(risk_quantiles) <- as.character(risk_times)

model_matrix <- stats::model.matrix(fit)
coef_names <- names(coefs)
feature_means <- colMeans(model_matrix[, coef_names, drop = FALSE])

get_mode <- function(x) {
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[[1]]
}

reference_profile <- list(
  aces_score = 0,
  age = round(stats::median(train_data$age, na.rm = TRUE), 1),
  sex = "Female",
  bmi = round(stats::median(train_data$bmi, na.rm = TRUE), 1),
  education = "College",
  smoking = "Never",
  household_income = get_mode(train_data$household_income),
  physical_activity = get_mode(train_data$physical_activity),
  tdi = round(stats::median(train_data$tdi, na.rm = TRUE), 1)
)

cox_qc <- fread(cox_qc_csv, showProgress = FALSE)
ml_metrics <- fread(ml_metrics_csv, showProgress = FALSE)
best_ml <- ml_metrics[which.max(auc)]
xgb <- ml_metrics[model == "XGBoost"][1]

payload <- list(
  meta = list(
    title = "ACEs-informed spine degenerative disease risk calculator",
    modelType = "Cox proportional hazards model",
    outcome = "Incident spine degenerative disease",
    eventDefinition = "ICD-10 M48.0, M50.3, M51.3, M47, and M43.1",
    generatedAt = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    sourceDataset = basename(input_csv)
  ),
  coefficients = as.list(round(as.numeric(coefs), 12)),
  coefficientNames = names(coefs),
  baselineCumHaz = baseline_cumhaz,
  featureMeans = as.list(round(as.numeric(feature_means), 12)),
  featureMeanNames = names(feature_means),
  riskQuantiles = risk_quantiles,
  referenceProfile = reference_profile,
  levels = list(
    sex = levels(train_data$sex),
    education = levels(train_data$education),
    smoking = levels(train_data$smoking),
    household_income = levels(train_data$household_income),
    physical_activity = levels(train_data$physical_activity)
  ),
  inputRanges = list(
    aces_score = list(min = 0, max = 5, step = 1),
    age = list(
      min = floor(stats::quantile(train_data$age, 0.01, na.rm = TRUE)),
      max = ceiling(stats::quantile(train_data$age, 0.99, na.rm = TRUE)),
      step = 1
    ),
    bmi = list(
      min = floor(stats::quantile(train_data$bmi, 0.01, na.rm = TRUE)),
      max = ceiling(stats::quantile(train_data$bmi, 0.99, na.rm = TRUE)),
      step = 0.1
    ),
    tdi = list(
      min = floor(stats::quantile(train_data$tdi, 0.01, na.rm = TRUE)),
      max = ceiling(stats::quantile(train_data$tdi, 0.99, na.rm = TRUE)),
      step = 0.1
    )
  ),
  metrics = list(
    trainN = as.integer(cox_qc[item == "train_n", value]),
    trainEvents = as.integer(cox_qc[item == "train_events", value]),
    testN = as.integer(cox_qc[item == "test_n", value]),
    testEvents = as.integer(cox_qc[item == "test_events", value]),
    coxTrainCIndex = as.numeric(cox_qc[item == "train_c_index", value]),
    coxTestCIndex = as.numeric(cox_qc[item == "test_c_index", value]),
    bestMlModel = best_ml$model,
    bestMlAuc = round(best_ml$auc, 3),
    xgboostAuc = round(xgb$auc, 3),
    xgboostSensitivity = round(xgb$sensitivity, 3),
    xgboostSpecificity = round(xgb$specificity, 3)
  )
)

names(payload$coefficients) <- names(coefs)
names(payload$featureMeans) <- names(feature_means)

json_text <- jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, digits = 12)
dir.create(dirname(out_json), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_js), recursive = TRUE, showWarnings = FALSE)
writeLines(json_text, out_json, useBytes = TRUE)
writeLines(
  c(
    "window.COX_MODEL = ",
    json_text,
    ";"
  ),
  out_js,
  useBytes = TRUE
)

cat("Wrote ", out_json, "\n", sep = "")
cat("Wrote ", out_js, "\n", sep = "")
