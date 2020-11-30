library(sl3)
library(origami)
library(dplyr)
library(knitr)
library(ggplot2)
library(here)
library(R6)
library(tidyverse)
library(readxl)
library(gam)
library(data.table)
library(gbm)
library(tidyr)
library(gbm)
library(xgboost)
library(ranger)
library(glmnet)

data_original <- read_csv(here("Analysis/update_data/data/processed/cleaned_covid_data_final.csv"))

ML_pipeline_results <- readRDS(here("Analysis/update_data/data/processed/ML_pipeline_5_outcomes_noscale_july16.RDS")) ### MAKE SURE TO CHANGE THIS AS UPDATED!

## create the per-capita outcomes
data_original$CountyRelativeDay25Cases_PopScale <- data_original$CountyRelativeDay25Cases / data_original$Population
data_original$TotalCasesUpToDate_PopScale <- data_original$TotalCasesUpToDate / data_original$Population
data_original$USRelativeDay100Deaths_PopScale <- data_original$USRelativeDay100Deaths / data_original$Population
data_original$TotalDeathsUpToDate_PopScale <- data_original$TotalDeathsUpToDate / data_original$Population



covars <- colnames(data_original)[-which(names(data_original) %in% c(
  all_outcomes,
  "X1",
  "FIPS",
  "FIPS.1",
  "county_names"
))]

target_outcomes <- c(
  "FirstCaseDay",
  "CountyRelativeDay25Cases_PopScale",
  "TotalCasesUpToDate_PopScale",
  "USRelativeDay100Deaths_PopScale",
  "TotalDeathsUpToDate_PopScale"
)

task <- make_sl3_Task(
  data = data_original,
  covariates = covars,
  outcome = target_outcomes[1],
  folds = origami::make_folds(data_original,
                              fold_fun = folds_vfold, V = 2
  )
)

## calculate residuals 

## day first case
sl <- ML_pipeline_results[[1]]$sl_obj
sl_day_first <- sl$train(task)
day_first_preds <- sl_day_first$predict()
ssr <- sum((data_original$FirstCaseDay - day_first_preds)^2)
sst <- sum((data_original$FirstCaseDay - mean(data_original$FirstCaseDay))^2)
1 - ssr/sst

## covid cases 25
task <- make_sl3_Task(
  data = data_original,
  covariates = covars,
  outcome = target_outcomes[2],
  folds = origami::make_folds(data_original,
                              fold_fun = folds_vfold, V = 2
  )
)


sl <- ML_pipeline_results[[2]]$sl_obj
cases_25 <- sl$train(task)
cases_25_preds <- cases_25$predict()
ssr <- sum((data_original$CountyRelativeDay25Cases_PopScale - cases_25_preds)^2)
sst <- sum((data_original$CountyRelativeDay25Cases_PopScale - mean(data_original$CountyRelativeDay25Cases_PopScale))^2)
1 - ssr/sst

## cases to-date
task <- make_sl3_Task(
  data = data_original,
  covariates = covars,
  outcome = target_outcomes[3],
  folds = origami::make_folds(data_original,
                              fold_fun = folds_vfold, V = 2
  )
)


sl <- ML_pipeline_results[[3]]$sl_obj
cases_total <- sl$train(task)
cases_total_preds <- cases_total$predict()
ssr <- sum((data_original$TotalCasesUpToDate_PopScale - cases_total_preds)^2)
sst <- sum((data_original$TotalCasesUpToDate_PopScale - mean(data_original$TotalCasesUpToDate_PopScale))^2)
1 - ssr/sst

## deaths day 100
task <- make_sl3_Task(
  data = data_original,
  covariates = covars,
  outcome = target_outcomes[4],
  folds = origami::make_folds(data_original,
                              fold_fun = folds_vfold, V = 2
  )
)


sl <- ML_pipeline_results[[4]]$sl_obj
deaths_100 <- sl$train(task)
deaths_100_preds <- deaths_100$predict()
ssr <- sum((data_original$USRelativeDay100Deaths_PopScale - deaths_100_preds)^2)
sst <- sum((data_original$USRelativeDay100Deaths_PopScale - mean(data_original$USRelativeDay100Deaths_PopScale))^2)
1 - ssr/sst

## total deaths
task <- make_sl3_Task(
  data = data_original,
  covariates = covars,
  outcome = target_outcomes[5],
  folds = origami::make_folds(data_original,
                              fold_fun = folds_vfold, V = 2
  )
)


sl <- ML_pipeline_results[[5]]$sl_obj
deaths_total <- sl$train(task)
deaths_total_preds <- deaths_total$predict()
ssr <- sum((data_original$TotalDeathsUpToDate_PopScale - deaths_total_preds)^2)
sst <- sum((data_original$TotalDeathsUpToDate_PopScale - mean(data_original$TotalDeathsUpToDate_PopScale))^2)
1 - ssr/sst


