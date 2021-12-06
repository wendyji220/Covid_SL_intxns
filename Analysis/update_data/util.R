check_packages = function(p){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

packages <- c("tigris", "sf", "caret", "rvest", "dplyr",  "origami", "knitr", "ggplot2", "R6", "gam", "gbm", "remotes",
              "tidyverse", "tidycensus", "here", "tidyr", "readxl", "panelr", "pROC", "imputeTS", "data.table", "xgboost", "zeallot")

lapply(packages, check_packages)

# install superLearner
remotes::install_github("tlverse/sl3")
library("sl3")

`%notin%` <- Negate(`%in%`)

RAW_DATA_PATH = function(file_name){
  return(paste(here('Analysis/update_data/data/raw/'), file_name, sep=''))
}

PROCESSED_DATA_PATH = function(file_name){
  return(paste(here('Analysis/update_data/data/processed/'), file_name, sep=''))
}
