library(pROC)
library(caret)
library(rvest)
library(dplyr)
library(tidyverse)
library(here)
library(imputeTS)
library(readxl)

`%notin%` <- Negate(`%in%`)

## thresholds
na_thresh <- 0.80
corr_thres <- 0.99
## run census variable rename? 
census_data_rename <- FALSE


# Changed to CountiesMergedData_July_10.csv
covid_data_unprocessed <- read_csv(here("Analysis/update_data/data/processed/covid_data_racial_scores_included.csv"))
covid_data_unprocessed <- covid_data_unprocessed[,-2]
## remove character values that aren't needed
covid_data_unprocessed <- covid_data_unprocessed %>% 
  select(-c(Name, CTYNAME, NearestAirportName, NearestAirportOver5000000Name))

covid_data_unprocessed[sapply(covid_data_unprocessed, is.character)] <- lapply(covid_data_unprocessed[sapply(covid_data_unprocessed, is.character)], as.factor)

# ## convert to numeric


## these variables still need standardization by population: 
covid_data_unprocessed$Premature.death.raw.value <- covid_data_unprocessed$Premature.death.raw.value / covid_data_unprocessed$Population 
covid_data_unprocessed$HIV.prevalence.raw.value <- covid_data_unprocessed$HIV.prevalence.raw.value / covid_data_unprocessed$Population 
covid_data_unprocessed$Sexually.transmitted.infections.raw.value <- covid_data_unprocessed$Sexually.transmitted.infections.raw.value / covid_data_unprocessed$Population 
covid_data_unprocessed$Preventable.hospital.stays.raw.value <- covid_data_unprocessed$Preventable.hospital.stays.raw.value / covid_data_unprocessed$Population 


##############################################################################################
################################# AIR POLLUION VARIABLES #####################################
##############################################################################################

## read in data
LUR.air.pollution.data <- read.csv(here('Analysis/update_data/data/raw/LUR_pollution_data.csv'))
LUR.air.pollution.data$year <- as.factor(LUR.air.pollution.data$year)
LUR.air.pollution.data$pollutant <- as.factor(LUR.air.pollution.data$pollutant)

means.cross.year <- LUR.air.pollution.data %>%
  group_by(fips,pollutant) %>%
  summarize(mean_size = mean(pred_wght, na.rm = TRUE))

LUR.air.pull.wide <- means.cross.year %>% 
  pivot_wider(names_from = pollutant, values_from = mean_size)

##############################################################################################
################################# MASK USE VARIABLES #########################################
##############################################################################################


mask.use.data <- read.csv(here('Analysis/update_data/data/raw/mask-use-by-county.csv'))

##############################################################################################
####################### LEAD EXPOSURE VARIABLES #########################################
##############################################################################################

lead_risk_score <- read_csv("Analysis/update_data/data/raw/lead-risk-score.csv")
lead_risk_score <- lead_risk_score[,-1]

lead_risk_score <- lead_risk_score %>%
  mutate(fips = substr(id, 1, 5))


lead_risk_score <- lead_risk_score %>% 
  group_by(fips) %>%
  summarise(across(-(c(name, id)), mean, na.rm = TRUE))

##############################################################################################
############### WATER CONTAMINANT EXPOSURE VARIABLES #########################################
##############################################################################################

summarized_contaminants_raw <- read_csv(here("Analysis/update_data/data/raw/summarized_contaminants.csv"))
summarized_contaminants_ratio <- summarized_contaminants_raw[,-1] / summarized_contaminants_raw[[62]]
summarized_contaminants_ratio <- summarized_contaminants_ratio[-61]
summarized_contaminants_ratio$fips <- summarized_contaminants_raw$fips


##############################################################################################
####################### PESTICIDE EXPOSURE VARIABLES #########################################
##############################################################################################

pesticide_data <- read.csv(here('Analysis/update_data/data/raw/EPest_county_estimates_2013_2017_v2.txt'), sep = "\t")

pesticide_data <- pesticide_data %>%
  rowwise() %>% mutate(kg_avg=mean(c(EPEST_LOW_KG, EPEST_HIGH_KG), na.rm=T)) 

pesticide_data$fips <-paste(pesticide_data$STATE_FIPS_CODE, str_pad(pesticide_data$COUNTY_FIPS_CODE, 3, pad = "0"), sep = "")

pesticide_avgs_by_year <- pesticide_data %>%
  group_by(fips, COMPOUND) %>%
  summarize(mean_kg = mean(kg_avg, na.rm = TRUE))


pesticide_avgs_by_year <- pesticide_avgs_by_year %>% 
  pivot_wider(names_from = COMPOUND, values_from = mean_kg)

pesticide_avgs_by_year <- pesticide_avgs_by_year[, which(colMeans(!is.na(pesticide_avgs_by_year)) > na_thresh)]

pesticide_avgs_by_year <- na.interpolation(pesticide_avgs_by_year, option = "spline")

##############################################################################################
####################### CHEMICAL EXPOSURE VARIABLES ##########################################
##############################################################################################

arsenic_violations <- read.csv(here('Analysis/update_data/data/raw/SDWIS_As_Violations_County_2006-2017_FINAL.csv'), sep = ",")
arsenic_violations <- arsenic_violations %>% select(FIPS, Freq)
colnames(arsenic_violations)[2] <- "water_arsenic_violation_freq"

arsenic_lvls <- read.csv(here('Analysis/update_data/data/raw/arsenic_levels.csv'), sep = ",", stringsAsFactors=FALSE)
arsenic_lvls$Value <- as.numeric(as.character(arsenic_lvls$Value))

arsenic_lvls <- arsenic_lvls %>%
  group_by(countyFIPS) %>%
  summarize(avg_arsenic_lvls = mean(Value, na.rm = TRUE))

nitrate_lvls <- read.csv(here('Analysis/update_data/data/raw/nitrate_levels.csv'), sep = ",", stringsAsFactors=FALSE)
nitrate_lvls$Value <- as.numeric(as.character(nitrate_lvls$Value))

nitrate_lvls <- nitrate_lvls %>%
  group_by(countyFIPS) %>%
  summarize(avg_nitrate_lvls = mean(Value, na.rm = TRUE))


dehp_lvls <- read.csv(here('Analysis/update_data/data/raw/dehp_levels.csv'), sep = ",", stringsAsFactors=FALSE)
dehp_lvls$Value <- as.numeric(as.character(dehp_lvls$Value))

dehp_lvls <- dehp_lvls %>%
  group_by(countyFIPS) %>%
  summarize(avg_dehp_lvls = mean(Value, na.rm = TRUE))

tce_lvls <- read.csv(here('Analysis/update_data/data/raw/TCE_lvls.csv'), sep = ",", stringsAsFactors=FALSE)
tce_lvls$Value <- as.numeric(as.character(tce_lvls$Value))

tce_lvls <- tce_lvls %>%
  group_by(countyFIPS) %>%
  summarize(avg_tce_lvls = mean(Value, na.rm = TRUE))

atrazine_lvls <- read.csv(here('Analysis/update_data/data/raw/atrazine_levels.csv'), sep = ",", stringsAsFactors=FALSE)
atrazine_lvls$Value <- as.numeric(as.character(atrazine_lvls$Value))

atrazine_lvls <- atrazine_lvls %>%
  group_by(countyFIPS) %>%
  summarize(avg_atrazine_lvls = mean(Value, na.rm = TRUE))

PCE_lvls <- read.csv(here('Analysis/update_data/data/raw/PCE_levels.csv'), sep = ",", stringsAsFactors=FALSE)
PCE_lvls$Value <- as.numeric(as.character(PCE_lvls$Value))

PCE_lvls <- PCE_lvls %>%
  group_by(countyFIPS) %>%
  summarize(avg_pce_lvls = mean(Value, na.rm = TRUE))

tthm_lvls <- read.csv(here('Analysis/update_data/data/raw/tthm_levels.csv'), sep = ",", stringsAsFactors=FALSE)
tthm_lvls$Value <- as.numeric(as.character(tthm_lvls$Value))

tthm_lvls <- tthm_lvls %>%
  group_by(countyFIPS) %>%
  summarize(avg_tthm_lvls = mean(Value, na.rm = TRUE))

##############################################################################################
####################### INCARCERATION VARIABLES BY ETHNICITY #################################
##############################################################################################

incarceration_trends <- read_excel(here("Analysis/update_data/data/raw/incarceration_trends.xlsx"))
incarceration_trends <- incarceration_trends %>%
  filter(year == 2018)

incarceration_trends <- incarceration_trends[, which(colMeans(!is.na(incarceration_trends)) > na_thresh)]

## remove variables that are not as relevant to the question of interest: 

incarceration_trends <- subset(incarceration_trends, select = c(fips, 
                                                                county_name,
                                                                total_pop, 
                                                                total_pop_15to64, 
                                                                female_pop_15to64, 
                                                                male_pop_15to64,
                                                                aapi_pop_15to64, 
                                                                black_pop_15to64, 
                                                                latinx_pop_15to64, 
                                                                native_pop_15to64, 
                                                                white_pop_15to64, 
                                                                total_jail_pop_rate, 
                                                                female_jail_pop_rate, 
                                                                male_jail_pop_rate, 
                                                                aapi_jail_pop_rate, 
                                                                black_jail_pop_rate,
                                                                latinx_jail_pop_rate,
                                                                native_jail_pop_rate,
                                                                white_jail_pop_rate,
                                                                total_jail_adm_rate, 
                                                                total_jail_pretrial_rate))

incarceration_trends$black_white_jail_pop_rate <- incarceration_trends$black_jail_pop_rate / incarceration_trends$white_jail_pop_rate
incarceration_trends$aapi_white_jail_pop_rate <- incarceration_trends$aapi_jail_pop_rate / incarceration_trends$white_jail_pop_rate
incarceration_trends$latinx_white_jail_pop_rate <- incarceration_trends$latinx_jail_pop_rate / incarceration_trends$white_jail_pop_rate
incarceration_trends$native_white_jail_pop_rate <- incarceration_trends$native_jail_pop_rate / incarceration_trends$white_jail_pop_rate

incarceration_trends$white_pop_15to64_ratio <- incarceration_trends$white_pop_15to64 / incarceration_trends$total_pop

incarceration_trends$total_pop_15to64_ratio <- incarceration_trends$total_pop_15to64 / incarceration_trends$total_pop
incarceration_trends$female_pop_15to64_ratio <- incarceration_trends$female_pop_15to64 / incarceration_trends$total_pop
incarceration_trends$male_pop_15to64_ratio <- incarceration_trends$male_pop_15to64 / incarceration_trends$total_pop

incarceration_trends$aapi_pop_15to64_ratio <- (incarceration_trends$aapi_pop_15to64 / incarceration_trends$total_pop) / incarceration_trends$white_pop_15to64_ratio
incarceration_trends$black_pop_15to64_ratio <- (incarceration_trends$black_pop_15to64 / incarceration_trends$total_pop) / incarceration_trends$white_pop_15to64_ratio
incarceration_trends$latinx_pop_15to64_ratio <- (incarceration_trends$latinx_pop_15to64 / incarceration_trends$total_pop) / incarceration_trends$white_pop_15to64_ratio
incarceration_trends$native_pop_15to64_ratio <- (incarceration_trends$native_pop_15to64 / incarceration_trends$total_pop) / incarceration_trends$white_pop_15to64_ratio

incarceration_trends <- subset(incarceration_trends, select = -c(total_pop, 
                                                                 total_pop_15to64,
                                                                 female_pop_15to64, 
                                                                 male_pop_15to64, 
                                                                 aapi_pop_15to64,
                                                                 black_pop_15to64,
                                                                 latinx_pop_15to64,
                                                                 native_pop_15to64,
                                                                 white_pop_15to64,
                                                                 total_jail_pop_rate))


covid_data_unprocessed <- merge(LUR.air.pull.wide, covid_data_unprocessed, by.x = "fips", by.y = "FIPS")
covid_data_unprocessed <- merge(mask.use.data, covid_data_unprocessed, by.x = "COUNTYFP", by.y = "fips")
covid_data_unprocessed <- merge(lead_risk_score, covid_data_unprocessed, by.x = "fips", by.y = "COUNTYFP")
covid_data_unprocessed <- merge(summarized_contaminants_ratio, covid_data_unprocessed, by.x = "fips", by.y = "fips")
covid_data_unprocessed <- merge(pesticide_avgs_by_year, covid_data_unprocessed, by.x = "fips", by.y = "fips")
covid_data_unprocessed <- merge(arsenic_violations, covid_data_unprocessed, by.x = "FIPS", by.y = "fips")

## water quality data
covid_data_unprocessed <- merge(arsenic_lvls, covid_data_unprocessed, by.x = "countyFIPS", by.y = "FIPS", all.y = TRUE)
covid_data_unprocessed <- merge(nitrate_lvls, covid_data_unprocessed, by.x = "countyFIPS", by.y = "countyFIPS", all.y = TRUE)
covid_data_unprocessed <- merge(dehp_lvls, covid_data_unprocessed, by.x = "countyFIPS", by.y = "countyFIPS", all.y = TRUE)
covid_data_unprocessed <- merge(tce_lvls, covid_data_unprocessed, by.x = "countyFIPS", by.y = "countyFIPS", all.y = TRUE)
covid_data_unprocessed <- merge(atrazine_lvls, covid_data_unprocessed, by.x = "countyFIPS", by.y = "countyFIPS", all.y = TRUE)
covid_data_unprocessed <- merge(PCE_lvls, covid_data_unprocessed, by.x = "countyFIPS", by.y = "countyFIPS", all.y = TRUE)
covid_data_unprocessed <- merge(tthm_lvls, covid_data_unprocessed, by.x = "countyFIPS", by.y = "countyFIPS", all.y = TRUE)

covid_data_unprocessed <- merge(incarceration_trends, covid_data_unprocessed, by.x = "fips", by.y = "countyFIPS", all.y = TRUE)


covid_data_unprocessed$avg_arsenic_lvls <- na.interpolation(covid_data_unprocessed$avg_arsenic_lvls, option = "spline")
covid_data_unprocessed$avg_nitrate_lvls <- na.interpolation(covid_data_unprocessed$avg_nitrate_lvls, option = "spline")
covid_data_unprocessed$avg_dehp_lvls <- na.interpolation(covid_data_unprocessed$avg_dehp_lvls, option = "spline")
covid_data_unprocessed$avg_tce_lvls <- na.interpolation(covid_data_unprocessed$avg_tce_lvls, option = "spline")
covid_data_unprocessed$avg_atrazine_lvls <- na.interpolation(covid_data_unprocessed$avg_atrazine_lvls, option = "spline")
covid_data_unprocessed$avg_pce_lvls <- na.interpolation(covid_data_unprocessed$avg_pce_lvls, option = "spline")
covid_data_unprocessed$avg_tthm_lvls <- na.interpolation(covid_data_unprocessed$avg_tthm_lvls, option = "spline")

##############################################################################################
####################### STRUCTURAL RACISM VARIABLES ##########################################
##############################################################################################

structural_racism <- read_excel(here("Analysis/update_data/data/raw/structural_racism_all_forsharing2021.xls"))
structural_racism[ structural_racism == "NA" ] <- NA
structural_racism <- structural_racism[, which(colMeans(!is.na(structural_racism)) > na_thresh)]


covid_data_unprocessed <- merge(structural_racism, covid_data_unprocessed, by.x = "GEOID2", by.y = "fips", all.y = TRUE)

colnames(covid_data_unprocessed)[which(colnames(covid_data_unprocessed) == "GEOID2")] <- "fips"
covid_data_unprocessed <- subset(covid_data_unprocessed, select=-c(state_code, name))

##############################################################################################
########################################## CLEANING ##########################################
##############################################################################################


# get data dictionary 
Data_Dictionary <- read_excel(here("Analysis/update_data/data/processed/Data_Dictionary.xlsx"))

vars_2_keep <- Data_Dictionary %>% 
  filter(Keep == "Yes") %>% select(`Variable Name`)

covid_data_unprocessed <-covid_data_unprocessed %>% 
  select(vars_2_keep$`Variable Name`)

## remove columns with NA greater than threshold
covid_data_processed <- covid_data_unprocessed[, which(colMeans(!is.na(covid_data_unprocessed)) > na_thresh)]

vars_rmv_na <- colnames(covid_data_unprocessed)[names(covid_data_unprocessed) %notin% names(covid_data_processed)]
write.csv(vars_rmv_na, here("Analysis/update_data/data/processed/vars_removed_na_thresh.csv"))

## removing near zero variance variables
nz_idx_vector <- nearZeroVar(
  covid_data_processed,
  freqCut = 95/5,
  uniqueCut = 10,
  saveMetrics = FALSE,
  names = FALSE,
  foreach = FALSE,
  allowParallel = TRUE
)

## what variables are near nonvarying
if (length(nz_idx_vector) > 0) {
  covid_data_processed <- covid_data_processed[,-nz_idx_vector]
}


## check structures of data 

str(covid_data_processed)
char_vars <- names(covid_data_processed[, sapply(covid_data_processed, class) == 'character'])

covid_data_processed <- data.frame(lapply(covid_data_processed,
                                            function(x) as.numeric(as.character(x))))


outcomes <- c("CountyRelativeDay100Cases", 
              "TotalCasesUpToDate", 
              "CountyRelativeDay100Deaths", 
              "TotalDeathsUpToDate", 
              "FirstCaseDay",
              "Deathsat1year",
              "Casesat1year")

outcome_data <- covid_data_processed[,c(outcomes, "fips")]

## check for na in outcome data
list_na <- colnames(outcome_data)[ apply(outcome_data, 2, anyNA) ]

number_na <- colSums(is.na(outcome_data))
number_na

## only a few, we will remove these at the last step

## remove outcomes
covid_data_processed_features <- covid_data_processed %>% 
  select(-outcomes)


## impute the mean for NA values in the numeric dataset (we already filtered for NA columns greater than 75%)
covid_data_processed_features_numeric_imputed<- na_mean(covid_data_processed_features, option = "mean")
covid_data_processed_features_numeric_imputed$fips <- as.numeric(covid_data_processed_features_numeric_imputed$fips)
## identifying and removing highly correlated features
descrCor <-  cor(covid_data_processed_features_numeric_imputed)
descrCor[upper.tri(descrCor)] <- 0
diag(descrCor) <- 0

names(covid_data_processed_features_numeric_imputed[,apply(descrCor,2,function(x) any(x > corr_thres))])
covid_data_processed_features_numeric_imputed_corr_rem <- covid_data_processed_features_numeric_imputed[,!apply(descrCor,2,function(x) any(x > corr_thres))]

covid_data_processed_features_numeric_imputed_corr_rem$occ_total_all_industries <- covid_data_processed_features_numeric_imputed$occ_total_all_industries

final_covid_processed <- covid_data_processed_features_numeric_imputed_corr_rem


## add the outcome back in and let's just remove the rows with NAs for outcome

## quick function to remove rows with any NA from selected columns
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

final_covid_processed <- merge(outcome_data, final_covid_processed, by = "fips")
final_covid_processed <- completeFun(final_covid_processed, outcomes)

## Column bind the outcome data and write the final dataset
write.csv(final_covid_processed, file = here("Analysis/update_data/data/processed/cleaned_covid_data_final_sept_24_21.csv"))






  