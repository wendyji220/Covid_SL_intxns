library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(imputeTS)

## read in data
LUR.air.pollution.data <- read.csv(here('Analysis/update_data/data/raw/LUR_pollution_data.csv'))
LUR.air.pollution.data$year <- as.factor(LUR.air.pollution.data$year)
LUR.air.pollution.data$pollutant <- as.factor(LUR.air.pollution.data$pollutant)

means.cross.year <- LUR.air.pollution.data %>%
  group_by(fips,pollutant) %>%
  summarize(mean_size = mean(pred_wght, na.rm = TRUE))

LUR.air.pull.wide <- means.cross.year %>% 
  pivot_wider(names_from = pollutant, values_from = mean_size)

mask.use.data <- read.csv(here('Analysis/update_data/data/raw/mask-use-by-county.csv'))

pesticide_data <- read.csv(here('Analysis/update_data/data/raw/EPest_county_estimates_2013_2017_v2.txt'), sep = "\t")


pesticide_data <- pesticide_data %>%
  rowwise() %>% mutate(kg_avg=mean(c(EPEST_LOW_KG, EPEST_HIGH_KG), na.rm=T)) 

pesticide_data$fips <-paste(pesticide_data$STATE_FIPS_CODE, str_pad(pesticide_data$COUNTY_FIPS_CODE, 3, pad = "0"), sep = "")

pesticide_avgs_by_year <- pesticide_data %>%
  group_by(fips, COMPOUND) %>%
  summarize(mean_kg = mean(kg_avg, na.rm = TRUE))

pesticide_avgs_by_year <- pesticide_avgs_by_year %>% 
  pivot_wider(names_from = COMPOUND, values_from = mean_kg)

pesticide_avgs_by_year <- pesticide_avgs_by_year[, which(colMeans(!is.na(pesticide_avgs_by_year)) > 0.8)]

pesticide_avgs_by_year <- na.interpolation(pesticide_avgs_by_year, option = "spline")


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








