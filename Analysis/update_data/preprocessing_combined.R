################ Load Libraries ##################

packages <- c("tigris", "sf", "caret", "rvest", "dplyr", 
              "tidyverse", "tidycensus", "here", "tidyr", "readxl", "panelr", "pROC", "imputeTS")

check_packages = function(p){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

lapply(packages, check_packages)


################ Define Global Variables ##################

`%notin%` <- Negate(`%in%`)

## thresholds
NA_THRESH <- 0.80
CORR_THRESH <- 0.99

## run census variable rename? 
CENSUS_DATA_RENAME <- FALSE

RAW_DATA_PATH = function(file_name){
  return(paste(here('Analysis/update_data/data/raw/'), file_name, sep=''))
}

PROCESSED_DATA_PATH = function(file_name){
  return(paste(here('Analysis/update_data/data/processed/'), file_name, sep=''))
}



################ US Facts ##################

usf <- data.frame(
  read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"),
  read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
)

# Parse FIPS as integers
usf$fips <- as.integer(usf[, 1])

# Remove counties in Alaska and Hawaii
usf <- usf[!((usf$State %in% c("AK", "HI")) | (usf$fips == 0)), ]


################ County Polygons ##################

polygons <- counties(cb = F, year = 2019, class = "sf")
polygons$fips <- as.integer(as.character(polygons$GEOID))
# Keep only counties with data from US Facts
polygons <- polygons[polygons$fips %in% usf$fips, ]
# Order polygons by FIPS
polygons <- polygons[order(polygons$fips), ]
# Calculate counties centroids
centroids <- sf::st_coordinates(sf::st_centroid(polygons))

# Initialize counties
counties <- data.frame(
  "FIPS"=polygons$fips,
  "Name"=polygons$NAME,
  "CentroidLat"=centroids[,2],
  "CentroidLon"=centroids[,1],
  "AreaLand"=polygons$ALAND,
  "AreaWater"=polygons$AWATER
)
FIPS <- counties$FIPS

ndays <- ncol(usf) / 2 - 5
usf <- usf[match(counties$FIPS, usf$fips), ]
mcases <- data.matrix(usf[, 6:(5 + ndays)])
mdeaths <- data.matrix(usf[, (ndays + 10):(2 * ndays + 9)])

# Calculate counties cases and deaths statistics
for (i in 1:nrow(counties)) {
  if (any(mcases[i,]>0)){
    counties$TotalCasesUpToDate[i] <- mcases[i,ndays]
    counties$TotalDeathsUpToDate[i] <- mdeaths[i,ndays]
    fc <- min(which(mcases[i,]>0))
    counties$FirstCaseDay[i]<-fc
    if (ndays-fc>=100) {counties$CountyRelativeDay100Deaths[i]=mdeaths[i,100]}
    if (ndays-fc>=100) {counties$CountyRelativeDay100Cases[i]=mcases[i,fc+24]}
    if (ndays-fc>=365) {counties$Deathsat1year[i]=mdeaths[i,365]}
    if (ndays-fc>=365) {counties$Casesat1year[i]=mcases[i,365]}
  }
}


################ Airport Data ##################

airports <- read.csv(PROCESSED_DATA_PATH("counties_airports.csv"))

# Store a vector of indices of airports with enplanements of at least 5,000,000
ind_larger_than_5m <- which(airports$CY.18.Enplanements >= 5000000, )
for (i in 1:nrow(counties)) {
  # Calculate county nearest airport
  dists <- gmt::geodist(counties$CentroidLat[i], counties$CentroidLon[i], airports$Latitude, airports$Longitude, units = "km")
  m <- which.min(dists)
  counties$NearestAirportName[i] <- as.character(airports$Name[m])
  counties$NearestAirportDistance[i] <- dists[m]
  counties$NearestAirportEnplanements[i] <- airports$CY.18.Enplanements[m]
  # Calculate county nearest airport with enplanements>5000000
  m <- which.min(dists[ind_larger_than_5m])
  counties$NearestAirportOver5000000Name[i] <- as.character(airports$Name[ind_larger_than_5m[m]])
  counties$NearestAirportOver5000000Distance[i] <- dists[ind_larger_than_5m[m]]
  counties$NearestAirportOver5000000Enplanements[i] <- airports$CY.18.Enplanements[ind_larger_than_5m[m]]
}


################ Population Data ##################

county_population <- read.csv(PROCESSED_DATA_PATH("nir_covid_county_population_usafacts.csv"))
counties$Population <- county_population$population[match(FIPS, as.integer(county_population$countyFIPS))]

################ County GDP Data ##################

county_GDP <- read.csv(RAW_DATA_PATH('CountyGDP.csv'))
counties$GDP <- county_GDP$X2018[match(FIPS, as.integer(county_GDP$GeoFips))]


################ Census Value ##################

census_files = c("air_quality", "all_heartdisease_deathrate", "all_stroke_deathrate", 
                   "num_hospitals", "percent_park_access", "urban_rural_status")

for (name in census_files) {
  dat <- read.csv(paste(RAW_DATA_PATH(name), ".csv", sep = ""))
  dat$Value[dat$Value == -1] <- NA
  counties[name] <- dat$Value[match(FIPS, dat$cnty_fips)]
}


################ Analytic Data ##################

analytic_data <- read.csv(RAW_DATA_PATH('analytic_data2020.csv'))
analytic_data <- analytic_data[2:nrow(analytic_data), ]
col_analytics <- names(analytic_data)

counties <- cbind(counties, 
                  analytic_data[match(FIPS, as.integer(as.character(analytic_data$X5.digit.FIPS.Code))), 
                                grepl("raw.value" , col_analytics) | 
                                  col_analytics %in% c("Ratio.of.population.to.primary.care.physicians.", "Percentage.of.households.with.overcrowding")])

################ Chronic Conditions ##################

age_group <- c("prev_2017_all_ages_", "prev_2017_under_65_", "prev_2017_over_65_")

for (i in 1:3) {
  chronic_dat <- read_excel(here("Analysis/update_data/data/chronic_conditions_prev_by_age_2017.xlsx"), sheet = i)
  selected_dat <- chronic_dat[, 4:ncol(chronic_dat)]
  colnames(selected_dat) <- paste0(age_group[i], colnames(selected_dat))
  chronic_fips <- as.integer(chronic_dat$`State/County FIPS Code`)
  counties <- cbind(counties, selected_dat[match(FIPS, chronic_fips), ])
}

################ County SVI ##################

county_SVI <- read.csv(RAW_DATA_PATH("SVI2018_US_COUNTY.csv"))
SVI_fips <- county_SVI$FIPS

# exclude locations
county_SVI <- county_SVI[8:dim(county_SVI)[2]]

# get all the variables except the one containing theme
counties <- cbind(counties, county_SVI[match(FIPS, SVI_fips), 
                                       !grepl("theme" , names(county_SVI))])


################ County ACS ##################

acs <- read.csv(RAW_DATA_PATH("acs_2018_Jun.csv"))
counties <- cbind(counties, acs[match(FIPS, acs$GEIOD), 3:ncol(acs)])

# Prepare states to match census state fips to NOAA state fips
states <- as.character(unique(usf$State))
states_fips <- purrr::map(states, function(state) usf$stateFIPS[which(usf$State == state)[1]])


################ Climate Change Data ##################

temp_lm_models_by_county <- read_excel(RAW_DATA_PATH("temp_lm_models_by_county.xlsx"))

counties <- merge(counties, 
                  temp_lm_models_by_county, by.x = "FIPS", by.y = "fips")


################ Commuting Data ##################

commuting <- read_excel(RAW_DATA_PATH("USCommuting2015.xlsx"), skip = 6)
commuting <- commuting[1:139433, ]
FIPS <- counties$FIPS

add_commute = function(commute_type, counties){
  if(commute_type=="residence") col_ind <- 1 else col_ind <- 5
  
  fips <- as.integer(commuting[[paste0("State FIPS Code...", col_ind)]]) * 1000 + 
    as.integer(commuting[[paste0("County FIPS Code...", (col_ind + 1))]])
  by_commute <- aggregate(commuting$`Workers in Commuting Flow`, list(fips = fips), sum)
  
  col_name <- paste0('agg_commuting_by_', commute_type, '_place')
  counties[col_name] <- by_commute$x[match(FIPS, by_commute$fips)]/counties$Population
  return(counties)
}

counties <- add_commute("residence", counties)
counties <- add_commute("work", counties)


################ Employment Data ##################

lbs_employment_types <- read_excel(RAW_DATA_PATH("lbs_employment_types.xlsx"),
                                   sheet = "US_St_Cn_MSA")


lbs_employment_x_county <- lbs_employment_types %>%
  filter(`Area Type` == "County") %>%
  select(fips = `Area\r\nCode`, Ownership, Industry, occu_counts = `Establishment Count`)

lbs_employment_x_county$Industry <- paste(
  lbs_employment_x_county$Ownership,
  lbs_employment_x_county$Industry
)

lbs_employment_x_county$Industry <- gsub("[[:digit:]]+", "", lbs_employment_x_county$Industry)
lbs_employment_x_county <- lbs_employment_x_county %>%
  select(-c("Ownership"))

lbs_employment_x_county_wide <- spread(
  lbs_employment_x_county,
  Industry,
  occu_counts
)

lbs_employment_x_county_wide$fips <- as.integer(lbs_employment_x_county_wide$fips)


lbs_employment_x_county_wide_rename <-
  lbs_employment_x_county_wide %>% 
  select(fips,
         occ_all_federal = `Federal Government  Total, all industries`,
         occ_all_state = `State Government  Total, all industries`,
         occ_all_local = `Local Government  Total, all industries`,
         occ_all_private = `Private  Total, all industries`,
         occ_goods_prod = `Private  Goods-producing`,
         occ_natural_mining = `Private  Natural resources and mining`,
         occ_construction = `Private  Construction`,
         occ_Manufacturing = `Private  Manufacturing`,
         occ_servic_prov = `Private  Service-providing`,
         occ_trade_trans_util = `Private  Trade, transportation, and utilities`,
         occ_Info = `Private  Information`,
         occ_financial = `Private  Financial activities`,
         occ_prof_business = `Private  Professional and business services`,
         occ_educ_health = `Private  Education and health services`,
         occ_leisure = `Private  Leisure and hospitality`,
         occ_other_services = `Private  Unclassified`
  )

counties_occ <- merge(counties, 
                      lbs_employment_x_county_wide_rename, by.x = "FIPS", by.y = "fips")

counties_occ <- counties_occ %>% mutate_at(vars(contains('occ_')), function(x) x /counties_occ$Population)


################ Health Ranking Data ##################

County_Health_Rankings_Data <- read_excel(RAW_DATA_PATH("2020 County Health Rankings Data.xlsx"),
                                          sheet = "Ranked Measure Data", skip = 1)

County_Health_Rankings_Data_add_msrs <- County_Health_Rankings_Data %>% 
  select(FIPS,
         soc_assc_rate = `Social Association Rate`)

County_Health_Rankings_Data_add_data <- read_excel(RAW_DATA_PATH("2020 County Health Rankings Data.xlsx"),
                                                   sheet = "Additional Measure Data", skip = 1)

County_Health_Rankings_Data_add_data_msrs <- County_Health_Rankings_Data_add_data %>% 
  select(`FIPS`,
         seg_index = `Segregation index`,
         pct_mental_distress = `% Frequent Mental Distress`,
         pct_insufficient_sleep = `% Insufficient Sleep`)

County_Health_Rankings_Data_targets <- merge(County_Health_Rankings_Data_add_msrs,
                                             County_Health_Rankings_Data_add_data_msrs,
                                             by = "FIPS")

counties_add_data <- merge(counties_occ, County_Health_Rankings_Data_targets, by = "FIPS")

################ Political Party Data ##################

countypres_2000_2020 <- read_csv(RAW_DATA_PATH("countypres_2000-2020.csv"))

countypres2020 <- countypres_2000_2020 %>% filter(year == 2020)

countypres2020_rep <- countypres2020 %>%
  group_by(county_fips) %>%
  mutate(rep_ratio = candidatevotes / sum(candidatevotes)) %>%
  filter(party == "REPUBLICAN") %>%
  select(county_fips, rep_ratio)

countypres2020_rep <- countypres2020_rep %>%
  group_by(county_fips) %>%
  summarize(rep_ratio = mean(rep_ratio, na.rm=TRUE))

counties_add_data_political <- merge(counties_add_data, countypres2020_rep, 
                                     by.x= "FIPS", by.y= "county_fips")


################ Census segregation estimation ##################
racial_seg_census <- read_csv(PROCESSED_DATA_PATH("racial_seg_census_api.csv")) %>% select(-c("...1"))

counties_add_data_political <- merge(counties_add_data_political, racial_seg_census, 
                                     by.x = "FIPS", by.y = "FIPS")

##################################################################################
####################### MOBILITY DATA PROCESSING #################################
##################################################################################
summary_report_US <- read.csv(RAW_DATA_PATH("summary_report_US.txt"))

summary_report_US$date <- as.Date(summary_report_US$date, "%Y-%m-%d")

summary_report_US <- summary_report_US %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y"))


mobility_data_long <- as.data.frame(summary_report_US)
mobility_data_long <- subset(mobility_data_long, 
                             select = -c(parks, transit.stations, transit,walking))

mobility_data_long <- mobility_data_long %>% drop_na(retail.and.recreation:driving)

names(mobility_data_long)[4:8] <- paste0("mobility_", 
                                         gsub("\\.","\\_",names(mobility_data_long[4:8])))

mobility_data_long <- mobility_data_long %>%
  pivot_longer(cols = starts_with("mobility_"),
               names_to = "type",
               names_prefix = "mobility_",
               values_to = "value",
               values_drop_na = TRUE)

google_mobility_coefs <- mobility_data_long %>% 
  group_by(state, year, month, type) %>% 
  group_modify(~broom::tidy(lm(value ~ date, ., na.action=na.exclude))) %>%
  filter(term == "date") %>% 
  select(state, month, estimate)

google_mobility_coefs <- spread(google_mobility_coefs, type, estimate)

google_mobility_coefs$date <- as.factor(paste(google_mobility_coefs$month, google_mobility_coefs$year, sep = ""))

google_mobility_coefs <- subset(google_mobility_coefs, select = -c(month, year))
google_mobility_coefs_panel <- panel_data(google_mobility_coefs, id = state, wave = date)

google_mobility_coefs_wide <- widen_panel(google_mobility_coefs_panel, separator = "_")

google_mobility_coefs_wide$state <- state.abb[match(google_mobility_coefs_wide$state,state.name)]

fips_state_crosswalk <- read_excel(PROCESSED_DATA_PATH("fips_state_crosswalk.xlsx"))

counties_add_data_political_xwalk <- merge(counties_add_data_political, fips_state_crosswalk, on = "FIPS")

counties_add_data_political_xwalk_google_mob <- merge(counties_add_data_political_xwalk, 
                                                      google_mobility_coefs_wide, by.x = "State", by.y = "state", all.x)


write.csv(
  counties_add_data_political_xwalk_google_mob,
  PROCESSED_DATA_PATH("CountiesMergedData_Oct_22_2021.csv")
)


################ Read from 00 ##################

# Changed to CountiesMergedData_July_10.csv
covid_data_unprocessed <- read_csv(PROCESSED_DATA_PATH("CountiesMergedData_Oct_22_2021.csv"))
# covid_data_unprocessed <- counties_add_data_political_xwalk_google_mob
covid_data_unprocessed <- covid_data_unprocessed[,-1]

## remove character values that aren't needed
covid_data_unprocessed <- covid_data_unprocessed %>% 
  select(-c(Name, CTYNAME, NearestAirportName, NearestAirportOver5000000Name))

char_columns <- sapply(covid_data_unprocessed, is.character)
covid_data_unprocessed[char_columns] <- lapply(covid_data_unprocessed[char_columns], as.factor)


## these variables still need standardization by population: 
std_variables <- c("Premature.death.raw.value", "HIV.prevalence.raw.value", "Sexually.transmitted.infections.raw.value", "Preventable.hospital.stays.raw.value")

covid_data_unprocessed[std_variables] = covid_data_unprocessed[std_variables]/covid_data_unprocessed$Population


################ Air pollution Data ##################
LUR.air.pollution.data <- read.csv(RAW_DATA_PATH('LUR_pollution_data.csv'))

LUR.air.pollution.data <- LUR.air.pollution.data %>% mutate_at(c("year", "pollutant"), as.factor) 

means.cross.year <- LUR.air.pollution.data %>%
  group_by(fips, pollutant) %>%
  summarize(mean_size = mean(pred_wght, na.rm = TRUE))

LUR.air.pull.wide <- means.cross.year %>% 
  pivot_wider(names_from = pollutant, values_from = mean_size)


################ Mask Use Variable ##################
mask.use.data <- read.csv(RAW_DATA_PATH('mask-use-by-county.csv'))


################ Lead Exposure Variable ##################
lead_risk_score <- read_csv(RAW_DATA_PATH("lead-risk-score.csv"))
lead_risk_score <- lead_risk_score[,-1]

lead_risk_score <- lead_risk_score %>%
  mutate(fips = substr(id, 1, 5))

lead_risk_score <- lead_risk_score %>% 
  group_by(fips) %>%
  summarise(across(-(c(name, id)), mean, na.rm = TRUE))


################ Water Contaminant Exposure Variable ##################

summarized_contaminants_raw <- read_csv(RAW_DATA_PATH("summarized_contaminants.csv"))

summarized_contaminants_ratio <- summarized_contaminants_raw %>%
  subset(select = -c(fips,`number of points`)) / summarized_contaminants_raw$`number of points`

summarized_contaminants_ratio$fips <- summarized_contaminants_raw$fips


################ PESTICIDE EXPOSURE VARIABLES ##################

pesticide_data <- read.csv(RAW_DATA_PATH('EPest_county_estimates_2013_2017_v2.txt'), sep = "\t")

pesticide_data <- pesticide_data %>%
  rowwise() %>% mutate(kg_avg=mean(c(EPEST_LOW_KG, EPEST_HIGH_KG), na.rm=T)) 

pesticide_data$fips <- paste(pesticide_data$STATE_FIPS_CODE, str_pad(pesticide_data$COUNTY_FIPS_CODE, 3, pad = "0"), sep = "")

pesticide_avgs_by_year <- pesticide_data %>%
  group_by(fips, COMPOUND) %>%
  summarize(mean_kg = mean(kg_avg, na.rm = TRUE))


pesticide_avgs_by_year <- pesticide_avgs_by_year %>% 
  pivot_wider(names_from = COMPOUND, values_from = mean_kg)

pesticide_avgs_by_year <- pesticide_avgs_by_year[, which(colMeans(!is.na(pesticide_avgs_by_year)) > NA_THRESH)]

pesticide_avgs_by_year <- na_interpolation(pesticide_avgs_by_year, option = "spline")


################ CHEMICAL EXPOSURE VARIABLES ##################
arsenic_violations <- read.csv(RAW_DATA_PATH('SDWIS_As_Violations_County_2006-2017_FINAL.csv'), sep = ",")
arsenic_violations <- arsenic_violations %>% select(FIPS, Freq)
colnames(arsenic_violations)[2] <- "water_arsenic_violation_freq"

prepare_data_for_chemicals = function(file_name){
  chemical_data <- read.csv(RAW_DATA_PATH(file_name), sep = ",", stringsAsFactors = FALSE)
  chemical_data$Value <- as.numeric(as.character(chemical_data$Value))
  
  avg_name <- paste("avg", gsub(".csv", "", file_name), sep="_")
  chemical_data <- chemical_data %>% 
    group_by(countyFIPS) %>%
    summarize(!!avg_name := mean(Value, na.rm=TRUE))
  
  return(chemical_data)
}

file_names <- list.files(path = RAW_DATA_PATH(""), pattern = "\\levels.csv$")
chemical_dataFrames <- lapply(file_names, prepare_data_for_chemicals)

covid_data_unprocessed <- covid_data_unprocessed %>% rename(countyFIPS = FIPS)

## water quality data
for (df in chemical_dataFrames){
  covid_data_unprocessed <- merge(df, covid_data_unprocessed, by.x = "countyFIPS", by.y = "countyFIPS", all.y = TRUE)
}

avg_names <- paste("avg", gsub(".csv", "", file_names), sep="_")
covid_data_unprocessed[avg_names] <- na_interpolation(covid_data_unprocessed[avg_names], option = "spline")



################ INCARCERATION VARIABLES BY ETHNICITY ##################

incarceration_trends <- read_excel(RAW_DATA_PATH('incarceration_trends.xlsx'))
incarceration_trends <- incarceration_trends %>%
  filter(year == 2018)

incarceration_trends <- incarceration_trends[, which(colMeans(!is.na(incarceration_trends)) > NA_THRESH)]

in_colnames <- names(incarceration_trends)
end_matching_age = function(x) endsWith(x, '15to64')

end_matching_rate <- function(x) endsWith(x, '_rate')


incarceration_trends <- subset(incarceration_trends, 
                               select = in_colnames %in% c("fips", "county_name", "total_pop") 
                               | end_matching_age(in_colnames) 
                               | end_matching_rate(in_colnames))


ethnity_group <- c("black", "aapi", "latinx", "native")

incarceration_trends <- incarceration_trends %>%
  mutate(across(contains(paste(ethnity_group, "jail_pop_rate", sep="_")), 
                 .fns = list(white = function(x) x/incarceration_trends$white_jail_pop_rate),
                 .names = "{fn}_{col}" ))

other_group <- c("white", "total", "female", "male")

incarceration_trends <- incarceration_trends %>%
  mutate(across(contains(paste(other_group, "pop_15to64", sep="_")), 
                .fns = list(ratio = function(x) x/incarceration_trends$total_pop),
                .names = "{col}_{fn}" ))

incarceration_trends <- incarceration_trends %>%
  mutate(across(contains(paste(ethnity_group, "pop_15to64", sep="_")), 
                .fns = list(ratio = function(x) (x/incarceration_trends$total_pop)/incarceration_trends$white_pop_15to64_ratio),
                .names = "{col}_{fn}" ))


incarceration_trends <- subset(incarceration_trends, 
                               select = !end_matching_age(names(incarceration_trends))
                                          & !names(incarceration_trends) %in% c("total_pop", "total_jail_pop_rate"))



################ Merge Data ##################

merge_dfs <- c("LUR.air.pull.wide", "mask.use.data", "lead_risk_score", "summarized_contaminants_ratio", 
                   "pesticide_avgs_by_year", "arsenic_violations")

fips_columns <- c("fips", "COUNTYFP", "fips", "fips", "fips", "FIPS")

for (i in 1:length(merge_dfs)){
  df <- get(merge_dfs[i])
  covid_data_unprocessed <- merge(covid_data_unprocessed, df, by.x = "countyFIPS", by.y = fips_columns[i])
}


covid_data_unprocessed <- merge(incarceration_trends, covid_data_unprocessed, by.x = "fips", by.y = "countyFIPS", all.y = TRUE)


################ STRUCTURAL RACISM VARIABLES ##################

structural_racism <- read_excel(RAW_DATA_PATH("structural_racism_all_forsharing2021.xls"))
structural_racism[ structural_racism == "NA" ] <- NA
structural_racism <- structural_racism[, which(colMeans(!is.na(structural_racism)) > NA_THRESH)]


covid_data_unprocessed <- merge(structural_racism, covid_data_unprocessed, by.x = "GEOID2", by.y = "fips", all.y = TRUE)

colnames(covid_data_unprocessed)[which(colnames(covid_data_unprocessed) == "GEOID2")] <- "fips"
covid_data_unprocessed <- subset(covid_data_unprocessed, select=-c(state_code, name))


################ CLEANING ##################

# get data dictionary 
Data_Dictionary <- read_excel(PROCESSED_DATA_PATH("Data_Dictionary.xlsx"))

vars_2_keep <- Data_Dictionary %>% 
  filter(Keep == "Yes")

covid_data_unprocessed  <- covid_data_unprocessed %>% 
  select(vars_2_keep$`Variable Name`)

## remove columns with NA greater than threshold
covid_data_processed <- covid_data_unprocessed[, which(colMeans(!is.na(covid_data_unprocessed)) > NA_THRESH)]

vars_rmv_na <- colnames(covid_data_unprocessed)[names(covid_data_unprocessed) %notin% names(covid_data_processed)]
write.csv(vars_rmv_na, PROCESSED_DATA_PATH("vars_removed_na_thresh_combined.csv"))

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

## what variables are near non-varying
if (length(nz_idx_vector) > 0) {
  covid_data_processed <- covid_data_processed[,-nz_idx_vector]
}


## check structures of data 
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

## remove outcomes
covid_data_processed_features <- covid_data_processed %>% 
  select(-outcomes)

## impute the mean for NA values in the numeric dataset (we already filtered for NA columns greater than 75%)
covid_data_processed_features_numeric_imputed<- na_mean(covid_data_processed_features, option = "mean")
covid_data_processed_features_numeric_imputed$fips <- as.numeric(covid_data_processed_features_numeric_imputed$fips)

## identifying and removing highly correlated features
descrCor <- cor(covid_data_processed_features_numeric_imputed)
descrCor[upper.tri(descrCor)] <- 0
diag(descrCor) <- 0

names(covid_data_processed_features_numeric_imputed[,apply(descrCor, 2, function(x) any(x > CORR_THRESH))])
covid_data_processed_features_numeric_imputed_corr_rem <- covid_data_processed_features_numeric_imputed[,!apply(descrCor,2,function(x) any(x > CORR_THRESH))]

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
write.csv(final_covid_processed, file = PROCESSED_DATA_PATH("cleaned_covid_data_final_Oct_22_21.csv"))




