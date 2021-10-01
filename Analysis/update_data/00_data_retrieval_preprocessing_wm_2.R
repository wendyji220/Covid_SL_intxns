##############################
# Load librairies
##############################
packages <- c("tigris", "sf", "caret", "rvest", "dplyr", 
             "tidyverse", "here", "tidyr", "readxl")

check_packages = function(p){
  if(!require(p, character.only = TRUE)){
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

lapply(packages, check_packages)

# Download USFacts data
usf <- data.frame(
  read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"),
  read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
)

# Parse FIPS as integers
usf$fips <- as.integer(usf[, 1])

# Remove counties in Alaska and Hawaii
usf <- usf[!((usf$State %in% c("AK", "HI")) | (usf$fips == 0)), ]

# Read airports data
airports <- read.csv(here("Analysis/update_data/data/processed/counties_airports.csv"))


# Read counties polygons
# When I tried to pushed the changes to git
# there was an error because tl_2019_us_county.shp
# was too large
# polygons=sf::st_read("data/shape/tl_2019_us_county.shp")
polygons <- counties(cb = F, year = 2019, class = "sf")
# Parse FIPS as integers
# polygons <- readRDS(here("Analysis/update_data/data/raw/polygons.RDS"))
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
  "FirstCaseDay"=NA,
  "CountyRelativeDay100Cases"=NA,
  "TotalCasesUpToDate"=0,
  "CountyRelativeDay100Deaths"=0,
  "TotalDeathsUpToDate"=0,
  "Deathsat1year" = 0, 
  "Casesat1year" = 0, 
  "CentroidLat"=centroids[,2],
  "CentroidLon"=centroids[,1],
  "NearestAirportName"=NA,
  "NearestAirportDistance"=NA,
  "NearestAirportEnplanements"=NA,
  "NearestAirportOver5000000Name"=NA,
  "NearestAirportOver5000000Distance"=NA,
  "NearestAirportOver5000000Enplanements"=NA,
  "Population"=NA,
  "PublicTransportation"=NA,
  "GDP"=NA,
  "AreaLand"=polygons$ALAND,
  "AreaWater"=polygons$AWATER
)

# Convert cases and deaths data into matrix
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

# Store a vector of indices of airports with enplanements of at least 5,000,000
f <- which(airports$CY.18.Enplanements >= 5000000, )
for (i in 1:nrow(counties)) {
  # Calculate county nearest airport
  dists <- gmt::geodist(counties$CentroidLat[i], counties$CentroidLon[i], airports$Latitude, airports$Longitude, units = "km")
  m <- which.min(dists)
  counties$NearestAirportName[i] <- as.character(airports$Name[m])
  counties$NearestAirportDistance[i] <- dists[m]
  counties$NearestAirportEnplanements[i] <- airports$CY.18.Enplanements[m]
  # Calculate county nearest airport with enplanements>5000000
  m <- which.min(dists[f])
  counties$NearestAirportOver5000000Name[i] <- as.character(airports$Name[f[m]])
  counties$NearestAirportOver5000000Distance[i] <- dists[f[m]]
  counties$NearestAirportOver5000000Enplanements[i] <- airports$CY.18.Enplanements[f[m]]
}

# Add population data
county_population <- read.csv(here("Analysis/update_data/data/processed/nir_covid_county_population_usafacts.csv"))
counties$Population <- county_population$population[match(counties$FIPS, as.integer(county_population$countyFIPS))]


# Add county GDP data
county_GDP <- read.csv(here("Analysis/update_data/data/raw/CountyGDP.csv"))
counties$GDP <- county_GDP$X2018[match(counties$FIPS, as.integer(county_GDP$GeoFips))]

# Add single value variables from census
for (name in c("air_quality", "all_heartdisease_deathrate", "all_stroke_deathrate", "num_hospitals", "percent_park_access", "urban_rural_status")) {
  a <- read.csv(here(paste("Analysis/update_data/data/raw/", name, ".csv", sep = "")))
  a$Value[a$Value == -1] <- NA
  counties[name] <- a$Value[match(counties$FIPS, a$cnty_fips)]
}

# Add analytic_data2020
analytic_data <- read.csv(here("Analysis/update_data/data/raw/analytic_data2020.csv"))
analytic_data <- analytic_data[2:nrow(analytic_data), ]
counties <- cbind(counties, analytic_data[match(counties$FIPS, as.integer(as.character(analytic_data$X5.digit.FIPS.Code))), 
                              c(8, 34, 39, 70, 75, 80, 85, 90, 95, 105, 130, 135, 140, 153, 173, 183, 188, 193, 218, 258, 263, 276, 282, 302, 307, 402, 407, 412, 417, 462, 503, 681, 686, 691, 701, 706)])

# Add County_Table_Chronic_Conditions_Prevalence_by_Age_2017.xlsx
age_group <- c("prev_2017_all_ages_", "prev_2017_under_65_", "prev_2017_over_65_")
for (i in 1:3) {
  a <- readxl::read_excel(here("Analysis/update_data/data/chronic_conditions_prev_by_age_2017.xlsx"), sheet = i)
  # a=a[1:nrow(a),]
  b <- a[, 4:24]
  colnames(b) <- paste0(age_group[i], colnames(b))
  c <- data.frame(a[, 1:3], b)
  counties <- cbind(counties, c[match(counties$FIPS, as.integer(c$State.County.FIPS.Code)), 4:ncol(c)])
}


# Add SVI2018_US_COUNTY.csv - this has the CDC vulnerability scores:
county_SVI <- read.csv(here("Analysis/update_data/data/raw/SVI2018_US_COUNTY.csv"))
county_SVI <- select(county_SVI, c(
  "FIPS",
  "EPL_AGE65",
  "EPL_AGE17",
  "EPL_DISABL",
  "EPL_SNGPNT",
  "EPL_MINRTY",
  "EPL_LIMENG",
  "EPL_MUNIT",
  "EPL_MOBILE",
  "EPL_CROWD",
  "EPL_NOVEH",
  "EPL_GROUPQ",
  "EPL_PCI"
))
counties <- cbind(counties, county_SVI[match(counties$FIPS, county_SVI$FIPS), ])


# Changed from tester2.csv to 2018ACS.csv
# 06/10/2020 changed 2018ACS to acs_2018_Jun.csv
acs <- read.csv(here("Analysis/update_data/data/raw/acs_2018_Jun.csv"))
counties <- cbind(counties, acs[match(counties$FIPS, acs$GEIOD), 3:ncol(acs)])

# Prepare states to match census state fips to NOAA state fips
states <- as.character(unique(usf$State))
states_fips <- purrr::map(states, function(state) usf$stateFIPS[which(usf$State == state)[1]])

## double FIPS 
counties <- counties[,-128]

###############################################################################################################
####################### CLIMATE CHANGE DATA ON TEMPERATURE CHANGES OVER YEARS #################################
###############################################################################################################

temp_lm_models_by_county <- read_excel(here("Analysis/update_data/data/raw/temp_lm_models_by_county.xlsx"))

counties <- merge(counties, 
                  temp_lm_models_by_county, by.x = "FIPS", by.y = "fips")


# Read commuting data
commuting <- readxl::read_excel("Analysis/update_data/data/raw/USCommuting2015.xlsx", skip = 6)
commuting <- commuting[1:139433, ]

fips_residence <- as.integer(commuting$`State FIPS Code...1`) * 1000 + as.integer(commuting$`County FIPS Code...2`)
fips_work <- as.integer(commuting$`State FIPS Code...5`) * 1000 + as.integer(commuting$`County FIPS Code...6`)

by_residence <- aggregate(commuting$`Workers in Commuting Flow`, list(fips = fips_residence), sum)
by_work <- aggregate(commuting$`Workers in Commuting Flow`, list(fips = fips_work), sum)

counties$agg_commuting_by_residence_place <- by_residence$x[match(counties$FIPS, by_residence$fips)]
counties$agg_commuting_by_work_place <- by_work$x[match(counties$FIPS, by_work$fips)]

counties$agg_commuting_by_residence_place <- counties$agg_commuting_by_residence_place / counties$Population
counties$agg_commuting_by_work_place <- counties$agg_commuting_by_work_place / counties$Population


## read in employment data
lbs_employment_types <- read_excel(here("Analysis/update_data/data/raw/lbs_employment_types.xlsx"),
  sheet = "US_St_Cn_MSA"
)

lbs_employment_x_county <- lbs_employment_types %>%
  filter(`Area Type` == "County") %>%
  select(c("Area\r\nCode", "Ownership", "Industry", "Establishment Count")) %>%
  mutate(fips = `Area\r\nCode`) %>%
  mutate(occu_counts = `Establishment Count`)

lbs_employment_x_county$Industry <- paste(
  lbs_employment_x_county$Ownership,
  lbs_employment_x_county$Industry
)

lbs_employment_x_county$Industry <- gsub("[[:digit:]]+", "", lbs_employment_x_county$Industry)

lbs_employment_x_county <- lbs_employment_x_county %>%
  select(-c("Ownership", "Area\r\nCode", "Establishment Count"))

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

## need to rewrite this later to make sure there isn't errors due to redundancy
counties_occ$occ_all_federal <- counties_occ$occ_all_federal / counties_occ$Population
counties_occ$occ_all_state <-  counties_occ$occ_all_state / counties_occ$Population
counties_occ$occ_all_local <- counties_occ$occ_all_local / counties_occ$Population 
counties_occ$occ_all_private <- counties_occ$occ_all_private / counties_occ$Population
counties_occ$occ_goods_prod <- counties_occ$occ_goods_prod / counties_occ$Population
counties_occ$occ_natural_mining <- counties_occ$occ_natural_mining / counties_occ$Population
counties_occ$occ_construction <- counties_occ$occ_construction / counties_occ$Population
counties_occ$occ_Manufacturing <- counties_occ$occ_Manufacturing / counties_occ$Population
counties_occ$occ_servic_prov <- counties_occ$occ_servic_prov / counties_occ$Population
counties_occ$occ_trade_trans_util <- counties_occ$occ_trade_trans_util / counties_occ$Population
counties_occ$occ_Info  <- counties_occ$occ_Info / counties_occ$Population
counties_occ$occ_financial <- counties_occ$occ_financial / counties_occ$Population
counties_occ$occ_prof_business <- counties_occ$occ_prof_business / counties_occ$Population
counties_occ$occ_educ_health <- counties_occ$occ_educ_health / counties_occ$Population
counties_occ$occ_leisure <- counties_occ$occ_leisure / counties_occ$Population
counties_occ$occ_other_services <- counties_occ$occ_other_services / counties_occ$Population

## check total industries agains't the sum

## getting additional data from county rankings that were not aggregated (i.e. obesity and segregration)

County_Health_Rankings_Data <- read_excel(here("Analysis/update_data/data/raw/2020 County Health Rankings Data.xlsx"),
  sheet = "Ranked Measure Data", skip = 1
)

County_Health_Rankings_Data_add_msrs <- County_Health_Rankings_Data %>% select(
  FIPS,
  soc_assc_rate = `Social Association Rate`
)

County_Health_Rankings_Data_add_data <- read_excel(here("Analysis/update_data/data/raw/2020 County Health Rankings Data.xlsx"),
  sheet = "Additional Measure Data", skip = 1
)

County_Health_Rankings_Data_add_data_msrs <- County_Health_Rankings_Data_add_data %>% 
  select(
  `FIPS`,
  seg_index = `Segregation index`,
  pct_mental_distress = `% Frequent Mental Distress`,
  pct_insufficient_sleep = `% Insufficient Sleep`
)

County_Health_Rankings_Data_targets <- merge(County_Health_Rankings_Data_add_msrs,
  County_Health_Rankings_Data_add_data_msrs,
  by = "FIPS"
)

counties_add_data <- merge(counties_occ, County_Health_Rankings_Data_targets, by = "FIPS")

## political party data

countypres_2000_2020 <- read_csv(here("Analysis/update_data/data/raw/countypres_2000-2020.csv"))

countypres2020 <- countypres_2000_2020 %>% filter(year == 2020)

countypres2020_rep <- countypres2020 %>%
  group_by(county_fips) %>%
  mutate(rep_ratio = candidatevotes / sum(candidatevotes)) %>%
  filter(party == "REPUBLICAN")

countypres2020_rep <- countypres2020_rep %>% select(county_fips, rep_ratio)

countypres2020_rep <- countypres2020_rep %>%
  group_by(county_fips) %>%
  dplyr::summarize(rep_ratio = mean(rep_ratio, na.rm=TRUE))

counties_add_data_political <- merge(counties_add_data, countypres2020_rep, by.x= "FIPS", by.y= "county_fips")

write.csv(
  counties_add_data_political,
  here("Analysis/update_data/data/processed/CountiesMergedData_Sept_14_2021_no_mobility.csv")
)


##################################################################################
####################### MOBILITY DATA PROCESSING #################################
##################################################################################

## integrate the google mobility data 

summary_report_US <- read.csv(here("Analysis/update_data/data/raw/summary_report_US.txt"))

summary_report_US$date <- as.Date(summary_report_US$date, "%Y-%m-%d")

summary_report_US <- summary_report_US %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y"))

mobility_data_long <- as.data.frame(summary_report_US)

mobility_data_long <- mobility_data_long %>% drop_na(retail.and.recreation)
mobility_data_long <- mobility_data_long %>% drop_na(grocery.and.pharmacy)
mobility_data_long <- mobility_data_long %>% drop_na(driving)
mobility_data_long <- mobility_data_long %>% drop_na(workplaces)
mobility_data_long <- mobility_data_long %>% drop_na(residential)

mobility_data_long <- subset(mobility_data_long, select = -c(parks, transit.stations, transit,walking))

colnames(mobility_data_long)[which(names(mobility_data_long) == "retail.and.recreation")] <- "mobility_retail_and_recreation"
colnames(mobility_data_long)[which(names(mobility_data_long) == "grocery.and.pharmacy")] <- "mobility_grocery_and_pharmacy"
colnames(mobility_data_long)[which(names(mobility_data_long) == "workplaces")] <- "mobility_workplaces"
colnames(mobility_data_long)[which(names(mobility_data_long) == "residential")] <- "mobility_residential"
colnames(mobility_data_long)[which(names(mobility_data_long) == "driving")] <- "mobility_driving"

mobility_data_long <- mobility_data_long %>%
  pivot_longer(
    cols = starts_with("mobility_"),
    names_to = "type",
    names_prefix = "mobility_",
    values_to = "value",
    values_drop_na = TRUE
  )

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

fips_state_crosswalk <- read_excel(here("Analysis/update_data/data/processed/fips_state_crosswalk.xlsx"))

counties_add_data_political_xwalk <- merge(counties_add_data_political, fips_state_crosswalk, on = "FIPS")

counties_add_data_political_xwalk_google_mob <- merge(counties_add_data_political_xwalk, google_mobility_coefs_wide, by.x = "State", by.y = "state", all.x)

write.csv(
  counties_add_data_political_xwalk_google_mob,
  here("Analysis/update_data/data/processed/CountiesMergedData_Sept_14_2021.csv")
)

