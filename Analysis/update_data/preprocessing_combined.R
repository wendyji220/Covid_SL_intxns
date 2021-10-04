##############################
# Load librairies
##############################
packages <- c("tigris", "sf", "caret", "rvest", "dplyr", 
              "tidyverse", "here", "tidyr", "readxl", "panelr", "pROC", "imputeTS")

check_packages = function(p){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

lapply(packages, check_packages)

##############################
# Define global variables
##############################

`%notin%` <- Negate(`%in%`)

## thresholds
NA_THRESH <- 0.80
CORR_THRESH <- 0.99

## run census variable rename? 
CENSUS_DATA_RENAME <- FALSE


##############################
# US Facts
##############################

usf <- data.frame(
  read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"),
  read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
)

# Parse FIPS as integers
usf$fips <- as.integer(usf[, 1])

# Remove counties in Alaska and Hawaii
usf <- usf[!((usf$State %in% c("AK", "HI")) | (usf$fips == 0)), ]


##############################
#  County polygon
##############################

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


##############################
# Airports data
##############################
airports <- read.csv(here("Analysis/update_data/data/processed/counties_airports.csv"))

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

##############################
# Population data
##############################
county_population <- read.csv(here("Analysis/update_data/data/processed/nir_covid_county_population_usafacts.csv"))
counties$Population <- county_population$population[match(FIPS, as.integer(county_population$countyFIPS))]

##############################
# County GDP data
##############################
county_GDP <- read.csv(here("Analysis/update_data/data/raw/CountyGDP.csv"))
counties$GDP <- county_GDP$X2018[match(FIPS, as.integer(county_GDP$GeoFips))]


##############################
# Census value
##############################

census_files = c("air_quality", "all_heartdisease_deathrate", "all_stroke_deathrate", 
                   "num_hospitals", "percent_park_access", "urban_rural_status")
for (name in census_files) {
  dat <- read.csv(here(paste("Analysis/update_data/data/raw/", name, ".csv", sep = "")))
  dat$Value[a$Value == -1] <- NA
  counties[name] <- dat$Value[match(FIPS, dat$cnty_fips)]
}

##############################
# Analytic data
##############################
analytic_data <- read.csv(here("Analysis/update_data/data/raw/analytic_data2020.csv"))
analytic_data <- analytic_data[2:nrow(analytic_data), ]
counties <- cbind(counties, 
                  analytic_data[match(FIPS, as.integer(as.character(analytic_data$X5.digit.FIPS.Code))), 
                                          c(8, 34, 39, 70, 75, 80, 85, 90, 95, 105, 130, 135, 140, 153, 173, 183, 188, 193, 218, 258, 263, 276, 282, 302, 
                                            307, 402, 407, 412, 417, 462, 503, 681, 686, 691, 701, 706)])

##############################
# Chronic conditions
##############################

age_group <- c("prev_2017_all_ages_", "prev_2017_under_65_", "prev_2017_over_65_")
for (i in 1:3) {
  chronic_dat <- readxl::read_excel(here("Analysis/update_data/data/chronic_conditions_prev_by_age_2017.xlsx"), sheet = i)
  selected_dat <- chronic_dat[, 4:ncol(chronic_dat)]
  colnames(selected_dat) <- paste0(age_group[i], colnames(selected_dat))
  chronic_fips <- as.integer(chronic_dat$`State/County FIPS Code`)
  counties <- cbind(counties, selected_dat[match(FIPS, chronic_fips), ])
}

##############################
# County SVI 
##############################
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
counties <- cbind(counties, county_SVI[match(FIPS, county_SVI$FIPS), ])

##############################
# County ACS
##############################

acs <- read.csv(here("Analysis/update_data/data/raw/acs_2018_Jun.csv"))
counties <- cbind(counties, acs[match(FIPS, acs$GEIOD), 3:ncol(acs)])

# Prepare states to match census state fips to NOAA state fips
states <- as.character(unique(usf$State))
states_fips <- purrr::map(states, function(state) usf$stateFIPS[which(usf$State == state)[1]])

##############################
# Climate change data
##############################

temp_lm_models_by_county <- read_excel(here("Analysis/update_data/data/raw/temp_lm_models_by_county.xlsx"))

counties <- merge(counties, 
                  temp_lm_models_by_county, by.x = "FIPS", by.y = "fips")


##############################
# Commuting data
##############################

commuting <- readxl::read_excel(here("Analysis/update_data/data/raw/USCommuting2015.xlsx"), skip = 6)
commuting <- commuting[1:139433, ]


add_commute = function(commute_type){
  if(commute_type=="residence") col_ind <- 1 else col_ind <- 5
  
  fips <- as.integer(commuting[[paste0("State FIPS Code...", col_ind)]]) * 1000 + 
    as.integer(commuting[[paste0("County FIPS Code...", (col_ind + 1))]])
  by_commute <- aggregate(commuting$`Workers in Commuting Flow`, list(fips = fips), sum)
  
  col_name <- paste0('agg_commuting_by_', commute_type, '_place')
  counties[col_name] <- by_commute$x[match(FIPS, by_commute$fips)]/counties$population
}

add_commute("residence")
add_commute("work")






