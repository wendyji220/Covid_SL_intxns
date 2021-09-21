Sys.setenv(CENSUS_KEY='fde701a056f01569bf75106ba4c3d5cb56469ad2')
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

library(sf)
library(sp)
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_class = "sf")
library(tmap)
library(rmapshaper)
library(imputeTS)
library(here)

census_api_key('fde701a056f01569bf75106ba4c3d5cb56469ad2')

# Bring in census tract data using the Census API 

my_states <- state.abb

us.tracts <- get_acs(geography = "tract", 
                     year = 2019,
                     variables = c(tpop = "B01003_001", 
                                   tpopr = "B03002_001", 
                                   nhwhite = "B03002_003", 
                                   nhblk = "B03002_004",
                                   nhasn = "B03002_006", 
                                   hisp = "B03002_012"),
                     state = my_states,
                     survey = "acs5",
                     geometry = TRUE)

us.county <- get_acs(geography = "county", 
                     year = 2019,
                     variables = c(tpop = "B01003_001", 
                                   tpopr = "B03002_001", 
                                   nhwhite = "B03002_003", 
                                   nhblk = "B03002_004",
                                   nhasn = "B03002_006", 
                                   hisp = "B03002_012"),
                     state = my_states,
                     survey = "acs5",
                     geometry = TRUE)

fips_name <- subset(us.county, select = c(GEOID, NAME))

# Make the data tidy, calculate and keep essential vars. Also take out zero population tracts
us.tracts <- us.tracts %>% 
  select(-(moe)) %>%
  spread(key = variable, value = estimate) %>%
  mutate(pnhwhite = nhwhite/tpopr, pnhasn = nhasn/tpopr, 
         pnhblk = nhblk/tpopr, phisp = hisp/tpopr, oth = tpopr - (nhwhite+nhblk+nhasn+hisp), 
         poth = oth/tpopr, nonwhite = tpopr-nhwhite, pnonwhite = nonwhite/tpopr) %>%
  select(c(GEOID,tpop, tpopr, pnhwhite, pnhasn, pnhblk, phisp,
           nhwhite, nhasn, nhblk, hisp, nonwhite, pnonwhite, oth, poth))  %>%
  filter(tpop != 0)

# Bring in metro area boundary
cb <- core_based_statistical_areas(cb = TRUE)

# Keep six largest metros in CA

#Keep tracts in large metros. Drop unnecessary variables
large.tracts <- st_join(us.tracts, cb, join = st_within, left=FALSE) %>%
  select(-(c(ALAND, AWATER, AFFGEOID)))

x <- large.tracts
y <- fips_name

z <- st_join(x, y, join = st_within, left=FALSE)

fips_joined <- z %>% distinct(GEOID.x, .keep_all = TRUE)

fips_joined <- fips_joined %>%
  group_by(GEOID) %>%
  mutate(nhwhitec = sum(nhwhite), nonwhitec = sum(nonwhite), 
         nhasnc = sum(nhasn), nhblkc = sum(nhblk), othc = sum(oth),
         hispc = sum(hisp), tpoprc = sum(tpopr)) %>%
  ungroup()


dissimilarity_indices <- fips_joined %>%
  group_by(GEOID) %>%
  mutate(d.wb = abs(nhblk/nhblkc-nhwhite/nhwhitec),
         d.wa = abs(nhasn/nhasnc-nhwhite/nhwhitec), 
         d.wh = abs(hisp/hispc-nhwhite/nhwhitec),
         d.wnw = abs(nonwhite/nonwhitec-nhwhite/nhwhitec)) %>%
  summarize(BWD = 0.5*sum(d.wb, na.rm=TRUE), AWD = 0.5*sum(d.wa, na.rm=TRUE),
            HWD = 0.5*sum(d.wh, na.rm=TRUE), NWWD = 0.5*sum(d.wnw, na.rm=TRUE))


interaction_index <- fips_joined %>%
  group_by(GEOID) %>%
  mutate(i.wb = (nhblk/nhblkc)*(nhwhite/tpopr),
         i.wa = (nhasn/nhasnc)*(nhwhite/tpopr), 
         i.wh = (hisp/hispc)*(nhwhite/tpopr),
         i.wnw = (nonwhite/nonwhitec)*(nhwhite/tpopr)) %>%
  summarize(BWI = sum(i.wb, na.rm=TRUE), AWI = sum(i.wa, na.rm=TRUE),
            HWI = sum(i.wh, na.rm=TRUE), NWWI = sum(i.wnw, na.rm=TRUE))

fips_joined <- fips_joined %>%
  mutate(e1 =  pnhwhite*log(1/pnhwhite), e2 = pnhasn*log(1/pnhasn),
         e3 = pnhblk*log(1/pnhblk), e4 = phisp*log(1/phisp),
         e5 = poth*log(1/poth), 
         e1 = replace(e1, is.nan(e1), 0), e2 = replace(e2, is.nan(e2), 0),
         e3 = replace(e3, is.nan(e3), 0), e4 = replace(e4, is.nan(e4), 0),
         e5 = replace(e5, is.nan(e5), 0),
         ent = e1 + e2 + e3 + e4 +e5) %>%
  select(-c(e1:e5))

fips_joined <- fips_joined %>%
  group_by(GEOID) %>%
  mutate(pnhwhitec = nhwhitec/tpoprc, pnhasnc = nhasnc/tpoprc, 
         pnhblkc = nhblkc/tpoprc, pothc = othc/tpoprc, phispc = hispc/tpoprc,
         e1 =  pnhwhitec*log(1/pnhwhitec), e2 = pnhasnc*log(1/pnhasnc),
         e3 = pnhblkc*log(1/pnhblkc), e4 = phispc*log(1/phispc),
         e5 = pothc*log(1/pothc), 
         e1 = replace(e1, is.nan(e1), 0), e2 = replace(e2, is.nan(e2), 0),
         e3 = replace(e3, is.nan(e3), 0), e4 = replace(e4, is.nan(e4), 0),
         e5 = replace(e5, is.nan(e5), 0),
         entc = e1 + e2 + e3 + e4 +e5) %>%
  select(-c(e1:e5)) %>%
  ungroup()

entropy_index <- fips_joined %>%
  group_by(GEOID) %>%
  summarize(H = sum((tpopr*(entc-ent)/(entc*tpoprc)), na.rm = TRUE))

racial_seg_data <- cbind(dissimilarity_indices, interaction_index, entropy_index)
racial_seg_data <- as.data.frame(racial_seg_data)

racial_seg_data <- subset(racial_seg_data, select = c("GEOID","BWD","AWD",
                                                      "HWD","NWWD","BWI",
                                                      "AWI","HWI","NWWI","H"))

## load data
covid_data_processed <- read_csv(here("Analysis/update_data/data/processed/CountiesMergedData_Sept_14_2021_no_mobility.csv"))
colnames(covid_data_processed)[which(colnames(covid_data_processed) == "countyFIPS")] <- "FIPS"

covid_data_processed <- covid_data_processed %>% select(-c("BWD","AWD",
                                   "HWD","NWWD","BWI",
                                   "AWI","HWI","NWWI","H"))


covid_race_seg_merge <- merge(covid_data_processed, racial_seg_data, by.x = "FIPS", by.y = "GEOID")


### do not impute
# covid_race_seg_merge_red <- covid_race_seg_merge[complete.cases(covid_race_seg_merge),]

# covid_race_seg_merge$BWD <- na_mean(covid_race_seg_merge$BWD)
# covid_race_seg_merge$AWD <- na_mean(covid_race_seg_merge$AWD)
# covid_race_seg_merge$HWD <- na_mean(covid_race_seg_merge$HWD)
# covid_race_seg_merge$NWWD <- na_mean(covid_race_seg_merge$NWWD)
# covid_race_seg_merge$BWI <- na_mean(covid_race_seg_merge$BWI)
# covid_race_seg_merge$AWI <- na_mean(covid_race_seg_merge$AWI)
# covid_race_seg_merge$HWI <- na_mean(covid_race_seg_merge$HWI)
# covid_race_seg_merge$NWWI <- na_mean(covid_race_seg_merge$NWWI)
# covid_race_seg_merge$H <- na_mean(covid_race_seg_merge$H)

write_csv(covid_race_seg_merge, here("Analysis/update_data/data/processed/covid_data_racial_scores_included.csv"))
# write_csv(covid_race_seg_merge_red, here("Analysis/update_data/data/processed/cleaned_covid_data_final_red.csv"))




