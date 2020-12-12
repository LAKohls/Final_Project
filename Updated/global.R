library(reactable)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)
library(lubridate)
library(hablar)
library(tigris)
library(sp)
library(rgeos)
library(rgdal)
library(dplyr)
library(maps)
library(likert)
library(tidygeocoder)
library(stringr)
library(ggrepel)
library(tidycensus)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(stringr)
library(dplyr)
library(maps)
library(ggrepel)
library(knitr)

# Set Census API Key : This is needed to pull ACS
key <- "30f8a6b33c7a3c3ddc67053c0d75dfc692f6081a" 
col_pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#28E2E5")
# Read in Data 
data<- read.csv("Data.csv")
today<- Sys.Date()
data$DOB<- mdy(data$DOB)
data$age<- (year(today)- year(data$DOB))
data$gross_income<- as.numeric(data$gross_income)


# Tidy Data and Create useful categorical variables 
data <- data %>%
  arrange(age) %>%
  group_by(PropertyName) %>%
  mutate(median_age = median(age, na.rm= TRUE)) %>%
  ungroup() %>%
  arrange(age) %>%
  mutate(total_medianage = median(age, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(age2 = case_when(
    between(age, 0,4) ~ "Pre-School (0-4)",
    between(age, 4,20)~ "Elementary (5-10)",
    between(age, 11,13)~"Middle School (11-13)",
    between(age, 14,18) ~ "High School (14-18)",
    between(age, 19,24) ~ "Young Adults (19-24)",
    between(age, 25,35)~ "Early Career (25-35)",
    between(age, 36,54) ~ "Middle Age (36-54)",
    between(age, 54, 200) ~ "Senior(55+)"))

data$health_ins_type <- "Not_Collected"

data<- data %>%
  mutate(health_ins_type = case_when(
    ins_CHIP == "yes" ~ "CHIP",
    ins_medicaid == "yes" ~ "Medicaid",
    ins_medicare == "yes" ~ "Medicare",
    ins_private == "yes" ~ "Private",
    ins_VA == "yes" ~ "VA"))%>%
  mutate(safe_building = factor(safe_building, levels = c("Always", "Most", "Sometimes", "Never", "Refused", "Not_collected"))) %>%
  mutate(safe_neighborhood = factor(safe_building, levels = c("Always", "Most", "Sometimes", "Never", "Refused", "Not_collected")))

data<- data %>%
  mutate(race = case_when(
    race_amer_in == "yes" ~ "Am_Indian",
    race_asian == "yes" ~ "Asian",
    race_black == "yes" ~ "Black",
    race_white == "yes" ~ "White", 
    race_haw_pi == "yes" ~ "Haw_Pi",
    race_other == "yes" ~ "Other"))


data$race[data$race == NA]<- "Not_Collected"

data$age2<-  factor(data$age2, levels =c("Pre-School (0-4)",
                                         "Elementary (5-10)",
                                         "Middle School (11-13)",
                                         "High School (14-18)",
                                         "Young Adults (19-24)",
                                         "Early Career (25-35)",
                                         "Middle Age (36-54)",
                                         "Senior(55+)", "Not Collected"))
median_age<- median(data$age, na.rm= TRUE)
median_income<- median(data$gross_income, na.rm= TRUE)

unique_properties<- data %>%
  select(PropertyName) %>%
  arrange(PropertyName)


unique_properties<- unique(unique_properties$PropertyName)


# Select picker data for graphing
graph_picker_data <- data %>%    
  select(-race_collected, -ins_CHIP, -ins_medicaid, -ins_medicare, -ins_other, -ins_private, -ins_VA, -PropertyID, -HouseholdID, -ResidentID, -DOB, -move_out_date, -move_in_date, -race_amer_in, -race_asian, -race_black, -race_white, -race_other, -move_out_reason, -race_haw_pi)

# Select Pickers for colors
graph_picker_color<- graph_picker_data %>%
  select(
    "Age"= "age2", 
    "Race" = "race", 
    "Health_Ins_Type" = "health_ins_type", 
    "Employed" = "employed", 
    "Safe_in_Building" = "safe_building", 
    "Safe_in_Neighborhood" = "safe_neighborhood", 
    "Registered_to_Vote" = "voter_reg", 
    "Has_Usual_Medial_Provider" = "usual_provider", 
    "Routine_Checkups" = "routine_checkup", 
    "Visited ER in Last Year" = "er_visits", 
    "Has Experienced Food Insecurity" = "food_insec", 
    "Experiences Physical Barriers" = "physical_bar", 
    "Experiences Mental Barriers" = "mental_bar", 
    "Has Insurance" = "insurance_yes_no",
    "Has Checking Account" = "check_acct_res",
    "Has Savings Account" = "save_acct_res" )


# Geocode addresses and attach census Data 

addresses<- read.csv("addresses.csv")

census_full1 <- addresses %>% geocode(Addresses, 
                                      method = 'census', full_results = TRUE, return_type = 'geographies', return_addresses = FALSE)

# Pad FIPS codes to match Census Codes
census_full1$state_fips<- str_pad(census_full1$state_fips, 2, side = "left", pad = "0")
census_full1$county_fips<- str_pad(census_full1$county_fips, 3, side = "left", pad = "0")

census_full1$census_tract<- str_pad(census_full1$census_tract, 6, side = "left", pad = "0")
census_full1$census_block<-str_pad(census_full1$census_block, 4, side = "left", pad = "0")



# Create file with GEOIDs to match ACS 
census_full1<- census_full1 %>%
  mutate(GEOID = paste0(state_fips, county_fips, census_tract))

# Select Variables from Census to pull 
all_vars <- c(
  total_pop = "B01003_001",
  median_income = "B19013_001",
  median_rent = "B25064_001",
  poverty = "B06012_002",
  poverty_1.5x ="B06012_003",
  poverty_2x ="B06012_004",
  edu_no_highschool ="B06009_002",
  edu_highschool ="B06009_003",
  edu_bachelors ="B06009_005",
  edu_graduate ="B06009_006",
  employed ="B23025_004",
  unemployed ="B23025_005",
  total_labor_force ="B23025_002",
  african_american="B02009_001",
  european_american="C02003_003",
  asian_american="C02003_006",
  native_american="C02003_005",
  pacific_islander="C02003_007",
  hispanic_latino="B03002_012",
  total_male ="B01001_002",
  total_female="B01001_026",
  age_median="B01002_001",
  earnings_median="B24081_001",
  health_carecoverage= "C27021_001")


# Gather data from 
multi_state_tract <- get_acs(
  geography = "tract",
  variables = all_vars,
  state = census_full1$state_fips,
  year = 2018,
  survey = "acs5",
  geometry = FALSE,
  key = key) %>%
  filter(GEOID %in% census_full1$GEOID)

multi_state_tract<- multi_state_tract %>%
  select(-moe)

multi_state_tract<- unique(multi_state_tract)

attach(multi_state_tract)
test<- pivot_wider(multi_state_tract, names_from= "variable", values_from= "estimate")

test2<- merge(census_full1, test,  by = "GEOID")


# Get Percentages instead of Counts from ACS DATA 
test2<- test2 %>%
  mutate(Median_Income= median_income,
         Median_Rent = median_rent, 
         Male = (total_male/total_pop)*100,
         Female= (total_female/ total_pop)*100,
         Hispanic = (hispanic_latino/total_pop)*100,
         Black = (african_american/total_pop)*100, 
         Asian = (asian_american/total_pop)*100,
         White = (european_american/total_pop)*100,
         Pacific_Islander = (pacific_islander/total_pop)*100,
         Native_American = (native_american/total_pop)*100,
         Employed = (employed/total_labor_force)*100, 
         Unemployed = (unemployed / total_labor_force)*100, 
         Poverty = (poverty/total_pop)*100,
         Poverty_1.5x = (poverty_1.5x/total_pop)*100,
         Poverty_2x = (poverty_2x/total_pop)*100,
         No_Highschool= (edu_no_highschool/total_pop)*100,
         Highschool_Degree = (edu_highschool/total_pop)*100,
         Bachelors = (edu_bachelors/total_pop)*100,
         Graduate = (edu_graduate/total_pop)*100,
         HealthCare_Coverage = (health_carecoverage/total_pop)*100)

census_table_output<- test2 %>%
  select(PropertyName, Median_Income, Median_Rent, Male, Female, Employed, Unemployed, Black, White, Hispanic, Native_American, Asian, Pacific_Islander, Poverty, Poverty_1.5x, Poverty_2x, No_Highschool, Highschool_Degree, Bachelors, Graduate, HealthCare_Coverage)
