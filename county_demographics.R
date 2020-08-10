library(dplyr)
county_demo = read.csv(file = 'county_level_demographics.csv', stringsAsFactors = FALSE)
county_demo = county_demo %>% 
  filter(., YEAR ==12, AGEGRP== 0)
county_demo = county_demo %>% 
  select(., -STATE, -COUNTY, - SUMLEV) %>% 
  rename(., c("STATE" = "STNAME")) %>% 
  rename(., c("COUNTY" = "CTYNAME"))

#county breakdown by race and hispanic or latino
county_demo = county_demo %>% 
  group_by(., STATE, COUNTY) %>% 
  summarise(., tot_pop = sum(TOT_POP), white = sum(WA_MALE + WA_FEMALE), black =sum(BA_MALE + BA_FEMALE), 
            american_indian = sum(IA_MALE+ IA_FEMALE), asian = sum(AA_MALE + AA_FEMALE),
            native_hawaiian_pacific_islander = sum(NA_MALE + NA_FEMALE), two_or_more_races = sum(TOM_MALE + TOM_FEMALE),
            hispanic_or_latino = sum(H_MALE + H_FEMALE)) 
names(county_demo) = tolower(names(county_demo))
county_demo$county = tolower(trimws(gsub("County", "", county_demo$county)))
county_demo$state = tolower(trimws(county_demo$state))
state_demo = county_demo %>% 
  group_by(., state) %>% 
  summarise(., tot_pop = sum(TOT_POP), white = sum(WA_MALE + WA_FEMALE), black =sum(BA_MALE + BA_FEMALE), 
            american_indian = sum(IA_MALE+ IA_FEMALE), asian = sum(AA_MALE + AA_FEMALE),
            native_hawaiian_pacific_islander = sum(NA_MALE + NA_FEMALE), two_or_more_races = sum(TOM_MALE + TOM_FEMALE),
            hispanic_or_latino = sum(H_MALE + H_FEMALE))
