library(dplyr)
library(ggplot2)
library(maps)
library(tidyr)
library(maptools)
library(openintro)
library(stringr)


stores = read.csv(file = 'chipotle_stores.csv', stringsAsFactors = FALSE)
str(stores)
stores$location
latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}
testPoints <- data.frame(x = stores$longitude, y = stores$latitude)
stores = stores %>% 
  mutate(., county = latlong2county(testPoints))
str(stores)
stores = stores %>% 
  separate(., col=county, into = c("state", "county"), sep = ",")
# load in county income
county_income = read.csv('income_by_county.csv', stringsAsFactors = FALSE)
#split location into county and state
county_income = county_income %>% 
  separate(., col=county, into = c("county", "state"), sep = ",")
county_income$state = trimws(county_income$state)
county_income$state
county_income = county_income %>% 
  mutate(., state = str_remove(county_income$state, "\\*"))
county_income$state = abbr2state(county_income$state)

county_income2 = county_income %>% 
  filter(., linecode ==2) %>% 
  select(., county, state, year) %>% 
  rename(., c('county_pop' = 'year'))
county_data = inner_join(county_income_per_capita, county_income2, by=c("county", "state"))
county_data = county_data %>% 
  rename(., c("per_capita_income" = "year"))
colnames(county_income)
#filter county income by per capita income and convert to numeric
county_income = county_income %>% 
 rename(., c("year" = "capita_income")) 
county_data = county_data %>% select(., -linecode)
county_data = county_data %>% select(., -description)

# might not need this
#county_income_per_capita = county_income_per_capita %>% 
#  unite(., "county_state", c("county","state")) %>% 
 # mutate(., county_state = tolower(county_state))
county_count = stores %>% 
  group_by(., state, county) %>% 
  summarize(store_count = n())

county_data = county_data %>% 
  mutate(., state= tolower(state)) %>% 
  mutate(., county = tolower(county))
county_data = full_join(county_data, county_count, by=c("county", "state"))
county_data$store_count[is.na(county_data$store_count)] = 0  
county_data = county_data %>% 
  mutate(., has_chipotle = store_count >= 1)

county_data$per_capita_income =as.numeric(county_data$per_capita_income)
county_data$county_pop=as.numeric(county_data$county_pop)
county_data = county_data %>% 
  mutate(., pop_per_store = (county_pop / store_count)) %>% 
  mutate(., pop_per_store = ifelse(pop_per_store==Inf, county_pop, pop_per_store))

county_data = inner_join(county_data,county_demo, by=c("county", "state"))

county_gdp = read.csv(file = 'county_gdp.csv', stringsAsFactors = FALSE)
county_gdp = county_gdp %>% 
  separate(., county, into = c("county", "state"), sep=",") %>% 
  mutate(., county = trimws(tolower(county))) %>% 
  mutate(., state = tolower(abbr2state(trimws(state)))) %>% 
  mutate(., gdp_thous = as.numeric(gdp_thous))
str(county_data)
county_data = county_data %>% 
  mutate(., gdp_thous = as.numeric(gdp_thous))
county_data = inner_join(county_data,county_gdp, by=c("county", "state"))


county_data2 = county_data

county_data2 = county_data2 %>% 
  mutate(., white = round((white/ tot_pop), 2)) %>% 
  mutate(., black = round((black/ tot_pop), 2)) %>% 
  mutate(., american_indian = round((american_indian/ tot_pop), 2)) %>% 
  mutate(., asian = round((asian/ tot_pop), 2)) %>% 
  mutate(., native_hawaiian_pacific_islander = round((native_hawaiian_pacific_islander/ tot_pop), 2)) %>% 
  mutate(., two_or_more_races = round((two_or_more_races/ tot_pop), 2)) %>% 
  mutate(., hispanic_or_latino = round((hispanic_or_latino/ tot_pop), 2))
  
county_area = read.csv(file ="county_area.csv", stringsAsFactors = FALSE)
county_area = county_area %>% 
  separate(., Areaname, into = c("county", "state"), sep=",")
county_area$county = trimws(tolower(county_area$county))
county_area$state = tolower(abbr2state(trimws(county_area$state)))
county_data2 = inner_join(county_data2, county_area, by=c("county", "state"))

write.csv(county_data2, "chipotle_county_data2.csv", row.names=FALSE)
#write.csv(test,"chipotle_county_data.csv", row.names = FALSE)

summary(county_data)
sum(county_data$store_count)
state_counts = county_data %>% 
  group_by(., state) %>% 
  tally(., store_count)
summary(state_counts)
# 
state_pop = read.csv(file = "state_populations.csv", stringsAsFactors = FALSE)
state_pop$state = tolower(state_pop$state)
str(state_counts)
state_counts = inner_join(state_counts, state_pop, by="state")
median(state_counts$population / state_counts$state_count)

ggplot(county_data, aes(x = per_capita_income))+
  geom_density()


ggplot(county_data, aes(x = county_pop, color = has_chipotle))+
  geom_density()

ggplot(county_data, aes(x = has_chipotle, y = gdp_thous))+
  geom_boxplot()


ggplot(county_data, aes(x = has_chipotle, y = per_capita_income), na.rm=T)+
  geom_boxplot()

ggplot(county_data, aes(x = has_chipotle, y = pop_per_store))+
  geom_boxplot()

str(test1)
test2 = test1 %>% 
  separate(., col=county_state, into = c("county", "state"), sep = "_")
str(test2)
county_data %>% 
  filter(., has_chipotle==FALSE& pop_per_store > 500000)
# boxplots by state
tester = county_data %>% 
  group_by(., state) %>% 
  filter(., state == "north dakota")


ggplot(tester, aes(x=has_chipotle, y = per_capita_income ))+
  geom_boxplot()


ggplot(county_data, aes(x = store_count, y = per_capita_income)) +
  geom_point()

ggplot(county_data, aes(x = ))

county_data3 = county_data2 %>% 
  select(., per_capita_income, county_pop, store_count, pop_per_store, white, black,
         native_hawaiian_pacific_islander, asian, american_indian, two_or_more_races, hispanic_or_latino)
cor(county_data3$store_count, county_data3)

countydata2 = county_data2 %>% 
  mutate(., Population_Density = (tot_pop/ square_miles))

countydata2 %>% 
  ggplot(., aes(x = has_chipotle, y = (Population_Density/ store_count)))+
  geom_boxplot()
countydata2 %>% 
  filter(., has_chipotle==T) %>%
  select(., Population_Density, pop_per_store) %>% 
  summary(.,)
countydata2 %>% 
  filter(., has_chipotle==F) %>%
  select(., Population_Density, pop_per_store) %>% 
  summary(.,)
countydata2 %>% 
  filter(., pop_per_store > 109000, Population_Density >392)
