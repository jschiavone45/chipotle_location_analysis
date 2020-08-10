library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(leaflet)
library(googleVis)
library(shinydashboard)
library(pheatmap)
chipotles = read.csv(file = "chipotle_county_data2.csv", stringsAsFactors = FALSE)
store_loc = read.csv(file = "chipotle_stores.csv", stringsAsFactors = FALSE)
state_stat = store_loc %>% 
  group_by(., state) %>% 
  summarise(., Chipotles = n())

chipotles = chipotles %>% 
  mutate(., Population_Density = (Total_Population / square_miles))
