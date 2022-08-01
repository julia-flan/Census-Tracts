## Script to generate a list of census tracts for a census place or a county
## This script requires the input of a relevant_geos.csv which should have four columns:
## Geography: The name of the place or county
## County: The name of the county (one census place may exist in multiple counties)
## City: Official name of the census place or NA for counties
## Type: Label determining whether the geo is a "Place" or a "County"
## The script generates a tracts.csv in a HMDA subfolder with the following for every census tract within the county (and place if applicable)
## GEOID: Census tract geo id
## Name: Official Census name (Census Tract #, County, State)
## geo_name: Name of the census place or county


library(tidycensus)
library(tidyverse)
library(dplyr)
library(tigris)
library(sf)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # setting default directory

# Load ESI's default API Key.
# See https://api.census.gov/data/key_signup.html for more details.
tidycensus::census_api_key("f2f52f3ed440072977726da6d174da527f1f208a", install = TRUE, overwrite = TRUE)

## create force bind function
force_bind = function(df1, df2) {
  colnames(df2) = colnames(df1)
  bind_rows(df1, df2)}

geos <- read_csv("relevant_geos.csv")

# assign n to be the number of geographies of interest
n <- nrow(geos)
state <- (geos$State) ##list all the states
county <- (geos$County) ##list all the counties
city <-(geos$Geography)
type <-(geos$Type)


# Creating placeholder variables
vars <- c(total_hh = "B19001_001")


# create df to hold the results
tracts.export <- data.frame(GEO = "test", 
                                        Name = "test",
                                        geo_name = "test")


## loop through all geos
for (i  in 1:n){
  tracts_by_county <- get_acs(
    geography = "tract",
    variables = vars,
    state = state[i],
    county = county[i],
    geometry = TRUE,
    year = 2020
  )  
  
  if(type[i] == "Place"){
    place <- places(state = state[i], cb = FALSE, year = 2020) %>%
      filter_place(city[i])
    
    tracts_by_place <- as.data.frame(tracts_by_county[place,])%>%
      dplyr::select(c(GEOID, NAME))%>%
      mutate(geo_name = city[i])
    
    tracts.export <- force_bind(assign(paste('tracts.',i,sep=''),tracts_by_place), tracts.export)
  
    }## end if 
    else{
    
      tracts_by_county_clean <- as.data.frame(tracts_by_county) %>%
        dplyr::select(c(GEOID, NAME))%>%
        mutate(geo_name = county[i])
      
      tracts.export <- force_bind(assign(paste('tracts.',i,sep=''),tracts_by_county_clean), tracts.export)
  }## end else
}## end for loop

## remove the test row
tracts.export <- tracts.export %>%
  filter(NAME!= "test" )

## set working directory to HMDA subfolder and export tracts.csv
setwd(paste0(getwd(), "/HMDA")) 
write.csv(tracts.export, "tracts.csv")

