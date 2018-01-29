library(tidycensus)
library(tigris)
library(tidyverse)
library(sf)
library(mapview)
options(tigris_use_cache = TRUE)

st_erase <- function(x, y) {
  st_difference(x, st_union(st_combine(y)))
}

water <- area_water("NY", "Kings", class = "sf")

g <- map_df(2010, 2016, function (x) {
  
  get_acs(geography = "tract", 
          variables = c(bachelors = "DP02_0064", 
                        graduate = "DP02_0065"), 
          state = "NY", 
          county = "Kings", 
          geometry = TRUE, 
          cb = FALSE)
  
})  

test <- st_erase(g16, water) 

