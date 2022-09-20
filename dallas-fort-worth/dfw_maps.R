library(tidycensus)
library(tmap)
library(tmaptools)
library(sf)
library(tigris)
library(magick)
library(tidyverse)
options(tigris_use_cache = TRUE)

ctys <- c("Dallas", "Tarrant", "Collin County", "Denton", 
          "Parker", "Wise", "Johnson", "Ellis", "Rockwall", 
          "Kaufman")

p90 <- get_decennial(geography = "tract", variables = "P0010001", 
                     state = "TX", county = ctys, geometry = TRUE, 
                     year = 1990) %>%
  mutate(year = "1990")

p00 <- get_decennial(geography = "tract", variables = "P001001", 
                     state = "TX", county = ctys, geometry = TRUE, 
                     year = 2000) %>%
  mutate(year = "2000")

p10 <- get_decennial(geography = "tract", variables = "P001001", 
                     state = "TX", county = ctys, geometry = TRUE, 
                     year = 2010) %>%
  mutate(year = "2010")

p20 <- get_decennial(geography = "tract", variables = "P1_001N", 
               state = "TX", county = ctys, geometry = TRUE) %>%
  mutate(year = "2020")

dfs <- list(p90, p00, p10, p20)

url <- "https://api.mapbox.com/styles/v1/kwalkertcu/cj0jov12u007n2sqppuxe3fvr/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJMRk9JSmRvIn0.l1y2jHZ6IARHM_rA1-X45A"


osm <- read_osm(bb(p90), type = url)

walk(dfs, function(x) {
  
  dots <- x %>%
    st_transform(26914) %>%
    mutate(value = as.integer(value / 200)) %>%
    st_sample(., .$value) %>%
    st_sf() 
  
  b1 <- tm_shape(osm) + 
    tm_raster() + 
    tm_shape(dots) + 
    tm_dots(alpha = 0.4) + 
    tm_layout(title = unique(x$year)) + 
    tm_credits("1 dot = 200 people. Data source: Census/ACS via the R tidycensus package")
  
  save_tmap(b1, paste0("img/", unique(x$year), ".jpg"), dpi = 96)
  
})

i90 <- image_read("img/1990.jpg")
i00 <- image_read("img/2000.jpg")
i10 <- image_read("img/2010.jpg")
i16 <- image_read("img/2012-2016.jpg")

image_animate(c(i90, i00, i10, i16), fps = 0.5)

