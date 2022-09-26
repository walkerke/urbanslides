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

# p90 <- get_decennial(geography = "tract", variables = "P0010001", 
#                      state = "TX", county = ctys, geometry = TRUE, 
#                      year = 1990) %>%
#   mutate(year = "1990")

p00 <- get_decennial(geography = "tract", variables = "P001001", 
                     state = "TX", county = ctys, geometry = TRUE, 
                     year = 2000) %>%
  mutate(year = "2000")

p10 <- get_decennial(geography = "tract", variables = "P001001", 
                     state = "TX", county = ctys, geometry = TRUE, 
                     year = 2010) %>%
  mutate(year = "2010")

p20 <- get_decennial(geography = "tract", variables = "P1_001N", 
               state = "TX", county = ctys, geometry = TRUE,
               year = 2020) %>%
  mutate(year = "2020")

dfs <- list(p00, p10, p20)

url <- "https://api.mapbox.com/styles/v1/kwalkertcu/cj0jov12u007n2sqppuxe3fvr/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJMRk9JSmRvIn0.l1y2jHZ6IARHM_rA1-X45A"


# osm <- read_osm(bb(p00), type = url)
osm <- mapboxapi::get_static_tiles(location = p00,
                                   style_id = "cj0jov12u007n2sqppuxe3fvr",
                                   username = "kwalkertcu", 
                                   zoom = 9)

walk(dfs, function(x) {
  
  dots <- x %>%
    st_transform(26914) %>%
    as_dot_density(
      value = "value",
      values_per_dot = 200
    )
  
  # dots <- x %>%
  #   st_transform(26914) %>%
  #   mutate(value = as.integer(value / 200)) %>%
  #   st_sample(., .$value) %>%
  #   st_sf() 
  
  b1 <- tm_shape(osm) + 
    tm_rgb() + 
    tm_shape(dots) + 
    tm_dots(alpha = 0.4) + 
    tm_layout(title = unique(x$year)) + 
    tm_credits("1 dot = 200 people. Data source: US Census via the R tidycensus package")
  
  tmap_save(b1, paste0("dallas-fort-worth/img/", unique(x$year), ".jpg"), dpi = 96)
  
})

# i90 <- image_read("img/1990.jpg")
i00 <- image_read("dallas-fort-worth/img/2000.jpg")
i10 <- image_read("dallas-fort-worth/img/2010.jpg")
i20 <- image_read("dallas-fort-worth/img/2020.jpg")

image_animate(c(i00, i10, i20), fps = 0.5)

