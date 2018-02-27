library(tidycensus)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(extrafont)
library(magick)

options(tigris_use_cache = TRUE)

det_counties <- c("Wayne", "Oakland", "Macomb", 
                  "Livingston", "Washtenaw", "Monroe")

vars90 <- c(Hispanic = "P0080001", 
            White = "P0100001", 
            Black = "P0100002", 
            Asian = "P0100004", 
            Native = "P0100003", 
            Other = "P0100005")

r90 <- get_decennial(geography = "tract", 
                     variables = vars90, 
                     geometry = TRUE, 
                     state = "MI", 
                     county = det_counties, 
                     year = 1990, 
                     output = "wide") %>%
  mutate(Other = Native + Other) %>%
  select(-Native) %>%
  mutate(year = "1990")


vars00 <- c(Hispanic = "P004002", 
            White = "P004005", 
            Black = "P004006", 
            Asian = "P004008", 
            Native = "P004007",
            Hawaiian = "P004009",
            Other = "P004010", 
            Other2 = "P004011")

r00 <- get_decennial(geography = "tract", 
                     variables = vars00, 
                     geometry = TRUE, 
                     state = "MI", 
                     county = det_counties, 
                     year = 2000, 
                     output = "wide") %>%
  mutate(Other = Native + Hawaiian + Other + Other2) %>%
  select(-Native, -Hawaiian, -Other2) %>%
  mutate(year = "2000")


vars10 <- c(Hispanic = "P0040003", 
            White = "P0050003", 
            Black = "P0050004", 
            Asian = "P0050006", 
            Native = "P0050005",
            Hawaiian = "P0050007",
            Other = "P0050008", 
            Other2 = "P0050009")


r10 <- get_decennial(geography = "tract", 
                     variables = vars10, 
                     geometry = TRUE, 
                     state = "MI", 
                     county = det_counties, 
                     year = 2010, 
                     output = "wide") %>%
  mutate(Other = Native + Hawaiian + Other + Other2) %>%
  select(-Native, -Hawaiian, -Other2) %>%
  mutate(year = "2010")


vars16 <- c(Hispanic = "B03002_012", 
            White = "B03002_003", 
            Black = "B03002_004", 
            Asian = "B03002_006", 
            Native = "B03002_005",
            Hawaiian = "B03002_007",
            Other = "B03002_008", 
            Other2 = "B03002_009")

r16 <- get_acs(geography = "tract", 
                     variables = vars16, 
                     geometry = TRUE, 
                     state = "MI", 
                     county = det_counties, 
                     year = 2016, 
                     output = "wide") %>%
  mutate(Other = NativeE + HawaiianE + OtherE + Other2E) %>%
  rename(Hispanic = HispanicE, White = WhiteE, Black = BlackE, Asian = AsianE) %>%
  mutate(year = "2012-2016")


# Map

url <- "https://api.mapbox.com/styles/v1/kwalkertcu/cj0jov12u007n2sqppuxe3fvr/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJMRk9JSmRvIn0.l1y2jHZ6IARHM_rA1-X45A"

osm <- read_osm(bb(r90), type = url)

datasets <- list(r90, r00, r10, r16)

race <- c("Hispanic", "White", "Black", "Asian", "Other")

walk(datasets, function(d) {
  
  dots <- map(race, function(x) {
    
    d %>%
      st_transform(26917) %>%
      mutate(group = as.integer(d[[x]] / 200)) %>%
      st_sample(., .$group) %>%
      st_sf() %>%
      mutate(race = x)
    
  }) %>%
    reduce(rbind)
  
  dots <- dots[sample(nrow(dots)), ]
  
  m1 <- tm_shape(osm) + 
    tm_raster() + 
    tm_shape(dots) + 
    tm_dots(col = "race", palette = c("blue", "green", "orange", "pink", "purple"), 
            title = paste0("Race in Detroit, ", unique(d$year))) + 
    tm_layout(legend.position = c("right", "bottom"), fontfamily = "Verdana")
  
  save_tmap(m1, filename = paste0(unique(d$year), ".jpg"), 
            width = 8, height = 6)
  
})


i90 <- image_read("1990.jpg")
i00 <- image_read("2000.jpg")
i10 <- image_read("2010.jpg")
i16 <- image_read("2012-2016.jpg")

image_animate(c(i90, i00, i10, i16), fps = 0.5)

