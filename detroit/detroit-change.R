library(tidycensus)
library(leaflet)
library(tidyverse)
library(sf)
sf_use_s2(FALSE)
options(tigris_use_cache = TRUE)

w10 <- get_acs(geography = "tract", 
               variables = "B01003_001", 
               year = 2010, 
               state = "MI", 
               county = "Wayne",
               geometry = TRUE) %>%
  rename(estimate10 = estimate) %>%
  select(GEOID, estimate10)

w16 <- get_acs(geography = "tract", 
               variables = "B01003_001", 
               year = 2022, 
               state = "MI", 
               county = "Wayne", 
               geometry = TRUE) %>%
  rename(estimate16 = estimate) %>%
  select(GEOID, estimate16)

wayne_blocks <- tigris::blocks("MI", "Wayne", year = 2020)

w10 <- w10 %>%
  interpolate_pw(
    to = w16,
    to_id = "GEOID",
    extensive = TRUE,
    weights = wayne_blocks,
    weight_column = "POP20",
    crs = 4269
  )

wchange <- left_join(w16, st_drop_geometry(w10), by = "GEOID") %>%
  mutate(pctchange = ((estimate16 - estimate10) / estimate10) * 100) %>%
  st_transform(4326)

pal <- colorBin(palette = "PRGn",
                domain = NULL, 
                bins = c(-100, -50, -25, 0, 25, 50, 100, 1000)
)

popup <- paste0("Population, 2006-10: ", 
                wchange$estimate10, 
                "<br/>", 
                "Population: 2018-2022: ", 
                wchange$estimate16)

w1 <- leaflet() %>%
  addProviderTiles(providers$Stadia.StamenToner) %>%
  addPolygons(data = wchange, stroke = FALSE, smoothFactor = 0.2, 
              color = ~pal(pctchange), 
              fillOpacity = 0.8, 
              popup = popup) %>%
  addLegend(pal = pal, values = wchange$pctchange, 
            title = "Pop. change (%)") 

htmlwidgets::saveWidget(w1, "detroit/wayne_change.html")


