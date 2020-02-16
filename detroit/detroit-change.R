library(tidycensus)
library(leaflet)
library(tidyverse)
library(sf)
options(tigris_use_cache = TRUE)

w10 <- get_acs(geography = "tract", 
               variables = "B01003_001", 
               year = 2010, 
               state = "MI", 
               county = "Wayne") %>%
  rename(estimate10 = estimate) %>%
  select(GEOID, estimate10)

w16 <- get_acs(geography = "tract", 
               variables = "B01003_001", 
               year = 2018, 
               state = "MI", 
               county = "Wayne", 
               geometry = TRUE) %>%
  rename(estimate16 = estimate) %>%
  select(GEOID, estimate16)

wchange <- left_join(w16, w10, by = "GEOID") %>%
  mutate(pctchange = ((estimate16 - estimate10) / estimate10) * 100) %>%
  st_transform(4326)

pal <- colorBin(palette = "PRGn",
                domain = NULL, 
                bins = c(-100, -50, -25, 0, 25, 50, 500)
)

popup <- paste0("Population, 2006-10: ", 
                wchange$estimate10, 
                "<br/>", 
                "Population: 2014-2018: ", 
                wchange$estimate16)

w1 <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = wchange, stroke = FALSE, smoothFactor = 0.2, 
              color = ~pal(pctchange), 
              fillOpacity = 0.8, 
              popup = popup) %>%
  addLegend(pal = pal, values = wchange$pctchange, 
            title = "Pop. change (%)") 

htmlwidgets::saveWidget(w1, "wayne_change.html")


