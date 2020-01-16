library(tidycensus)
library(leaflet)
library(sf)
library(viridis)
options(tigris_use_cache = TRUE)

il1 <- get_acs(geography = "county", 
               variables = c(hhincome = "B19013_001"), 
               state = "IL", 
               geometry = TRUE, 
               year = 2018) %>%
  st_transform(4326)

il2 <- get_acs(geography = "tract", 
               variables = c(hhincome = "B19013_001"), 
               state = "IL", 
               geometry = TRUE,
               year = 2018) %>%
  st_transform(4326)


pal1 <- colorNumeric("viridis", il1$estimate)

pal2 <- colorNumeric("viridis", il2$estimate)

bins <- c(0, 30000, 40000, 50000, 60000, 70000, 80000, 250000)

pala <- colorBin("viridis", il1$estimate, bins = bins)

palb <- colorBin("viridis", il2$estimate, bins = bins)


l1 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = il1, stroke = FALSE, smoothFactor = 0.2, 
              color = ~pala(estimate), 
              label = ~as.character(estimate), 
              fillOpacity = 0.8, 
              group = "Counties") %>%
  addPolygons(data = il2, stroke = FALSE, smoothFactor = 0.2, 
              color = ~pala(estimate), 
              label = ~as.character(estimate), 
              fillOpacity = 0.8, 
              group = "Tracts") %>%
  addLegend(pal = pala, values = il1$estimate, 
            title = "Med. HH Income") %>%
  addLayersControl(overlayGroups = c("Tracts", "Counties")) %>%
  hideGroup("Tracts")

htmlwidgets::saveWidget(l1, "il_income.html")
