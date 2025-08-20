library(tidycensus)
library(mapgl)
library(sf)
options(tigris_use_cache = TRUE)

il1 <- get_acs(
  geography = "county",
  variables = c(hhincome = "B19013_001"),
  state = "IL",
  geometry = TRUE,
  year = 2023
)

il2 <- get_acs(
  geography = "tract",
  variables = c(hhincome = "B19013_001"),
  state = "IL",
  geometry = TRUE,
  year = 2023
)

mapboxgl(bounds = il1, config = list(basemap = list(theme = "monochrome"))) |> 
  add_fill_layer(
    id = "tracts",
    source = il2,
    min_zoom = 7,
    fill_color = interpolate(
      column = "estimate",
      values = c(0, 30000, 60000, 90000, 120000, 250000),
      stops = viridisLite::viridis(6),
      na_color = "lightgrey"
    ), 
    fill_opacity = 1
  ) |> 
  add_fill_layer(
    id = "counties",
    source = il1,
    max_zoom = 9,
    fill_color = interpolate(
      column = "estimate",
      values = c(0, 30000, 60000, 90000, 120000, 250000),
      stops = viridisLite::viridis(6)
    ), 
    fill_opacity = interpolate(
      property = "zoom",
      values = 7:9,
      stops = c(1, 0.5, 0)
    )
  ) |> 
  add_legend(
    "Median household income",
    values = c("$0k", "$30k", "$60k", "$120k", "$250k"),
    colors = viridisLite::viridis(6)
  ) |> 
  add_fullscreen_control()


pal1 <- colorNumeric("viridis", il1$estimate)

pal2 <- colorNumeric("viridis", il2$estimate)

bins <- c(0, 30000, 40000, 50000, 60000, 70000, 80000, 250000)

pala <- colorBin("viridis", il1$estimate, bins = bins)

palb <- colorBin("viridis", il2$estimate, bins = bins)


l1 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = il1,
    stroke = FALSE,
    smoothFactor = 0.2,
    color = ~ pala(estimate),
    label = ~ as.character(estimate),
    fillOpacity = 0.8,
    group = "Counties"
  ) %>%
  addPolygons(
    data = il2,
    stroke = FALSE,
    smoothFactor = 0.2,
    color = ~ pala(estimate),
    label = ~ as.character(estimate),
    fillOpacity = 0.8,
    group = "Tracts"
  ) %>%
  addLegend(pal = pala, values = il1$estimate, title = "Med. HH Income") %>%
  addLayersControl(overlayGroups = c("Tracts", "Counties")) %>%
  hideGroup("Tracts")

htmlwidgets::saveWidget(l1, "il_income.html")
