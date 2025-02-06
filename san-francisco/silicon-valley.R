library(tigris)
library(tidycensus)
library(sf)
library(tidyverse)
library(extrafont)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

county_names <- c("San Francisco", "Alameda", "Contra Costa", "Marin", 
                  "Napa", "San Mateo", "Santa Clara", "Solano", "Sonoma")

bay_water <- map(county_names, function(x) {
  area_water("CA", x)
}) %>%
  rbind_tigris()

bay_value <- get_acs(geography = "tract", 
                    variables = c(rent = "B25077_001"), 
                    state = "CA", 
                    county = county_names, 
                    geometry = TRUE) %>%
  st_transform(4326)

# st_erase <- function(x, y) {
#   st_difference(x, st_union(st_combine(y)))
# }
# 
# bay_erase <- st_erase(bay_rent, bay_water)

library(mapview)

mapview(bay_value, zcol = "estimate", legend = TRUE)

library(leaflet)
library(viridis)

pal <- colorNumeric("viridis", bay_value$estimate)

sf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = bay_value, weight = 0.5, 
              smoothFactor = 0.2, 
              color = "grey", 
              fillColor = ~pal(estimate), 
              label = ~paste0("$", estimate), 
              fillOpacity = 0.7, 
              highlightOptions = highlightOptions(color = "aqua", weight = 2, 
                                                  opacity = 1, bringToFront = TRUE, 
                                                  sendToBack = TRUE)) %>%
  addLegend(position = "bottomright", 
            pal = pal, title = "Median home value (2023)", 
            values = bay_value$estimate)

htmlwidgets::saveWidget(sf, "value.html")


# rent by metro

rent <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                variables = "B25064_001", 
                summary_var = "B01003_001", 
                survey = "acs1") %>%
  filter(summary_est > 1500000) %>%
  mutate(NAME = str_replace(NAME, "-.*", "")) %>%
  mutate(NAME = str_replace(NAME, ",.*|/.*", ""))

ggplot(rent, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_segment(aes(x = -0.2, y = reorder(NAME, estimate), 
                   xend = estimate, yend = reorder(NAME, estimate)), 
               color = "#386890") + 
  geom_point(color = "#386890", size = 3) + 
  theme_minimal(base_family = "Verdana") + 
  scale_x_continuous(labels = scales::dollar_format(), 
                     expand = c(0, 0, 0.02, 0)) + 
  labs(x = "2023 American Community Survey estimate", 
       y = "", 
       title = "Median gross rent (dollars)", 
       subtitle = "Metropolitan areas with population above 1.5 million", 
       caption = "Data acquired with the R tidycensus package. Chart by @kyle_e_walker.") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold"), 
        plot.caption = element_text(size = 7),
        plot.margin = margin(r = 30))

ggsave("img/rent.png", width = 9, height = 7, bg = "white")

# value by metro

value <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                variables = "B25077_001", 
                summary_var = "B01003_001", 
                survey = "acs1") %>%
  filter(summary_est > 1500000) %>%
  mutate(NAME = str_replace(NAME, "-.*", "")) %>%
  mutate(NAME = str_replace(NAME, ",.*|/.*", ""))

ggplot(value, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_segment(aes(x = -0.2, y = reorder(NAME, estimate), 
                   xend = estimate, yend = reorder(NAME, estimate)), 
               color = "#386890") + 
  geom_point(color = "#386890", size = 3) + 
  theme_minimal(base_family = "Verdana") + 
  scale_x_continuous(labels = scales::dollar, 
                     expand = c(0, 0, 0.02, 0)) + 
  labs(x = "2023 American Community Survey estimate", 
       y = "", 
       title = "Median housing value (dollars)", 
       subtitle = "Metropolitan areas with population above 1.5 million", 
       caption = "Data acquired with the R tidycensus package. Chart by @kyle_e_walker.") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold"), 
        plot.caption = element_text(size = 7),
        plot.margin = margin(r = 30))

ggsave("img/value.png", width = 9, height = 7, bg = "white")

vars <- c(paste0)

educ <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                 variables = "DP02_0068P", 
                 summary_var = "B01003_001", 
                 survey = "acs1") %>%
  filter(summary_est > 1500000, !str_detect(NAME, "San Juan")) %>%
  mutate(NAME = str_replace(NAME, "-.*", "")) %>%
  mutate(NAME = str_replace(NAME, ",.*|/.*", "")) 

ggplot(educ, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_segment(aes(x = -0.2, y = reorder(NAME, estimate), 
                   xend = estimate, yend = reorder(NAME, estimate)), 
               color = "#386890") + 
  geom_point(color = "#386890", size = 3) + 
  theme_minimal(base_family = "Verdana") + 
  scale_x_continuous(labels = function(x) {paste0(x, "%")}, 
                     expand = c(0, 0, 0.02, 0)) + 
  labs(x = "2023 American Community Survey estimate", 
       y = "", 
       title = "Percent age 25+ with at least a bachelor's degree", 
       subtitle = "Metropolitan areas with population above 1.5 million", 
       caption = "Data acquired with the R tidycensus package. Chart by @kyle_e_walker.") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold"), 
        plot.caption = element_text(size = 7))

ggsave("img/educ.png", width = 9, height = 7)


educ12 <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                variables = "DP02_0067P", 
                summary_var = "B01003_001", 
                survey = "acs1", 
                year = 2012) %>%
  filter(summary_est > 1500000) %>%
  mutate(NAME = str_replace(NAME, "-.*", "")) %>%
  mutate(NAME = str_replace(NAME, ",.*|/.*", ""))
