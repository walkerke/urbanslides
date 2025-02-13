library(tidycensus)
library(tidyverse)
library(extrafont)

city21 <- get_acs(geography = "place", 
                   variables = "B01003_001", 
                   survey = "acs1",
                   year = 2023) %>%
  filter(estimate > 500000) %>%
  rename(estimate21 = estimate)

city10 <- get_acs(geography = "place", 
                   variables = "B01003_001", 
                   survey = "acs1", 
                   year = 2013) %>%
  rename(estimate10 = estimate) %>%
  select(GEOID, estimate10)
  
  
cities <- inner_join(city21, city10, by = "GEOID") %>%
  mutate(pctchange = round(100 * ((estimate21 - estimate10) / estimate10), 2)) %>%
  filter(GEOID != "41980") %>%
  mutate(type = case_when(
    GEOID == "4835000" ~ "Houston", 
    pctchange < 0 ~ "decline", 
    pctchange > 0 ~ "growth"
  )) %>%
  mutate(NAME = str_replace(NAME, " city.*|-.*|/.*", "")) %>%
  arrange(pctchange)

color <- ifelse(cities$NAME == "Houston", "navy", "grey30")
face <- ifelse(cities$NAME == "Houston", "bold", "plain")


g1 <- ggplot(cities, aes(x = pctchange, y = reorder(NAME, pctchange), 
                   color = type)) + 
  geom_segment(aes(x = 0, y = reorder(NAME, pctchange), 
                   xend = pctchange, yend = reorder(NAME, pctchange), 
                   color = type)) + 
  geom_point(size = 3) + 
  theme_minimal(base_family = "Verdana") + 
  scale_color_manual(values = c("red", "#90b4d2", "navy"), guide = FALSE) + 
  scale_x_continuous(labels = function(x) { paste0(x, "%") }, 
                     expand = c(0.02, 0, 0.02, 0)) + 
  labs(x = "Percent change between 2013 and 2023", 
       y = "", 
       title = "Municipal population change, 2013-2023", 
       subtitle = "Cities with populations above 500,000", 
       caption = "Data acquired with the R tidycensus package. Chart by @kyle_e_walker.") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold"), 
        plot.caption = element_text(size = 7), 
        axis.text.y = element_text(color = color, face = face))

ggsave("houston/img/change.png", g1, width = 9, height = 6)


metro21 <- get_acs(geography = "cbsa", 
                   variables = "B01003_001", 
                   survey = "acs1",
                   year = 2023) %>%
  filter(estimate > 1500000) %>%
  rename(estimate21 = estimate)

metro13 <- get_acs(geography = "cbsa", 
                   variables = "B01003_001", 
                   survey = "acs1", 
                   year = 2013) %>%
  rename(estimate13 = estimate) %>%
  select(GEOID, estimate13)

# metro10 <- map_df(c(state.abb, "DC"), function (x) {
#   
#   get_decennial(geography = "metropolitan statistical area/micropolitan statistical area", 
#                 variables = "P0010001", 
#                 state = x)
#   
#   
# })

metros <- inner_join(metro21, metro13, by = "GEOID") %>%
  mutate(pctchange = round(100 * ((estimate21 - estimate13) / estimate13), 2)) %>%
  filter(GEOID != "41980") %>%
  mutate(type = case_when(
    GEOID == "26420" ~ "Houston", 
    pctchange < 0 ~ "decline", 
    pctchange > 0 ~ "growth"
  )) %>%
  mutate(NAME = str_replace(NAME, "-.*", "")) %>%
  mutate(NAME = str_replace(NAME, ",.*|/.*", "")) %>%
  arrange(pctchange)

color <- ifelse(metros$NAME == "Houston", "navy", "grey30")
face <- ifelse(metros$NAME == "Houston", "bold", "plain")


g2 <- ggplot(metros, aes(x = pctchange, y = reorder(NAME, pctchange), 
                   color = type)) + 
  geom_segment(aes(x = 0, y = reorder(NAME, pctchange), 
                   xend = pctchange, yend = reorder(NAME, pctchange), 
                   color = type)) + 
  geom_point(size = 3) + 
  theme_minimal(base_family = "Verdana") + 
  scale_color_manual(values = c("red", "#90b4d2", "navy"), guide = FALSE) + 
  scale_x_continuous(labels = function(x) { paste0(x, "%") }, 
                     expand = c(0.02, 0, 0.02, 0)) + 
  labs(x = "Percent change between 2013 and 2023", 
       y = "", 
       title = "Metropolitan population change, 2013-2023", 
       subtitle = "Metro areas with populations above 1.5 million", 
       caption = "Data acquired with the R tidycensus package. Chart by @kyle_e_walker.") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold"), 
        plot.caption = element_text(size = 7), 
        axis.text.y = element_text(color = color, face = face))

ggsave("houston/img/metro_change.png", g2, width = 9, height = 7)