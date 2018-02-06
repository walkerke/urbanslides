library(tidycensus)
library(tidyverse)
library(extrafont)

# View(load_variables(2016, "acs1/profile", cache = TRUE))

info <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                variables = "DP03_0039P", 
                summary_var = "B01003_001", 
                survey = "acs1") %>%
  filter(summary_est > 1500000) %>%
  mutate(NAME = str_replace(NAME, "-.*", "")) %>%
  mutate(NAME = str_replace(NAME, ",.*|/.*", ""))

ggplot(info, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_segment(aes(x = -0.2, y = reorder(NAME, estimate), 
                   xend = estimate, yend = reorder(NAME, estimate)), 
               color = "#386890") + 
  geom_point(color = "#386890", size = 3) + 
  theme_minimal(base_family = "Verdana") + 
  scale_x_continuous(labels = function(x) { paste0(x, "%") }, 
                     expand = c(0, 0, 0.02, 0)) + 
  labs(x = "2016 American Community Survey estimate", 
       y = "", 
       title = "Percent employed in the information services industry", 
       subtitle = "Metropolitan areas with population above 1.5 million", 
       caption = "Employed, civilian population age 16 and up. Data acquired with the R tidycensus package. Chart by @kyle_e_walker.") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold"), 
        plot.caption = element_text(size = 7))

ggsave("img/info.png", width = 9, height = 7)

prof <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                variables = "DP03_0041P", 
                survey = "acs1", 
                summary_var = "B01003_001") %>%
  filter(summary_est > 1500000) %>%
  mutate(NAME = str_replace(NAME, "-.*", "")) %>%
  mutate(NAME = str_replace(NAME, ",.*|/.*", ""))


ggplot(prof, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_segment(aes(x = -0.2, y = reorder(NAME, estimate), 
                   xend = estimate, yend = reorder(NAME, estimate)), 
               color = "#386890") + 
  geom_point(color = "#386890", size = 3) + 
  theme_minimal(base_family = "Verdana") + 
  scale_x_continuous(labels = function(x) { paste0(x, "%") }, 
                     expand = c(0, 0, 0.02, 0)) + 
  labs(x = "2016 American Community Survey estimate", 
       y = "", 
       title = "Percent employed in the professional services industry", 
       subtitle = "Metropolitan areas with population above 1.5 million", 
       caption = "Employed, civilian population age 16 and up. Data acquired with the R tidycensus package. Chart by @kyle_e_walker.") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold"), 
        plot.caption = element_text(size = 7))

ggsave("img/prof.png", width = 9, height = 7)


manuf <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                variables = "DP03_0035P", 
                survey = "acs1", 
                summary_var = "B01003_001") %>%
  filter(summary_est > 1500000) %>%
  mutate(NAME = str_replace(NAME, "-.*", "")) %>%
  mutate(NAME = str_replace(NAME, ",.*|/.*", ""))


ggplot(manuf, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_segment(aes(x = -0.2, y = reorder(NAME, estimate), 
                   xend = estimate, yend = reorder(NAME, estimate)), 
               color = "#386890") + 
  geom_point(color = "#386890", size = 3) + 
  theme_minimal(base_family = "Verdana") + 
  scale_x_continuous(labels = function(x) { paste0(x, "%") }, 
                     expand = c(0, 0, 0.02, 0)) + 
  labs(x = "2016 American Community Survey estimate", 
       y = "", 
       title = "Percent employed in the manufacturing industry", 
       subtitle = "Metropolitan areas with population above 1.5 million", 
       caption = "Employed, civilian population age 16 and up. Data acquired with the R tidycensus package. Chart by @kyle_e_walker.") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold"), 
        plot.caption = element_text(size = 7))

ggsave("img/manuf.png", width = 9, height = 7)




library(tidycensus)
library(tmap)
library(sf)
library(tidyverse)
library(tigris)

x <- load_variables(2016, "acs5", cache = TRUE)

try <- get_acs(geography = "tract", 
               variables = c(artists = "C24050_026"), 
               state = "CA", 
               county = "San Francisco")

