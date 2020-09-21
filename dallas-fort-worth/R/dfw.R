library(tidycensus)
library(tidyverse)
library(extrafont)

metro16 <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                   variables = "DP03_0019P", 
                   survey = "acs1", 
                   summary_var = "B01003_001") %>%
  filter(summary_est > 1500000) 

metros <- metro16 %>%
  mutate(type = ifelse(GEOID == "19100", "DFW", "Other")) %>%
  mutate(NAME = str_replace(NAME, "-.*", "")) %>%
  mutate(NAME = str_replace(NAME, ",.*|/.*", "")) %>%
  arrange(estimate)

color <- ifelse(metros$NAME == "Dallas", "navy", "grey30")
face <- ifelse(metros$NAME == "Dallas", "bold", "plain")


ggplot(metros, aes(x = estimate, y = reorder(NAME, estimate), 
                   color = type)) + 
  geom_segment(aes(x = 0, y = reorder(NAME, estimate), 
                   xend = estimate, yend = reorder(NAME, estimate), 
                   color = type)) + 
  geom_point(size = 3) + 
  theme_minimal(base_family = "Verdana") + 
  scale_color_manual(values = c("navy", "#90b4d2"), guide = FALSE) + 
  scale_x_continuous(labels = function(x) { paste0(x, "%") }, 
                     expand = c(0.02, 0, 0.02, 0)) + 
  labs(x = "2018 1-year ACS estimate", 
       y = "", 
       title = "Percent of commuters driving alone to work", 
       subtitle = "Metro areas with populations above 1.5 million", 
       caption = "Data acquired with the R tidycensus package. Chart by @kyle_e_walker.") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold"), 
        plot.caption = element_text(size = 7), 
        axis.text.y = element_text(color = color, face = face))

ggsave("dallas-fort-worth/img/driving.png", width = 9, height = 7)