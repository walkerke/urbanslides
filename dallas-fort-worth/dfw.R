library(tidycensus)
library(tidyverse)
library(extrafont)

metro18 <- get_acs(geography = "cbsa", 
                   variables = "DP03_0019P", 
                   survey = "acs1", 
                   summary_var = "B01003_001") %>%
  filter(summary_est > 1500000) 

metros <- metro18 %>%
  mutate(type = ifelse(GEOID == "19100", "DFW", "Other")) %>%
  mutate(NAME = str_replace(NAME, "-.*", "")) %>%
  mutate(NAME = str_replace(NAME, ",.*|/.*", "")) %>%
  arrange(estimate)

color <- ifelse(metros$NAME == "Dallas", "navy", "grey30")
face <- ifelse(metros$NAME == "Dallas", "bold", "plain")


d1 <- ggplot(metros, aes(x = estimate, y = reorder(NAME, estimate), 
                   color = type)) + 
  geom_segment(aes(x = 0, y = reorder(NAME, estimate), 
                   xend = estimate, yend = reorder(NAME, estimate), 
                   color = type)) + 
  geom_point(size = 3) + 
  theme_minimal(base_family = "Verdana", base_size = 16) + 
  scale_color_manual(values = c("navy", "#90b4d2"), guide = FALSE) + 
  scale_x_continuous(labels = function(x) { paste0(x, "%") }, 
                     expand = c(0.02, 0, 0.02, 0)) + 
  labs(x = "2018 1-year ACS estimate", 
       y = "", 
       title = "Percent driving alone to work", 
       subtitle = "Metro areas with populations above 1.5 million", 
       caption = "Workers age 16 and up. Data acquired with the R tidycensus package. Chart by @kyle_e_walker.") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold"), 
        plot.caption = element_text(size = 7), 
        axis.text.y = element_text(color = color, face = face))

ggsave("img/driving.png", d1, width = 11, height = 9)


metro18 <- get_acs(geography = "cbsa", 
                   variables = "DP03_0024P", 
                   survey = "acs1", 
                   summary_var = "B01003_001") %>%
  filter(summary_est > 1500000) 

metros <- metro18 %>%
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
  scale_color_manual(values = c("#90b4d2", "navy"), guide = FALSE) + 
  scale_x_continuous(labels = function(x) { paste0(x, "%") }, 
                     expand = c(0.02, 0, 0.02, 0)) + 
  labs(x = "2018 1-year ACS estimate", 
       y = "", 
       title = "Percent working at home", 
       subtitle = "Metro areas with populations above 1.5 million", 
       caption = "Workers age 16 and up. Data acquired with the R tidycensus package. Chart by @kyle_e_walker.") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold"), 
        plot.caption = element_text(size = 7), 
        axis.text.y = element_text(color = color, face = face))

ggsave("img/workathome.png", width = 9, height = 7)


place00 <- get_decennial(geography = "place", state = "TX", 
                         variables = "P001001", year = 2000) %>%
  mutate(NAME = str_replace(NAME, " city.*", "")) %>%
  select(NAME, pop00 = value)

place18 <- get_acs(geography = "place", state = "TX", 
                   variables = "B01003_001", survey = "acs1") %>%
  mutate(NAME = str_replace(NAME, " city.*", "")) %>%
  select(NAME, pop18 = estimate) %>%
  inner_join(place00, by = "NAME")

dfwp <- c("Allen", "Arlington", "Carrollton", "Dallas", "Denton", 
         "Fort Worth", "Frisco", "Garland", "Grand Prairie", 
         "Irving", "Lewisville", "McKinney", "Mansfield", 
         "Mesquite", "Plano", "Richardson")

dfw <- filter(place18, NAME %in% dfwp)

p <- ggplot(dfw) + 
  geom_segment(aes(x = pop00, xend = pop18, 
                   y = reorder(NAME, pop18), 
                   yend = reorder(NAME, pop18))) + 
  geom_point(aes(x = pop18, y = reorder(NAME, pop18), text = paste0("2018: ", pop18)), 
             color = "navy", size = 3) + 
  geom_point(aes(x = pop00, y = reorder(NAME, pop18), text = paste0("2000: ", pop00)), 
             color = "#90b4d2", size = 3) + 
  geom_text(data = data.frame(), aes(x = 1188580, y = "Dallas", label = "2000"), 
            color = "#90b4d2", hjust = 1, size = 3, fontface = "bold", 
            nudge_x = -80000) + 
  geom_text(data = data.frame(), aes(x = 1317942, y = "Dallas", label = "2018"), 
            color = "navy", hjust = 0, size = 3, fontface = "bold", 
            nudge_x = 80000) + 
  scale_x_continuous(labels = scales::comma, expand = c(0.1, 0.1)) + 
  theme_minimal(base_family = "Verdana", base_size = 12) + 
  labs(x = "Population size", 
       y = "", 
       title = "Population change in Dallas-Fort Worth") + 
  theme(panel.grid.major.y=element_blank())

library(plotly)
gg <- ggplotly(p, tooltip = "text")

htmlwidgets::saveWidget(gg, "popchange.html")
  


