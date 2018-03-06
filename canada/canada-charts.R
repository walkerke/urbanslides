library(cancensus)
library(tidyverse)
library(extrafont)
options(cancensus.api_key = "CensusMapper_6293253518cbfcfc86b62e83f056f92f")

## Need: "CA16"

# Montreal: CMA 24462

View(list_census_vectors("CA16"))

# CMA
# v_CA16_1

pop <- get_census("CA16", level = "CMA", vectors = "v_CA16_1", 
                  regions = list(C = "01"))

pop <- arrange(pop, desc(Population)) 

pop10 <- pop[1:10,] %>%
  mutate(name = str_replace(`Region Name`, " .*", "")) %>%
  select(name, pop = Population)

ggplot(pop10, aes(x = pop, y = reorder(name, pop))) + 
  geom_segment(aes(x = 0, y = reorder(name, pop), 
                   xend = pop, yend = reorder(name, pop)), 
               color = "#90b4d2", size = 1) + 
  geom_point(size = 4, color = "#90b4d2") + 
  theme_minimal(base_family = "Verdana") + 
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 6000000, 1000000)) + 
  labs(x = "Population, 2016", 
       y = "", 
       title = "Largest metro areas in Canada", 
       caption = "Data acquired with the R cancensus package") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold"), 
        plot.caption = element_text(size = 7))
  

ggsave("img/metros.png")