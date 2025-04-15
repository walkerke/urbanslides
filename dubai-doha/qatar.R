library(idbr) 
library(ggplot2)
library(dplyr)
library(gganimate)
library(animation)
library(extrafont)

# idb_api_key("Your key goes here")

male <- get_idb('Qatar', 2025, sex = 'male') %>%
  mutate(pop = pop * -1,
         sex = 'Male')

female <- get_idb('Qatar', 2025, sex = 'female') %>%
  mutate(sex = 'Female')

qatar <- rbind(male, female) 

g1 <- ggplot(qatar, aes(x = age, y = pop, fill = sex, width = 1)) +
  coord_fixed() +
  coord_flip() +
  geom_bar(data = subset(qatar, sex == "Female"), stat = "identity", position = 'identity') +
  geom_bar(data = subset(qatar, sex == "Male"), stat = "identity", position = 'identity') +
  scale_y_continuous(breaks = seq(-50000, 50000, 25000),
                     labels = c('50k', '25k', '0', '25k', '50k'),
                     limits = c(min(qatar$pop), max(qatar$pop))) +
  theme_minimal(base_size = 14, base_family = "Tahoma") +
  scale_fill_manual(values = c('darkred', 'pink')) +
  ggtitle('Population structure of Qatar, 2025') + 
  labs(y = "Age", x = "Population", fill = "Sex") + 
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(caption = 'Chart by @kyle_e_walker | Data source: US Census Bureau IDB via the idbr R package') + 
  guides(fill = guide_legend(reverse = TRUE))


library(plotly)

g2 <- ggplotly(g1)

htmlwidgets::saveWidget(g2, "qatar.html")

gganimate(g1, interval = 0.1, ani.width = 700, ani.height = 600)