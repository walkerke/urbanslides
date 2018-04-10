library(idbr) 
library(ggplot2)
library(dplyr)
library(gganimate)
library(animation)
library(extrafont)

# idb_api_key("Your key goes here")

male <- idb1('Qatar', 2018, sex = 'male') %>%
  mutate(POP = POP * -1,
         SEX = 'Male')

female <- idb1('Qatar', 2018, sex = 'female') %>%
  mutate(SEX = 'Female')

qatar <- rbind(male, female) 

g1 <- ggplot(qatar, aes(x = AGE, y = POP, fill = SEX, width = 1, frame = time)) +
  coord_fixed() +
  coord_flip() +
  geom_bar(data = subset(qatar, SEX == "Female"), stat = "identity", position = 'identity') +
  geom_bar(data = subset(qatar, SEX == "Male"), stat = "identity", position = 'identity') +
  scale_y_continuous(breaks = seq(-50000, 50000, 25000),
                     labels = c('50k', '25k', '0', '25k', '50k'),
                     limits = c(min(qatar$POP), max(qatar$POP))) +
  theme_minimal(base_size = 14, base_family = "Tahoma") +
  scale_fill_manual(values = c('darkred', 'pink')) +
  ggtitle('Population structure of Qatar, 2018') + 
  ylab('Population') +
  xlab('Age') +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(caption = 'Chart by @kyle_e_walker | Data source: US Census Bureau IDB via the idbr R package') + 
  guides(fill = guide_legend(reverse = TRUE))


library(plotly)

g2 <- ggplotly(g1)

htmlwidgets::saveWidget(g2, "qatar.html")

gganimate(g1, interval = 0.1, ani.width = 700, ani.height = 600)