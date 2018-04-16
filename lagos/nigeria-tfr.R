library(rnaturalearth)
library(sf)
library(plotly)
library(crosstalk)
library(viridis)

ng <- ne_states(country = "Nigeria", returnclass = "sf") %>%
  select(Name = name) 

# Source: https://dhsprogram.com/pubs/pdf/FR293/FR293.pdf

ng$TFR <- c(4.7, 4.2, 3.9, 4.8, 3.8, 4.5, 
            5.2, 5.4, 6.0, 5.1, 4.1, 6.1, 
            5.4, 5.2, 4.3, 4.1, 4.5, 4.2, 
            8.1, 7.0, 4.1, 4.4, 4.8, 5.3, 
            4.1, 4.2, 5.4, 5.4, 7.6, 6.8, 
            7.4, 7.0, 8.4, 6.6, 6.7, 5.8, 4.5)

ng <- SharedData$new(ng, key = ~Name)

bar <- ggplot(ng, aes(x = reorder(Name, TFR), y = TFR, fill = TFR)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  scale_fill_viridis(guide = FALSE) + 
  labs(x = "", 
       y = "Total fertility rate, 2013. Source: USAID Demographic & Health Surveys.", 
       title = "States in Nigeria") 

map <- ggplot(ng, aes(fill = TFR)) + 
  geom_sf() + 
  coord_equal() + 
  scale_fill_viridis() 

barly <- ggplotly(bar, tooltip = NA, height = 600) %>%
  highlight("plotly_click", color = "red")

maply <- ggplotly(map, tooltip = "Name", height = 600) %>%
  highlight("plotly_click", color = "red")

bscols(barly, maply)

