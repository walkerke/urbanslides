library(idbr)
library(tidyverse)

ng <- idb5(country = "Nigeria", variables = "IMR", year = 1953:2018)

ggplot(ng, aes(x = time, y = IMR)) + 
  geom_line(color = "darkgreen") + 
  geom_point(color = "darkgreen") + 
  theme_minimal() + 
  labs(title = "Infant mortality rate in Nigeria", 
       subtitle = "Data source: US Census Bureau International Data Base", 
       y = "Infant mortality rate (deaths per 1000 live births)", 
       x = "") + 
  scale_x_continuous(breaks = seq(1960, 2020, 10))
  
