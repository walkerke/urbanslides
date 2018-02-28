library(sf)
library(tidyverse)
library(extrafont)
library(tigris)
library(magick)
options(tigris_use_cache = TRUE)

ctys <- c("1630", "1250", "0990", "0125")

# 1940

t40 <- st_read("data-raw/nhgis0093_shape/US_tract_1940.shp") %>%
  filter(NHGISST == "260", NHGISCTY %in% ctys)

d40 <- read_csv("data-raw/nhgis0093_csv/nhgis0093_csv/nhgis0093_ds76_1940_tract.csv")

df40 <- t40 %>%
  inner_join(d40, by = "GISJOIN") %>%
  mutate(Other = BUQ002 - BVG001) %>%
  select(GISJOIN, White = BUQ001, Black = BVG001, Other) %>%
  mutate(year = "1940")

# 1950

t50 <- st_read("data-raw/nhgis0093_shape/US_tract_1950.shp", 
               stringsAsFactors = FALSE) %>%
  filter(NHGISST == "260", NHGISCTY %in% ctys)

d50 <- read_csv("data-raw/nhgis0093_csv/nhgis0093_csv/nhgis0093_ds82_1950_tract.csv")

df50 <- t50 %>%
  inner_join(d50, by = "GISJOIN") %>%
  select(GISJOIN, White = B0J001, Black = B0J002, Other = B0J003) %>%
  mutate(year = "1950")

# 1960

t60 <- st_read("data-raw/nhgis0093_shape/US_tract_1960.shp") %>%
  filter(NHGISST == "260", NHGISCTY %in% ctys)

d60 <- read_csv("data-raw/nhgis0093_csv/nhgis0093_csv/nhgis0093_ds92_1960_tract.csv")

df60 <- t60 %>%
  inner_join(d60, by = "GISJOIN") %>%
  select(GISJOIN, White = B7B001, Black = B7B002, Other = B7B003) %>%
  mutate(year = "1960")

df60 <- na.omit(df60)

# 1970

t70 <- st_read("data-raw/nhgis0093_shape/US_tract_1970.shp") %>%
  filter(NHGISST == "260", NHGISCTY %in% ctys)

d70 <- read_csv("data-raw/nhgis0093_csv/nhgis0093_csv/nhgis0093_ds95_1970_tract.csv")

df70 <- t70 %>%
  inner_join(d70, by = "GISJOIN") %>%
  mutate(White = CEB001 + CEB010, 
         Black = CEB002 + CEB011, 
         Other = CEB003 + CEB004 + CEB005 + CEB006 + CEB007 + CEB008 + CEB009 + CEB012 + 
           CEB013 + CEB014 + CEB015 + CEB016 + CEB017 + CEB018) %>%
  select(GISJOIN, White, Black, Other) %>%
  mutate(year = "1970")

# 1980

t80 <- st_read("data-raw/nhgis0093_shape/US_tract_1980.shp") %>%
  filter(NHGISST == "260", NHGISCTY %in% ctys)

d80 <- read_csv("data-raw/nhgis0093_csv/nhgis0093_csv/nhgis0093_ds104_1980_tract.csv")

df80 <- t80 %>%
  inner_join(d80, by = "GISJOIN") %>%
  mutate(Other = C9D003 + C9D004 + C9D005 + C9D006 + C9D007 + C9D008 + C9D009 + C9D010 + 
           C9D011 + C9D012 + C9D013 + C9D014 + C9D015) %>%
  select(GISJOIN, White = C9D001, Black = C9D002, Other) %>%
  mutate(year = "1980")
    
detroit <- places("MI", cb = TRUE, class = "sf") %>%
  filter(NAME == "Detroit") %>%
  st_transform(26917)

years <- list(df40, df50, df60, df70, df80)

walk(years, function(y) {
  
  dots <- map(c("White", "Black", "Other"), function(x) {
    
    y %>%
      st_transform(26917) %>%
      mutate(group = as.integer(y[[x]] / 200)) %>%
      st_sample(., .$group) %>%
      st_sf() %>%
      mutate(race = x)
    
  }) %>%
    reduce(rbind) %>%
    st_transform(26917)
  
  dots <- dots[sample(nrow(dots)), ]
  
  dots <- set_names(dots, c("race", "geometry"))
  
  st_geometry(dots) <- "geometry"
  
  b <- st_bbox(detroit)
  
  g1 <- ggplot() +
    geom_sf(data = dots, aes(color = race, fill = race), shape = ".") + 
    geom_sf(data = detroit, fill = NA, color = "white") + 
    coord_sf(xlim = c(b[1], b[3]), ylim = c(b[2], b[4])) + 
    scale_color_manual(values = c("White" = "magenta", "Black" = "cyan", "Other" = "green")) + 
    scale_fill_manual(values = c("White" = "magenta", "Black" = "cyan", "Other" = "green")) + 
    theme_void(base_family = "Verdana") + 
    theme(plot.background = element_rect(fill = "black"),
          panel.background = element_rect(fill = "black"), 
          legend.position = "bottom", 
          legend.background = element_rect(fill = "black"), 
          legend.title = element_blank(), 
          text = element_text(color = "white"), 
          panel.grid.major = element_blank(), 
          plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm"), 
          plot.caption = element_text(size = 8)) + 
    labs(title = paste0("Race in the Detroit area, ", unique(y$year)), 
         subtitle = "1 dot = approximately 200 people", 
         caption = "Data source: US Census via NHGIS. White border: city of Detroit.") 
  
  ggsave(paste0("detroit", unique(y$year), ".png"), g1, dpi = 300, width = 8, height = 6.5)
  
})

g <- image_morph(
  c(image_read("detroit1940.png"), 
    image_read("detroit1950.png"), 
    image_read("detroit1960.png"), 
    image_read("detroit1970.png"),
    image_read("detroit1980.png")), 
  frames = 10
)

image_animate(g, fps = 5)

image_animate(c(image_read("detroit1940.png"), 
              image_read("detroit1950.png"), 
              image_read("detroit1960.png"), 
              image_read("detroit1970.png"),
              image_read("detroit1980.png")))

i40 <- image_read("detroit1940.png")



# url <- "https://api.mapbox.com/styles/v1/kwalkertcu/cj0jov12u007n2sqppuxe3fvr/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJMRk9JSmRvIn0.l1y2jHZ6IARHM_rA1-X45A"
# 
# osm <- read_osm(bb(st_transform(t, 4326)), type = url)
# 
# m1 <- tm_shape(osm) + 
#   tm_raster() + 
#   tm_shape(dots) + 
#   tm_dots(col = "race", palette = c("blue", "green"), 
#           title = "Race in Detroit, 19") + 
#   tm_layout(legend.position = c("right", "bottom"), fontfamily = "Verdana")
# 
# 
# 
# t50 <- st_read("data-raw/nhgis0093_shape/US_tract_1950.shp") %>%
#   filter(NHGISST == "260", NHGISCTY %in% ctys)