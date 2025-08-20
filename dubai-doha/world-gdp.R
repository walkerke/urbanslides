# Install and load required packages if needed
# install.packages(c("WDI", "mapgl", "rnaturalearth", "rnaturalearthdata", "dplyr", "RColorBrewer"))
# If you need high-resolution maps, also install:
# devtools::install_github("ropensci/rnaturalearthhires")

library(WDI)
library(mapgl)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(RColorBrewer)
library(sf)

# 1. Get the most recent GDP per capita data using the WDI package
# NY.GDP.PCAP.PP.CD is the indicator for GDP per capita, PPP (current international $)

# Refresh the WDI cache to get the latest data
new_cache <- WDIcache()

# Get the GDP per capita data for all countries (most recent available)
gdp_data <- WDI(
  indicator = "NY.GDP.PCAP.PP.CD",
  country = "all",
  start = 2023,  # Adjust as needed for most recent available data
  end = 2023,    # Adjust as needed for most recent available data
  cache = new_cache
)

# 2. Get the world map data from Natural Earth
# Use the "medium" scale for a world map (1:50m)
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# 3. Join the GDP data with the world map spatial data
# The join key is iso_a2 in the spatial data and iso2c in the WDI data
world_gdp <- world_map %>%
  left_join(gdp_data, by = c("iso_a2" = "iso2c"))

# 4. Calculate country rankings based on GDP per capita
# First remove NA values for calculating ranks
world_gdp_with_data <- world_gdp %>% 
  filter(!is.na(NY.GDP.PCAP.PP.CD))

# Calculate ranks (1 is highest GDP per capita)
world_gdp_with_data$rank <- rank(-world_gdp_with_data$NY.GDP.PCAP.PP.CD)

# Merge ranks back into the main dataset
world_gdp <- world_gdp %>%
  left_join(st_drop_geometry(world_gdp_with_data) %>% select(iso_a2, rank), by = "iso_a2")

# 5. Create a color palette for the map with 7 categories
gdp_palette <- colorRampPalette(brewer.pal(11, "RdYlBu")[11:1])(7)

# Use quantile breaks instead of equal intervals
# First, get the values without NAs
gdp_values <- world_gdp$NY.GDP.PCAP.PP.CD[!is.na(world_gdp$NY.GDP.PCAP.PP.CD)]

# Calculate 7 quantile breaks
gdp_breaks <- quantile(gdp_values, probs = seq(0, 1, length.out = 8))

# 6. Create tooltip and popup information for interactive features
world_gdp$tooltip <- paste0(world_gdp$name, ": $", 
                            format(round(world_gdp$NY.GDP.PCAP.PP.CD, 0), big.mark=","))

world_gdp$popup <- paste0(
  "<strong>", world_gdp$name, "</strong><br>",
  "GDP per capita (PPP): $", format(round(world_gdp$NY.GDP.PCAP.PP.CD, 0), big.mark=","), "<br>",
  "Global Rank: ", world_gdp$rank, " of ", max(world_gdp$rank, na.rm = TRUE), "<br>",
  "Year: ", world_gdp$year
)

# 7. Create the legend labels
legend_labels <- c(
  paste0("< $", format(round(gdp_breaks[2], 0), big.mark=",")),
  paste0("$", format(round(gdp_breaks[2], 0), big.mark=","), " - $", format(round(gdp_breaks[3], 0), big.mark=",")),
  paste0("$", format(round(gdp_breaks[3], 0), big.mark=","), " - $", format(round(gdp_breaks[4], 0), big.mark=",")),
  paste0("$", format(round(gdp_breaks[4], 0), big.mark=","), " - $", format(round(gdp_breaks[5], 0), big.mark=",")),
  paste0("$", format(round(gdp_breaks[5], 0), big.mark=","), " - $", format(round(gdp_breaks[6], 0), big.mark=",")),
  paste0("$", format(round(gdp_breaks[6], 0), big.mark=","), " - $", format(round(gdp_breaks[7], 0), big.mark=",")),
  paste0("> $", format(round(gdp_breaks[7], 0), big.mark=","))
)

# 8. Create the map using mapboxgl with Winkel Tripel projection
gdp_map <- mapboxgl(
  style = mapbox_style("light"),
  projection = "winkelTripel"  # Non-mercator projection
) %>%
  add_fill_layer(
    id = "countries",
    source = world_gdp,
    fill_color = step_expr(
      column = "NY.GDP.PCAP.PP.CD",
      base = gdp_palette[1],
      stops = gdp_palette[2:7],
      values = unname(gdp_breaks[2:7]),
      na_color = "lightgrey"
    ),
    fill_opacity = 0.7,
    popup = "popup",
    tooltip = "tooltip",
    hover_options = list(
      fill_color = "cyan"
    )
  ) %>%
  add_legend(
    legend_title = "GDP per capita (PPP)",
    values = legend_labels,
    colors = gdp_palette,
    type = "categorical"
  )

# Display the map
gdp_map