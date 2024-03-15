#### Plot sites and climate regions #### 

#Libraries
library(sf)
library(ggplot2)
library(rnaturalearth)

#Data

head(m_count)

head(m_clim)

#Merge

rclim_cord <- merge(m_coord, m_clim, by.x = c("bms_id", "transect_id"), by.y = c("bms_id", "SITE_ID"))

head(rclim_cord)

# Convert data frame to an sf object
rclim_cord_sf <- st_as_sf(rclim_cord, coords = c("transect_lon", "transect_lat"), crs = 3035)

# Get European country boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")

marked_cold_to_warm_palette <- c(
  "#5e4fa2", # Deep indigo, cold
  "#3288bd", # Bright blue
  "#66c2a5", # Aqua green
  "#abdda4", # Light green
  "#e6f598", # Light yellow-green
  "#fee08b", # Yellow
  "#fdae61", # Orange
  "#f46d43", # Reddish orange
  "#d53e4f", # Crimson red
  "#9e0142"  # Deep burgundy, warm
)

# Define the geographic coordinates for 20W and 25E (in degrees)
longitudes <- data.frame(lon = c(-20, 25), lat = c(20, 50)) # lat is arbitrary here

# Convert these points to sf objects using WGS 84 (EPSG:4326)
points_sf <- st_as_sf(longitudes, coords = c("lon", "lat"), crs = 4326)

# Transform these points to EPSG:3035
points_transformed <- st_transform(points_sf, crs = 3035)

# Extract the x coordinates after transformation
xlims_transformed <- st_coordinates(points_transformed)[,1]

#Plot the map
plot <-ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white") + # Draw countries
  geom_sf(data = rclim_cord_sf, aes(color = RCLIM), size = 2) + # Draw transect points
  scale_color_manual(values = marked_cold_to_warm_palette) + # Use the custom palette
  theme_minimal() +
  labs(color = "Climate regions") +
  coord_sf(crs = st_crs(3035), xlim = xlims_transformed, ylim = c(1000000, 5100000))

regions
