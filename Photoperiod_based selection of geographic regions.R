library(sf)

# --- Latitudinal range of the whole dataset --- #

# Remove rows where either transect_lon or transect_lat is NA
m_coord_clean <- m_coord[!is.na(m_coord$transect_lon) & !is.na(m_coord$transect_lat), ]

# Now convert to an sf object
m_coord_sf <- st_as_sf(m_coord_clean, coords = c("transect_lon", "transect_lat"), crs = 3035, agr = "constant")

# Transform the coordinate reference system to WGS84 (EPSG:4326)
m_coord_transformed <- st_transform(m_coord_sf, 4326)

# Extract coordinates
coords <- st_coordinates(m_coord_transformed)

# Now, find the minimum and maximum latitude values
min_lat_geo <- min(coords[,2]) # Latitude values are in the second column
max_lat_geo <- max(coords[,2])

# Print the results
cat("Minimum Latitude:", min_lat_geo, "\n")
cat("Maximum Latitude:", max_lat_geo, "\n")

# --- Latitudinal range of each bms_id --- #

# Ensure dplyr is installed and loaded

library(dplyr)

# First, extract the latitude as a new column
m_coord_transformed <- m_coord_transformed %>%
  mutate(latitude = st_coordinates(geometry)[, 2])

# Now, calculate the latitudinal range for each bms_id
latitudinal_range <- m_coord_transformed %>%
  group_by(bms_id) %>%
  summarise(
    min_latitude = min(latitude, na.rm = TRUE),
    max_latitude = max(latitude, na.rm = TRUE)
  )

# View the results
print(latitudinal_range)


# --- Create geographic regions --- # 

min_latitude <- 28.07431
max_latitude <- 67.47644
region_size <- 10

# Create the breaks for the regions
region_breaks <- seq(from = min_latitude, to = max_latitude, by = region_size)

# If the max latitude doesn't align perfectly with the breaks, ensure the last region includes the max latitude
if(max(region_breaks) < max_latitude) {
  region_breaks <- c(region_breaks, max_latitude)
}

# Assuming m_coord_transformed already has a latitude column from previous steps
m_coord_transformed$region <- cut(m_coord_transformed$latitude, breaks = region_breaks, include.lowest = TRUE, labels = FALSE)

# Optionally, to create more descriptive labels for each region based on the range they cover
region_labels <- paste(head(region_breaks, -1), tail(region_breaks, -1), sep = "-")
m_coord_transformed$region_label <- cut(m_coord_transformed$latitude, breaks = region_breaks, include.lowest = TRUE, labels = region_labels)

# Plot geographic regions

library(ggplot2)
library(rnaturalearth)

world <- ne_countries(scale = "medium", returnclass = "sf")
europe_map <- ne_countries(scale = "medium", continent = "europe", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") + # Plot Europe map
  geom_sf(data = m_coord_transformed, aes(color = as.factor(region_label)), size = 2, alpha = 0.6) + # Plot your data
  scale_color_viridis_d() + # Use a color scale
  labs(color = "Region") + # Color legend label
  theme_minimal() + # Use a minimal theme
  ggtitle("Geographic Regions in Europe") + # Title
  coord_sf(xlim = c(-20, 35), ylim = c(25, 70), expand = FALSE) # Set geographic limits

# --- how photoperiod varies with latitude? --- #

library(suncalc)

# Save the current locale
original_locale <- Sys.getlocale("LC_TIME")

# Set the locale to English
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Define a sequence of latitudes from -60 to 60 degrees
latitudes <- seq(28, 68, by = 1)

# Define dates for the 1st of March through September
dates <- as.Date(c("2024-03-01", "2024-04-01", "2024-05-01", "2024-06-01",
                   "2024-07-01", "2024-08-01", "2024-09-01"))

# Prepare a plot
plot(NULL, xlim = c(25, 75), ylim = c(5, 24), xlab = "Latitude", ylab = "Day Length (hours)",
     main = "Day Length vs. Latitude")

# Colors for each month
colors <- rainbow(length(dates))

# Loop over each date
for (i in seq_along(dates)) {
  # Calculate day length for each latitude for the current date
  day_lengths <- sapply(latitudes, function(lat) {
    sun_times <- getSunlightTimes(dates[i], lat, 0)  # Using longitude 0 for simplicity
    day_length <- as.numeric(difftime(sun_times$sunset, sun_times$sunrise, units = "hours"))
    return(day_length)
  })
  
  # Add a line to the plot for this month
  lines(latitudes, day_lengths, col = colors[i], lwd = 2)
}

# Add a legend
legend("topright", legend = format(dates, "%B"), col = colors, lwd = 2)


# --- Establishing photoperiod-based geographic region --- #

library(suncalc)

# Function to calculate day length based on latitude and date
calculate_day_length <- function(lat, date) {
  sun_times <- getSunlightTimes(date, lat, 0)  # Longitude 0, adjust as needed
  if (is.na(sun_times$sunset) || is.na(sun_times$sunrise)) {
    # Handle the NA values, perhaps by setting to 0 or 24 depending on the context
    return(NA)  # or an appropriate value for your use case
  }
  day_length <- as.numeric(difftime(sun_times$sunset, sun_times$sunrise, units = "hours"))
  return(day_length)
}

# Calculate the day length at ymin and ymax
day_length_ymin <- calculate_day_length(min_latitude, date)
day_length_ymax <- 24

# Total day-length difference
total_day_length_diff <- abs(day_length_ymax - day_length_ymin)

# Decide on the day-length difference for each region
desired_day_length_diff_per_region <- total_day_length_diff / 6  # For example, if you want around 7 regions

# Initialize regions data frame
regions <- data.frame(start_latitude = numeric(), end_latitude = numeric(), day_length_diff = numeric())

current_latitude <- min_latitude
start_latitude <- current_latitude
initial_day_length <- day_length_ymin

while(current_latitude <= max_latitude + 0.01) {  # Allow the loop to run one last time if we're just past the max
  # Calculate the current day length or set to NA if the sun doesn't set
  current_day_length <- calculate_day_length(current_latitude, date)
  if (is.na(current_day_length)) {
    # If day length is NA, we assume the sun doesn't set (24 hours of daylight)
    current_day_length <- 24
  }
  
  # Adjust logic for appending the new region to handle final region correctly
  if (abs(current_day_length - initial_day_length) >= desired_day_length_diff_per_region || current_latitude + 0.01 > max_latitude) {
    # Ensure we don't exceed max_latitude for the final region
    end_latitude = min(current_latitude, max_latitude)
    regions <- rbind(regions, data.frame(
      start_latitude = start_latitude,
      end_latitude = end_latitude,
      day_length_diff = abs(current_day_length - initial_day_length)
    ))
    
    start_latitude <- current_latitude
    initial_day_length <- current_day_length
  }
  
  current_latitude <- current_latitude + 0.01
  
}


# Format the region label with names and rounded latitude values
regions$region_label <- sprintf("R%d (%.2f-%.2f)", 
                                seq_along(regions$start_latitude), 
                                regions$start_latitude, 
                                regions$end_latitude)

# Updating m_coord_transformed with the correct region_label
m_coord_transformed <- m_coord_transformed %>%
  rowwise() %>%
  mutate(region_label = {
    lat <- latitude  # Extracting the latitude value
    # Find the corresponding region label
    label <- regions$region_label[lat >= regions$start_latitude & lat < regions$end_latitude]
    if(length(label) == 0) NA_character_ else label
  }) %>%
  ungroup()

# Filter out features with NA region labels
m_coord_transformed_filtered <- m_coord_transformed %>%
  filter(!is.na(region_label))


#Plot map
ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") + # Plot Europe map
  geom_sf(data = m_coord_transformed_filtered, aes(color = as.factor(region_label)), size = 2, alpha = 0.6) + # Plot your data
  scale_color_viridis_d() + # Use a color scale that fits your data
  labs(color = "Region (latitudinal range)") + # Label for the color legend
  theme_minimal() + # Use a minimal theme for aesthetics
  ggtitle("Photoperiod-based geographic regions (using photoperiod in summer solstice)") + 
  coord_sf(xlim = c(-20, 35), ylim = c(25, 70), expand = FALSE) # Focus on Europe with set limits



# --- Plot climate regions with latitudinal boundaries --- #

rclim_cord <- merge(m_coord, m_clim, by.x = c("bms_id", "transect_id"), by.y = c("bms_id", "SITE_ID"))

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

# Make sure rclim_cord_sf is in EPSG:3035
rclim_cord_sf <- st_transform(rclim_cord_sf, crs = 3035)

# Transform rclim_cord_sf to geographic coordinates (EPSG:4326)
rclim_cord_sf_geo <- st_transform(rclim_cord_sf, crs = 4326)

# Create a vector of latitudes representing the boundaries of the regions in geographic coordinates
region_boundaries <- c(regions$start_latitude, tail(regions$end_latitude, n = 1))

# Plot the map with rclim_cord_sf in geographic coordinates
ggplot() +
  geom_sf(data = europe, fill = "lightgrey", color = "white") +
  geom_sf(data = rclim_cord_sf_geo, aes(color = RCLIM), size = 1) +
  scale_color_manual(values = marked_cold_to_warm_palette) +
  theme_minimal() +
  labs(color = "Climate regions") +
  coord_sf(xlim = c(-20, 35), ylim = c(25, 70), expand = FALSE) +
  geom_hline(yintercept = region_boundaries, color = "black", linetype = "dashed") +
  ggtitle("Climate regions with latitudinal boudaries. 1.68 day length diff")


