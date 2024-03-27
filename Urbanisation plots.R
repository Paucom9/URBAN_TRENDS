# Clean environment and set the working directory to the location of the data files
rm(list = ls())  # Remove all objects from the current R session to ensure a clean working environment
setwd("D:/URBAN TRENDS/Urbanisation data") 

# Libraries required
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(gganimate)
library(tidyr)
library(transformr)
library(patchwork)


# Total built-up surface
built_ebms <- read.csv("embs_ubms_GHS_BUILT_stats.csv", sep = ";", dec = ".")
built_ubms <- read.csv("ubms_sites_GHS_BUILT.csv", sep = ";", dec = ".")

# Rename columns
built_ebms <- built_ebms %>%
  rename(
    transect_id = transect_i,
    longitude = geometry,
    latitude = geom1
  )

built_ubms <- built_ubms %>%
  rename(
    transect_id = transect_i,
    longitude = transect_1,
    latitude = transect_2
  )

built_ubms <- built_ubms %>%
  select(-transect_l)  # Remove the 'transect_l' column

# Convert the dataframe to a spatial dataframe
coordinates <- st_as_sf(built_ubms, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Transform coordinates to EPSG:3035
coordinates_transformed <- st_transform(coordinates, 3035)

# Extract the transformed coordinates back into the dataframe
built_ubms_transformed <- as.data.frame(coordinates_transformed)
built_ubms_transformed$longitude <- st_coordinates(coordinates_transformed)[, 1]
built_ubms_transformed$latitude <- st_coordinates(coordinates_transformed)[, 2]

built_ubms_transformed <- built_ubms_transformed %>%
  select(-geometry) %>% # Remove the geometry column if it exists
  mutate(bms_id = "ES_uBMS") %>% # Add the bms_id column with all values set to "ES_uBMS"
  select(bms_id, longitude, latitude, everything()) # Reorder columns

# rbind ebms and ubms files

built_df <- rbind(built_ebms,built_ubms_transformed)


# Population density
pop_ebms <- read.csv("embs_ubms_GHS_POP_stats.csv", sep = ";", dec = ".")
pop_ubms <- read.csv("ubms_sites_GHS_POP.csv", sep = ";", dec = ".")

# Rename columns
pop_ebms <- pop_ebms %>%
  rename(
    transect_id = transect_i,
    longitude = geometry,
    latitude = geom1
  )

pop_ubms <- pop_ubms %>%
  rename(
    transect_id = transect_i,
    longitude = transect_1,
    latitude = transect_2
  )

pop_ubms <- pop_ubms %>%
  select(-transect_l)  # Remove the 'transect_l' column

# Convert the dataframe to a spatial dataframe
coordinates <- st_as_sf(pop_ubms, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Transform coordinates to EPSG:3035
coordinates_transformed <- st_transform(coordinates, 3035)

# Extract the transformed coordinates back into the dataframe
pop_ubms_transformed <- as.data.frame(coordinates_transformed)
pop_ubms_transformed$longitude <- st_coordinates(coordinates_transformed)[, 1]
pop_ubms_transformed$latitude <- st_coordinates(coordinates_transformed)[, 2]

pop_ubms_transformed <- pop_ubms_transformed %>%
  select(-geometry) %>% # Remove the geometry column if it exists
  mutate(bms_id = "ES_uBMS") %>% # Add the bms_id column with all values set to "ES_uBMS"
  select(bms_id, longitude, latitude, everything()) # Reorder columns

# rbind ebms and ubms files
pop_df <- rbind(pop_ebms,pop_ubms_transformed)

# Degree of urbanisation (categorical)
mod_ebms <- read.csv("embs_ubms_GHS_MOD_stats.csv", sep = ";", dec = ".")
mod_ubms <- read.csv("ubms_sites_GHS_MOD.csv", sep = ";", dec = ".")

# Rename columns
mod_ebms <- mod_ebms %>%
  rename(
    transect_id = transect_i,
    longitude = geometry,
    latitude = geom1
  )

mod_ubms <- mod_ubms %>%
  rename(
    transect_id = transect_i,
    longitude = transect_1,
    latitude = transect_2
  )

mod_ubms <- mod_ubms %>%
  select(-transect_l)  # Remove the 'transect_l' column

# Convert the dataframe to a spatial dataframe
coordinates <- st_as_sf(mod_ubms, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Transform coordinates to EPSG:3035
coordinates_transformed <- st_transform(coordinates, 3035)

# Extract the transformed coordinates back into the dataframe
mod_ubms_transformed <- as.data.frame(coordinates_transformed)
mod_ubms_transformed$longitude <- st_coordinates(coordinates_transformed)[, 1]
mod_ubms_transformed$latitude <- st_coordinates(coordinates_transformed)[, 2]

mod_ubms_transformed <- mod_ubms_transformed %>%
  select(-geometry) %>% # Remove the geometry column if it exists
  mutate(bms_id = "ES_uBMS") %>% # Add the bms_id column with all values set to "ES_uBMS"
  select(bms_id, longitude, latitude, everything()) # Reorder columns

# rbind ebms and ubms files
mod_df <- rbind(mod_ebms,mod_ubms_transformed)

# --- Inspect degree of urbanisation data (mod_df) --- #

# Define the replacement logic
replace_values <- function(x) {
  case_when(
    x == 30 ~ "URBAN CENTRE",
    x == 23 ~ "DENSE URBAN CLUSTER",
    x == 22 ~ "SEMI-DENSE URBAN CLUSTER",
    x == 21 ~ "SUBURBAN OR PERI-URBAN",
    x == 13 ~ "RURAL CLUSTER GRID",
    x == 12 ~ "LOW DENSITY RURAL",
    x == 11 ~ "VERY LOW DENSITY RURAL",
    x == 10 ~ "WATER",
    TRUE ~ as.character(x) # Keep original value if none of the above conditions are met
  )
}

# Apply the replacement function to columns 5 to 15
mod_df <- mod_df %>%
  mutate(across(.cols = 5:15, .fns = replace_values))

#Check NA values
total_na <- sum(is.na(mod_df))
total_na

na_per_column <- colSums(is.na(mod_df))
na_per_column

mod_df <- na.omit(mod_df)

# Create a named vector for the palette with a gradient from red to green and blue for water
urban_rural_palette <- c(
  "URBAN CENTRE" = "#FF0000",                   # Bright red for the most urban
  "DENSE URBAN CLUSTER" = "#E67300",            # Less bright red/orange
  "SEMI-DENSE URBAN CLUSTER" = "#CC9900",       # Orange/yellow
  "SUBURBAN OR PERI-URBAN" = "#99CC00",         # Lighter green
  "RURAL CLUSTER" = "#66FF66",        # Green
  "LOW DENSITY RURAL" = "#33CC33",    # Darker green
  "VERY LOW DENSITY RURAL" = "#009900", # Very dark green
  "WATER" = "#0000FF"                 # Blue for water
)

# Convert these points to sf objects using projection EPSG:3035)
transects_sf <- st_as_sf(mod_df, coords = c("longitude", "latitude"), crs = 3035)

# Define the levels in order from most urban to most rural
ordered_levels <- c(
  "URBAN CENTRE",
  "DENSE URBAN CLUSTER",
  "SEMI-DENSE URBAN CLUSTER",
  "SUBURBAN OR PERI-URBAN",
  "RURAL CLUSTER",
  "LOW DENSITY RURAL",
  "VERY LOW DENSITY RURAL",
  "WATER"
)

# Convert each 'point_XXXX' column to an ordered factor
transects_sf$point_1975 <- factor(transects_sf$point_1975, levels = ordered_levels, ordered = TRUE)
transects_sf$point_1980 <- factor(transects_sf$point_1980, levels = ordered_levels, ordered = TRUE)
transects_sf$point_1985 <- factor(transects_sf$point_1985, levels = ordered_levels, ordered = TRUE)
transects_sf$point_1990 <- factor(transects_sf$point_1990, levels = ordered_levels, ordered = TRUE)
transects_sf$point_1995 <- factor(transects_sf$point_1995, levels = ordered_levels, ordered = TRUE)
transects_sf$point_2000 <- factor(transects_sf$point_2000, levels = ordered_levels, ordered = TRUE)
transects_sf$point_2005 <- factor(transects_sf$point_2005, levels = ordered_levels, ordered = TRUE)
transects_sf$point_2010 <- factor(transects_sf$point_2010, levels = ordered_levels, ordered = TRUE)
transects_sf$point_2015 <- factor(transects_sf$point_2015, levels = ordered_levels, ordered = TRUE)
transects_sf$point_2020 <- factor(transects_sf$point_2020, levels = ordered_levels, ordered = TRUE)
transects_sf$point_2025 <- factor(transects_sf$point_2025, levels = ordered_levels, ordered = TRUE)


# Get European country boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")
# Transform the CRS to EPSG:3035
world <- st_transform(world, crs = 3035)

#Plot the map
plot_1975 <-ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white") + # Draw countries
  geom_sf(data = transects_sf, aes(color = point_1975), size = 1) + # Draw transect points
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    plot.title = element_text(size = 20, hjust = 0.5) # Increase title size and center
  ) +
  labs(title = "1975") + # Add plot title and legend title
  coord_sf(crs = st_crs(3035), xlim = c(1602962, 5366783), ylim = c(1000000, 5100000)) +
scale_color_manual(values = urban_rural_palette)  # Use the custom palette

plot_2020 <-ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white") + # Draw countries
  geom_sf(data = transects_sf, aes(color = point_2020), size = 1) + # Draw transect points
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5) # Increase title size and center
  ) +
  labs(title = "2020", color = "Degree of urbanisation") +
  coord_sf(crs = st_crs(3035), xlim = c(1602962, 5366783), ylim = c(1000000, 5100000)) +
  scale_color_manual(values = urban_rural_palette)  # Use the custom palette
  
combined_plot <- plot_1975 + plot_2020 + plot_layout(ncol = 2)
combined_plot



# --- Inspect population data --- #

# Remove NA values
pop_df <- na.omit(pop_df)

# Convert these points to sf objects using projection EPSG:3035)
transects_sf <- st_as_sf(pop_df, coords = c("longitude", "latitude"), crs = 3035)

# Get European country boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")
# Transform the CRS to EPSG:3035
world <- st_transform(world, crs = 3035)

# Adjusted plot for continuous values
plot_1975 <- ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white") + # Draw countries
  geom_sf(data = transects_sf, aes(color = sum_2000m_1975), size = 1) + # Color by point_1975
  scale_color_gradientn(colors = c("green", "yellow", "red"),
                        values = scales::rescale(c(0, 0.01, 0.5, 1))) + # Define a continuous color scale  labs(
  labs(  title = "1975",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5), # Increase and center title size
    legend.position = "none" # Remove legend
  ) +
  coord_sf(crs = st_crs(3035), xlim = c(1602962, 5366783), ylim = c(1000000, 5100000))

plot_2020 <- ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white") + # Draw countries
  geom_sf(data = transects_sf, aes(color = sum_2000m_2020), size = 1) + # Color by point_1975
  scale_color_gradientn(colors = c("green", "yellow", "red"),
                        values = scales::rescale(c(0, 0.01, 0.5, 1))) + # Define a continuous color scale  labs(
  labs(  title = "2020",
    color = "Number of people"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5), # Increase and center title size
  ) +
  coord_sf(crs = st_crs(3035), xlim = c(1602962, 5366783), ylim = c(1000000, 5100000))

combined_plot <- plot_1975 + plot_2020 + plot_layout(ncol = 2)
combined_plot <- combined_plot + plot_annotation(title = "2000x2000m", 
                                                 theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
combined_plot


# --- Inspect built data --- #

# Remove NA values
built_df <- na.omit(built_df)

# Convert these points to sf objects using projection EPSG:3035)
transects_sf <- st_as_sf(built_df, coords = c("longitude", "latitude"), crs = 3035)

# Get European country boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")
# Transform the CRS to EPSG:3035
world <- st_transform(world, crs = 3035)

# Adjusted plot for continuous values
plot_1975 <- ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white") + # Draw countries
  geom_sf(data = transects_sf, aes(color = point_1975), size = 1) + # Color by point_1975
  scale_color_gradientn(colors = c("green", "yellow", "red"),
                        values = scales::rescale(c(0, 0.05, 0.5, 1))) + # Define a continuous color scale
  labs(
    title = "1975"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5), # Increase and center title size
    legend.position = "none" # Remove legend
  ) +
  coord_sf(crs = st_crs(3035), xlim = c(1602962, 5366783), ylim = c(1000000, 5100000))


plot_2020 <- ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white") + # Draw countries
  geom_sf(data = transects_sf, aes(color = point_2020), size = 1) + # Color by sum_2000m_2020
  scale_color_gradientn(colors = c("green", "yellow", "red"),
                        values = scales::rescale(c(0, 0.05, 0.5, 1))) + # Define a continuous color scale
  labs(
    title = "2020",
    color = expression("Total built-up surface (m"^2*")")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5) # Increase and center title size
  ) +
  coord_sf(crs = st_crs(3035), xlim = c(1602962, 5366783), ylim = c(1000000, 5100000))


combined_plot <- plot_1975 + plot_2020 + plot_layout(ncol = 2)
combined_plot <- combined_plot + plot_annotation(title = "100x100m", 
                                                 theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
combined_plot



# --- Plot urbanisation trends --- #

# Create a mapping of transect_id to longitude and latitude
setDT(built_long)
lon_lat_mapping <- unique(built_long[, .(longitude, latitude), by = transect_id])

# Merge the mapping with regression results
regression_results_built_dt <- merge(regression_results_built_dt, lon_lat_mapping, by = "transect_id", all.x = TRUE)
regression_results_pop_dt <- merge(regression_results_pop_dt, lon_lat_mapping, by = "transect_id", all.x = TRUE)

# Convert these points to sf objects using projection EPSG:3035)
built_sf <- st_as_sf(regression_results_built_dt, coords = c("longitude", "latitude"), crs = 3035)
pop_sf <- st_as_sf(regression_results_pop_dt, coords = c("longitude", "latitude"), crs = 3035)




#Plot the maps

built_plot<- ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white") + # Draw countries
  geom_sf(data = subset(built_sf, variable == "point"), aes(color = estimate), size = 1) + # Filter and color by estimate
  scale_color_gradientn(colors = c("green", "yellow", "red"),
                        values = scales::rescale(c(0, 0.01, 0.5, 1))) + # Define a continuous color scale  labs(
  labs(title = "Built-up fraction (1975-2025 trend)",
       color = "Estimate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5), # Increase and center title size
  ) +
  coord_sf(crs = st_crs(3035), xlim = c(1602962, 5366783), ylim = c(1000000, 5100000))

pop_plot<- ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white") + # Draw countries
  geom_sf(data = subset(pop_sf, variable == "point"), aes(color = estimate), size = 1) + # Filter and color by estimate
  scale_color_gradientn(colors = c("green", "yellow", "red"),
                        values = scales::rescale(c(0, 0.01, 0.5, 1))) + # Define a continuous color scale  labs(
  labs(title = "Human population number (1975-2025 trend)",
       color = "Estimate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5), # Increase and center title size
  ) +
  coord_sf(crs = st_crs(3035), xlim = c(1602962, 5366783), ylim = c(1000000, 5100000))


combined_plot <- pop_plot + built_plot + plot_layout(ncol = 2)
combined_plot <- combined_plot + plot_annotation(title = "100x100m", 
                                                 theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
combined_plot
