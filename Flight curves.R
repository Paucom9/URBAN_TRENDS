###### Flight curves and SINDEX calculation ######

# Clean environment and set the working directory to the location of the data files
rm(list = ls())  # Remove all objects from the current R session to ensure a clean working environment
setwd("D:/URBAN TRENDS/BMS data/BMS DATA 2024")  
setwd("/Users/SUITCASE/Documents/URBAN_TRENDS")

# Load required libraries
library(data.table)  # For efficient data handling
library(rbms)        # For butterfly monitoring data analysis
library(mgcv)        # For generalized additive models
library(dplyr)       # For data manipulation
library(tidyr)       # For data tidying
library(foreach)     # For looping constructs
library(broom)       # For converting statistical analysis objects into tidy data frames
library(stringr)     # For string manipulation
library(lubridate)   # For easy and intuitive work with dates and times
library(suncalc)     # For calculating photoperiod
library(sf)          # For managing spatial data
library(doParallel)  # For increasing loop performance


# >>> Run functions in FUNCTIONS.R  <<< #


# Data Import and Preparation

# eBMS data

# Import butterfly count data
ebms_count_df <- read.csv("ebms_count.csv", sep = ",", dec = ".")
# Import visit data
ebms_visit_df <- read.csv("ebms_visit.csv", sep = ",", dec = ".")
# Import climate region data
ebms_clim_df <- read.csv("ebms_transect_climate.csv", sep = ",", dec = ".")
# Import transect coordinates
ebms_coord_df <- read.csv("ebms_transect_coord.csv", sep = ",", dec = ".")
# Import country codes
country_codes <- read.csv("country_codes.csv", sep = ";", dec = ".")

# Extract bms_id from transect_id and select relevant columns
ebms_clim_df <- ebms_clim_df %>%
  mutate(bms_id = str_extract(transect_id, "^[^.]*")) %>%
  dplyr::select(bms_id, transect_id, genzname)

# uBMS data
# Import count data
ubms_count_df <- read.csv("output_count_table.csv", sep = ",", dec = ".")
# Assign a unique identifier to uBMS data
ubms_count_df$bms_id <- "ES-uBMS"
# Select and reorder columns to match the structure of eBMS count data
ubms_count_df <- ubms_count_df %>%
  dplyr::select(visit_id, bms_id, transect_id, visit_date, year, month, day, species_name, count)

# Import visit data and perform necessary transformations
ubms_visit_df <- read.csv("raw_ubms_ebms_visit.csv", sep = ",", dec = ".")
ubms_visit_df$bms_id <- "ES-uBMS"  # Assign the uBMS identifier

# Rename columns and calculate date components
ubms_visit_df <- ubms_visit_df %>%
  rename(visit_date = date_of_visit) %>%
  mutate(
    visit_date = ymd(visit_date),
    year = year(visit_date),
    month = month(visit_date),
    day = day(visit_date),
    week = week(visit_date),
    ebms_partner = TRUE
  )

# Adjust column order to match eBMS visit data structure
ubms_visit_df <- ubms_visit_df %>%
  dplyr::select(visit_id, bms_id, transect_id, visit_date, year, month, day, ebms_partner, week)

# Import transect cooordinates
ubms_coord <- read.csv("ubms_sites.csv", sep = ";", dec = ".")

ubms_coord <- ubms_coord %>%mutate(bms_id = "ES-uBMS")

# Filter out rows with NA in coordinates before transformation
ubms_coord_filtered <- ubms_coord %>%
  filter(!is.na(transect_longitude) & !is.na(transect_latitude))

# Convert to an sf object
ubms_coord_sf <- st_as_sf(ubms_coord_filtered, coords = c("transect_longitude", "transect_latitude"), crs = 4326, remove = FALSE)

# Transform coordinates to EPSG:3035
ubms_coord_transformed <- st_transform(ubms_coord_sf, crs = 3035)

ubms_coord_final <- data.frame(ubms_coord_transformed) %>%
  transmute(
    bms_id,
    transect_id,
    transect_length,
    transect_lon = st_coordinates(ubms_coord_transformed)[, 1],
    transect_lat = st_coordinates(ubms_coord_transformed)[, 2]
  )

## Concatenate rows

m_count_df <- rbind(ebms_count_df, ubms_count_df)
m_visit_df<- rbind(ebms_visit_df, ubms_visit_df)
m_clim_df<- ebms_clim_df
m_coord<- rbind(ebms_coord_df, ubms_coord_final)

## Transform data frames to data tables

m_count <- data.table(m_count_df)
m_visit <- data.table(m_visit_df)
m_clim <- data.table(m_clim_df)
dt_country_cod <- data.table(country_codes)

## Change column names

setnames(m_visit, c('transect_id', 'visit_date'), c('SITE_ID', 'DATE'))
setnames(m_count, c('transect_id', 'visit_date','species_name', 'count'),
         c('SITE_ID', 'DATE', 'SPECIES', 'COUNT'))
setnames(m_clim, c('transect_id', 'genzname'),
         c('SITE_ID', 'RCLIM'))

# Perform a left join to add RCLIM from m_clim to m_visit based on bms_id and SITE_ID
m_visit <- m_visit[m_clim, on = .(bms_id, SITE_ID), nomatch = 0]

## Perform a left join to merge m_clim into m_count

m_count <- left_join(m_count, m_clim, by = c("SITE_ID", "bms_id"))

# Merge m_count with dt_country_cod to include country_code
m_count <- merge(m_count, dt_country_cod, by = "bms_id", all.x = TRUE)

#NA values in RCLIM correspond to uBMS transects. All of them are part of climate region K
m_count$RCLIM <- replace(m_count$RCLIM, is.na(m_count$RCLIM), "K. Warm temperate and mesic")

# --- Establishing photoperiod-based geographic region --- #

# Calculate the day length at ymin and ymax
min_latitude <- 28.07431
max_latitude <- 67.47644
date <- as.Date("2024-06-20") # summer solstice in 2024
#library(growR) #added by YM, calculate_day_length not working in my lapop
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

regions$name_region <- paste0("R", seq_along(regions$start_latitude))

regions <- regions %>% filter(day_length_diff != 0)

# Remove rows with missing coordinates
m_coord_clean <- m_coord %>%
  filter(!is.na(transect_lon) & !is.na(transect_lat))

# Convert the cleaned data frame to an sf object
m_coord_sf <- st_as_sf(m_coord_clean, coords = c("transect_lon", "transect_lat"), crs = 3035)

# Transform the projection to EPSG:4326 to get geographic coordinates
m_coord_sf_transformed <- st_transform(m_coord_sf, crs = 4326)

# Extract the latitude and longitude from the geometry column of the transformed sf object
lat_lon <- st_coordinates(m_coord_sf_transformed)

# Add these coordinates back to the cleaned data frame
m_coord_clean$transect_lat_geo <- lat_lon[, 2] # Latitude
m_coord_clean$transect_lon_geo <- lat_lon[, 1] # Longitude

#Assigning geo_region to m_coord based on latitude range
# Apply the function to each row in m_coord to create the geo_region column
m_coord_clean$geo_region <- sapply(m_coord_clean$transect_lat_geo, find_region)

m_coord_clean <- merge(m_coord_clean, m_clim[, c("SITE_ID", "RCLIM")], by.x = "transect_id", by.y = "SITE_ID", all.x = TRUE)

m_coord_clean<- data.table(m_coord_clean)


# Joining m_count with m_coord_clean to include geo_region based on bms_id and the corresponding site/transect ID
m_count <- m_count %>%
  left_join(m_coord_clean %>% dplyr::select(bms_id, transect_id, geo_region), 
            by = c("bms_id" = "bms_id", "SITE_ID" = "transect_id"))

# Joining m_visit with m_coord_clean to include geo_region based on bms_id and the corresponding site/transect ID
m_visit <- m_visit %>%
  left_join(m_coord_clean %>% dplyr::select(bms_id, transect_id, geo_region), 
            by = c("bms_id" = "bms_id", "SITE_ID" = "transect_id"))








# --- Flight curves and SINDEX calculation --- #

# Ask to store the console output in a txt. Be aware with sink the output is no seen in the console. 
sink("outputfile.txt")

# Register the parallel backend to improve performance
registerDoParallel(cores = detectCores())

# Define the base path where you want to save the files
base_path <- "D:/URBAN TRENDS/sindex_results"
base_path <- "/Users/SUITCASE/Documents/URBAN_TRENDS/sindex_results"


# Iterate over each unique geographic region in the monitoring count data
for(region in unique(na.omit(m_count$geo_region))){
  
  # Filter monitoring count and visit data for the current latitudinal geographic region
  geocount_data <- m_count %>% filter(.data$geo_region == region)
  geovisit_data <- m_visit %>% filter(.data$geo_region == region)
  geocoord_data <- m_coord_clean %>% filter(.data$geo_region == region)
  
  # Inform about the processing status
  cat(sprintf("Processing latitudinal region %s: found %d unique species\n", region, length(unique(geocount_data$SPECIES))))
  
  # Identify unique climate regions for the current rclim 
  unique_rclim <- unique(na.omit(geocount_data$RCLIM))
  unique_rclim <- setdiff(unique_rclim, "") # Remove empty string levels, if any
  
  region_counter <- 1 # Track processed regions
  
  # Analyze data for each climate region
  for(rclim in unique(na.omit(geocount_data$RCLIM))){
    # Filter data for the current region
    rclimcount_data <- geocount_data %>% filter(RCLIM == rclim)
    rclimvisit_data <- geovisit_data %>% filter(RCLIM == rclim)
    rclimcoord_data <- geocoord_data %>% filter(RCLIM == rclim)
    
    #Function to calculate start and end months for monitoring season
    months_selection<- calculate_monitoring_season(rclimvisit_data)
    StartMonth <- months_selection$month_start 
    EndMonth <- months_selection$month_end 
    
    # Extract unique species within the region for analysis
    unique_species <- unique(rclimcount_data$SPECIES)
    cat(sprintf("Processing climate region %d/%d: %s\n", region_counter, length(unique_rclim), rclim))
    
    # Initialize species counter
    species_counter <- 1
    
    # Generate flight curves for each species in the region
    # Define the monitoring season and set up data for analysis
    ts_date <- rbms::ts_dwmy_table(InitYear = 1976, LastYear = 2021, WeekDay1 = 'monday')
    ts_season <- rbms::ts_monit_season(ts_date, StartMonth = 4, EndMonth = 9,
                                       StartDay = 1, EndDay = NULL, CompltSeason = TRUE, Anchor = TRUE,
                                       AnchorLength = 2, AnchorLag = 2, TimeUnit = 'd')
    ts_season_visit <- rbms::ts_monit_site(ts_season, rclimvisit_data)
    
    # Attempt to process data for each species within the region
        for(species in unique(rclimcount_data$SPECIES)){
        
          species_filename <- sprintf("%s/results_%s_%s_%s.csv", base_path, gsub(" ", "_", region), gsub(" ", "_", rclim), gsub(" ", "_", species))
          
          cat(sprintf("  Processing species %d/%d: %s\n", species_counter, length(unique_species), species))
          species_counter <- species_counter + 1
          
          # Subset data for the current species in the current region
          speciescount_data <- rclimcount_data %>% filter(SPECIES  == species)
    
          #Filter speciescount_data by some criteria (5  sites with minim occurrence = 3 in at least 3 years)
          #Assess occurrences per year per species per site. Retains species-site pairs where the species met the occurrence criteria in at least 3 different years
          species_yearly_occurrences <- speciescount_data %>%
            #filter(COUNT >= 1) %>%
          group_by(SITE_ID, year) %>%
          summarise(DaysWithOccurrences = n_distinct(DATE), .groups = "drop") %>%
          filter(DaysWithOccurrences >= 3) %>%
          group_by(SITE_ID) %>%
          summarise(YearsWithOccurrences = n_distinct(year), .groups = "drop") %>%
          filter(YearsWithOccurrences >= 3)
          
    
      # Check if the file already exists
      if (!file.exists(species_filename)){
      
        if(nrow(species_yearly_occurrences) >=5 ){
          
          tryCatch({
          
          # Filter speciescount_data by the 300 top sites using function rank_sites
          top_speciescount_data <- rank_sites(speciescount_data, rclimvisit_data, rclimcoord_data)
          
          # Perform operations for the current species
          ts_season_count <- rbms::ts_monit_count_site(ts_season_visit, top_speciescount_data, 
                                                       sp = species)
          ts_flight_curve <- rbms::flight_curve(ts_season_count, NbrSample = 300, 
                                                MinVisit = 10, MinOccur = 3, MinNbrSite = 5, 
                                                MaxTrial = 4, GamFamily = 'poisson', 
                                                SpeedGam = FALSE, CompltSeason = TRUE, 
                                                SelectYear = NULL, TimeUnit = 'd')
          
          # extract phenology data from the ts_fligh_curve list
          
          pheno <- ts_flight_curve$pheno
          
          # Impute predicted counts for missing monitoring dates
          
          impt_counts <- rbms::impute_count(ts_season_count = ts_season_count, 
                                            ts_flight_curve = pheno, YearLimit = NULL, 
                                            TimeUnit = 'd')
          sindex <- rbms::site_index(butterfly_count = impt_counts, MinFC = 0.10)
          
          # Ensure the species and region names are included in the results
          sindex[, `:=`(SPECIES = species, GEO_REGION = region, RCLIM = rclim)]
          
          # Save results to CSV within the loop for each species*site 
          if (nrow(sindex) > 0) {
            fwrite(sindex, species_filename)
            warning(sprintf("File '%s' was successfully saved.", species_filename), immediate. = TRUE)
          } else {
            warning(sprintf("sindex is empty. File '%s' was not saved.", species_filename), immediate. = TRUE)
          }
          
        }, error = function(e) {
          cat("Error with species:", species, "in rclim:", rclim, "Error message:", e$message, "\n")
          NULL # Return NULL as indicator of failure that can be handled later
        })
        
        
        #} else {
        #cat(sprintf("%s does not meet the site occurrence criteria at more than 5 sites, skipping.\n", species))
        # Skip to the next species
        #} 
        
      } else {
        cat("File exists, skipping: ", species, "\n")
      }
        
      } # End of species loop
      
      region_counter <- region_counter + 1 # Move to the next region
      
    } # End of rclim loop
  
  } # End of region loop


# Stop the parallel backend when done
stopImplicitCluster()

# save console message
sink()

# --- Compile sindex data --- #

# Set the base path
base_path <- "D:/URBAN TRENDS/sindex_results"
base_path <- "/Users/SUITCASE/Documents/URBAN_TRENDS/sindex_results"

# List all CSV files in the directory
files <- list.files(base_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store data from each file
data_list <- list()

# Loop through the files and read each one
for (i in seq_along(files)) {
  # Read the current file
  temp_data <- fread(files[i])
  # Append the data to the list
  data_list[[i]] <- temp_data
}

# Combine all data tables in the list into one
sindex_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

# View the combined data
print(sindex_data)

# Define the filename and path for the combined data
output_file_path <- "D:/URBAN TRENDS/sindex_results/sindex_results.csv"
output_file_path <- "/Users/SUITCASE/Documents/URBAN_TRENDS/sindex_results/sindex_results.csv"

# Save the combined data table to the specified file
fwrite(sindex_data, output_file_path)




