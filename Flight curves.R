###### Flight curves and SINDEX calculation ######

# Clean environment and set the working directory to the location of the data files
rm(list = ls())  # Remove all objects from the current R session to ensure a clean working environment
setwd("D:/URBAN TRENDS/BMS data/BMS DATA 2024")  

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
library(suncalc)


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

# Extract bms_id from transect_id and select relevant columns
ebms_clim_df <- ebms_clim_df %>%
  mutate(bms_id = str_extract(transect_id, "^[^.]*")) %>%
  dplyr::select(bms_id, transect_id, genzname)

# uBMS data
# Import count data
ubms_count_df <- read.csv("output_count_table.csv", sep = ",", dec = ".")

# Assign a unique identifier to uBMS data
ubms_count_df$bms_id <- "ES_uBMS"

# Select and reorder columns to match the structure of eBMS count data
ubms_count_df <- ubms_count_df %>%
  dplyr::select(visit_id, bms_id, transect_id, visit_date, year, month, day, species_name, count)

# Import visit data and perform necessary transformations
ubms_visit_df <- read.csv("raw_ubms_ebms_visit.csv", sep = ",", dec = ".")

ubms_visit_df$bms_id <- "ES_uBMS"  # Assign the uBMS identifier

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

## Concatenate rows

m_count_df <- rbind(ebms_count_df, ubms_count_df)
m_visit_df<- rbind(ebms_visit_df, ubms_visit_df)
m_clim_df<- ebms_clim_df
m_coord<- ebms_coord_df

## Transform data frames to data tables

m_count <- data.table(m_count_df)
m_visit <- data.table(m_visit_df)
m_clim <- data.table(m_clim_df)

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

#NA values in RCLIM correspond to uBMS transects. All of them are part of climate region K
m_count$RCLIM <- replace(m_count$RCLIM, is.na(m_count$RCLIM), "K. Warm temperate and mesic")

# --- Establishing photoperiod-based geographic region --- #

# Function to calculate day length based on latitude and date
calculate_day_length <- function(lat, date) {
  sun_times <- getSunlightTimes(date, lat, 0)  # Longitude 0, adjust as needed
  day_length <- as.numeric(difftime(sun_times$sunset, sun_times$sunrise, units = "hours"))
  return(day_length)
}

min_latitude <- 28.07431
max_latitude <- 67.47644
date <- as.Date("2024-06-20") # summer solstice in 2024
current_latitude <- min_latitude
start_latitude <- current_latitude
initial_day_length <- calculate_day_length(start_latitude, date)

regions <- data.frame(start_latitude = numeric(), end_latitude = numeric(), day_length_diff = numeric())

while(current_latitude <= max_latitude) {
  current_day_length <- calculate_day_length(current_latitude, date)
  
  if(!is.na(current_day_length) && (abs(current_day_length - initial_day_length) >= 1 || current_latitude == max_latitude)) {
    regions <- rbind(regions, data.frame(start_latitude = start_latitude, end_latitude = current_latitude, day_length_diff = abs(current_day_length - initial_day_length)))
    
    start_latitude <- current_latitude
    initial_day_length <- current_day_length
  }
  
  current_latitude <- current_latitude + 0.1  # Adjust the step size as needed for precision vs. performance
}

regions$name_region <- paste0("R", seq_along(regions$start_latitude))

#Convert transect_lat from EPSG:3035 to latitude in EPSG:4326

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

# A function to find the corresponding region based on latitude
find_region <- function(lat) {
  region <- regions$name_region[lat >= regions$start_latitude & lat <= regions$end_latitude]
  if (length(region) == 0) return(NA)
  return(region)
}

# Apply the function to each row in m_coord to create the geo_region column
m_coord_clean$geo_region <- sapply(m_coord_clean$transect_lat_geo, find_region)

# Joining m_count with m_coord_clean to include geo_region based on bms_id and the corresponding site/transect ID
m_count <- m_count %>%
  left_join(m_coord_clean %>% dplyr::select(bms_id, transect_id, geo_region), 
            by = c("bms_id" = "bms_id", "SITE_ID" = "transect_id"))

# Joining m_visit with m_coord_clean to include geo_region based on bms_id and the corresponding site/transect ID
m_visit <- m_visit %>%
  left_join(m_coord_clean %>% dplyr::select(bms_id, transect_id, geo_region), 
            by = c("bms_id" = "bms_id", "SITE_ID" = "transect_id"))


# --- Flight curves and SINDEX calculation --- #

# Iterate over each unique geographic region in the monitoring count data
for(region in unique(m_count$geo_region)){
  
  # Filter monitoring count and visit data for the current latitudinal geographic region
  geocount_data <- m_count %>% filter(.data$geo_region == region)
  geovisit_data <- m_visit %>% filter(.data$geo_region == region)
  
  # Inform about the processing status
  cat(sprintf("Processing latitudinal region %s: found %d unique species\n", region, length(unique(geocount_data$SPECIES))))
  
  # Identify unique climate regions for the current rclim to analyze region-specific trends
  unique_rclim <- unique(na.omit(geocount_data$RCLIM))
  unique_rclim <- setdiff(unique_rclim, "") # Remove empty string levels, if any
  
  # Initialize storage for results across all regions
  all_results <- list()
  region_counter <- 1 # Track processed regions
  
  # Analyze data for each climate region
  for(rclim in unique_rclim){
    # Filter data for the current region
    rclimcount_data <- geocount_data %>% filter(RCLIM == rclim)
    rclimvisit_data <- geovisit_data %>% filter(RCLIM == rclim)
    
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
    ts_season <- rbms::ts_monit_season(ts_date, StartMonth = StartMonth, EndMonth = EndMonth,
                                       StartDay = 1, EndDay = NULL, CompltSeason = TRUE, Anchor = TRUE,
                                       AnchorLength = 2, AnchorLag = 2, TimeUnit = 'd')
    ts_season_visit <- rbms::ts_monit_site(ts_season, m_visit)
    
    # Attempt to process data for each species within the region
    results <- tryCatch({
      foreach(species = unique_species, .combine = 'rbind', .packages = c("rbms", "data.table")) %do% {
        # Subset data for the current species in the current region
        current_species_data <- rclimcount_data[SPECIES == species]
        
        # Output the species being processed
        cat(sprintf("  Processing species %d/%d: %s\n", species_counter, length(unique_species), species))
        species_counter <- species_counter + 1
        
        # Perform operations for the current species
        ts_season_count <- rbms::ts_monit_count_site(ts_season_visit, current_species_data, 
                                                     sp = species)
        ts_flight_curve <- rbms::flight_curve(ts_season_count, NbrSample = 300, 
                                              MinVisit = 10, MinOccur = 3, MinNbrSite = 5, 
                                              MaxTrial = 4, GamFamily = 'poisson', 
                                              SpeedGam = FALSE, CompltSeason = TRUE, 
                                              SelectYear = NULL, TimeUnit = 'd')
        
        # extract phenology data from the ts_fligh_curve list
        
        pheno <- ts_flight_curve$pheno
        
        # Impute predicted counts for missing monitoring dates
        
        ## The impute_count() function produces a data.table that contains the original COUNT values,
        ## a series of IMPUTED_COUNT over monitoring season, TOTAL_COUNT per site and year, 
        ## TOTAL_NM (e.g. the proportion of the flight curve covered by the visits) and SINDEX. 
        ## The SINDEX is the site index that corresponds to the sum of both observed and imputed 
        ## counts over the sampling season. If the flight curve of a specific year is missing, 
        ## the impute_count() function uses the nearest phenology found. 
        ## If none is available within the limit of years set by the YearLimit parameter, 
        ## the function will return no SINDEX for that specific year.
        
        impt_counts <- rbms::impute_count(ts_season_count = ts_season_count, 
                                          ts_flight_curve = pheno, YearLimit = NULL, 
                                          TimeUnit = 'd')
        sindex <- rbms::site_index(butterfly_count = impt_counts, MinFC = 0.10)
        
        # Ensure the species and region names are included in the results
        impt_counts[, `:=`(SPECIES = species, RCLIM = rclim)]
      }
    }, error = function(e) {
      cat("Error in parallel processing:", e$message, "\n")
      NULL # Handle errors gracefully
    })
    
    # Store and combine results for analysis and output
    all_results[[region]] <- results
    region_counter <- region_counter + 1 # Move to the next region
  }
  
  # Combine and save final results for the current rclim
  final_results_rclim <- rbindlist(all_results, use.names = TRUE, fill = TRUE)
  write.csv(final_results_bms_id, sprintf("final_results_%s.csv", bms_id))
}


