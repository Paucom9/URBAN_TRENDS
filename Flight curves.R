###### Flight curves and SINDEX calculation ######

# Clean environment and set the working directory to the location of the data files
rm(list = ls())  # Remove all objects from the current R session to ensure a clean working environment
setwd("D:/URBAN TRENDS/BMS data/BMS DATA 2024")  # Set the working directory to where your data files are located

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


# --- Selection of start and end months for butterfly monitoring based on visit data ---

# Group visits by week and butterfly monitoring scheme ID (bms_id), then count visits per week
weekly_visits <- m_visit %>%
  group_by(bms_id, week) %>%
  summarise(total_visit = n(), .groups = 'drop') # Count the number of visits for each bms_id and week

# Calculate quantiles to identify the active monitoring season for each bms_id
quantile_weeks <- weekly_visits %>%
  group_by(bms_id) %>%
  mutate(
    cumulative_visit = cumsum(total_visit), # Cumulative sum of visits to identify the distribution over weeks
    total_year_visit = max(cumulative_visit) # Total visits for the year to calculate quantiles
  ) %>%
  mutate(
    p5_week = total_year_visit * 0.05, # 5th percentile of visits
    p95_week = total_year_visit * 0.95  # 95th percentile of visits
  ) %>%
  summarize(
    week_5th_percentile = week[min(which(cumulative_visit >= p5_week))], # Week reaching the 5th percentile
    week_95th_percentile = week[min(which(cumulative_visit >= p95_week))] # Week reaching the 95th percentile
  ) %>%
  ungroup()

# Convert week percentiles to months to define the monitoring season
months_selection <- quantile_weeks %>%
  mutate(
    month_start = ceiling(week_5th_percentile / 4.348), # Convert start week to month, assuming ~4.348 weeks/month
    month_start = ifelse(month_start > 12, 12, month_start), # Ensure month_start doesn't exceed December
    month_end = ceiling(week_95th_percentile / 4.348), # Convert end week to month
    month_end = ifelse(month_end > 12, 12, month_end) # Ensure month_end doesn't exceed December
  )

# --- Flight curves and SINDEX calculation ---

  # Select start and end months for butterfly monitoring for the current climate region
  filt_months_selection <- months_selection %>% filter(bms_id == id)
  StartMonth <- filt_months_selection$month_start[1]
  EndMonth <- filt_months_selection$month_end[1]
  
  # Identify unique climate regions 
  unique_regions <- unique(na.omit(m_count$RCLIM))
  unique_regions <- setdiff(unique_regions, "") # Remove empty string levels, if any
  
  # Initialize storage for results across all regions
  all_results <- list()
  region_counter <- 1 # Track processed regions
  
  # Analyze data for each climate region
  for(region in unique_regions){
    
    cat(sprintf("Processing region %d/%d: %s\n", region_counter, length(unique_regions), region))
    
    # Filter data for the current region
    region_data <- m_count %>% filter(RCLIM == region)
    
    # Since we're not looping over bms_id, define a generic or common period for analysis.
    # This requires adjusting the selection of StartMonth and EndMonth
    # If these months vary by region or another variable, adjust this section accordingly.
    # For demonstration, assuming fixed months or retrieving from a different logic:
    StartMonth <- 4 # Example, April
    EndMonth <- 9 # Example, September
    
    # Extract unique species within the region for analysis
    unique_species <- unique(region_data$SPECIES)
    
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
        current_species_data <- region_data[SPECIES == species]
        
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
        impt_counts[, `:=`(SPECIES = species, RCLIM = region)]
      }
    }, error = function(e) {
      cat("Error in parallel processing:", e$message, "\n")
      NULL # Handle errors gracefully
    })
    
    # Store and combine results for analysis and output
    all_results[[region]] <- results
    region_counter <- region_counter + 1 # Move to the next region
  }
  
  # Combine and save final results for the current bms_id
  final_results_bms_id <- rbindlist(all_results, use.names = TRUE, fill = TRUE)
  write.csv(final_results_bms_id, sprintf("final_results_%s.csv", bms_id))


