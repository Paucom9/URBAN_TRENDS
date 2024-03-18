# --- rank_sites function to spatially stratify and select the most informative transects for the regional GAM --- #

#Function description: 300 top sites are selected based on a composite criterion that incorporates visitation frequency and species occurrence, spatial distribution, temporal significance, 
#and an aggregate ranking across these dimensions. This multifaceted approach ensures that the sites chosen as "top" reflect a balanced consideration of their activity levels, 
#geographical importance, and relevance over years.

#Required libraries
library(sf)
library(rnaturalearth)
library(data.table)

rank_sites <- function(subcount, m_visit, m_cord) {
  
  # Check for sites exceeding the threshold and proceed only if necessary
  if (n_distinct(subcount$SITE_ID) <= 300) {
    cat("There are 300 or fewer unique SITE_IDs in subcount. No additional processing needed.\n")
    return(subcount)
  }
  
  subcount_n<- subcount[, .N, by =.(SITE_ID, year)]
  m_visit_n<- m_visit[, .N, by =.(SITE_ID, year)]
  
  subcount_visit_n<- merge(subcount_n, m_visit_n , by= c("SITE_ID","year"), all.x = TRUE)
  
  setnames(subcount_visit_n, c("N.x", "N.y"), c("Ncount", "Nvisit"))
  
  
  subcount_visit_n<-subcount_visit_n[order(-Nvisit, -Ncount),]
  
  
  subcount_visit_n<- merge(subcount_visit_n, m_cord, by.x = "SITE_ID", by.y = "transect_id", all.x = TRUE)
  
  subcount_visit_n_sf <- st_as_sf(subcount_visit_n[!is.na(subcount_visit_n$transect_lon), ], coords = c("transect_lon", "transect_lat"), crs = 3035)
  
  
  # Unique country codes from subcount
  unique_countries <- unique(subcount$country_code)
  
  # Initialize an empty list to store the sf objects for each country
  country_maps <- list()
  
  # Loop through the unique country codes to fetch each country's map using getData
  for (code in unique_countries) {
    # Attempt to fetch the country map using GADM data, level 0 (country level)
    country_map <- tryCatch({
      getData('GADM', country = code, level = 0)
    }, error = function(e) NULL) # Skip countries that result in an error
    
    # If a map was successfully fetched, convert it to an sf object and add it to the list
    if (!is.null(country_map)) {
      country_map_sf <- st_as_sf(country_map, crs = st_crs(subcount_visit_n_sf))
      country_maps[[code]] <- country_map_sf
    }
  }
  
  # Combine all the sf objects into a single sf object
  combined_map <- do.call(rbind, country_maps)
  
  # Transform combined_map to EPSG:3035
  combined_map <- st_transform(combined_map, crs = st_crs(subcount_visit_n_sf))
  
  comb_grid<- st_as_sf(st_make_grid(st_bbox(combined_map), cellsize = 100000 , square = TRUE,  what = "polygons"))
  
  # Perform the intersection check
  intersects_list <- st_intersects(comb_grid, combined_map)
  
  # Create a logical vector where TRUE indicates an intersection
  intersects_any <- sapply(intersects_list, length) > 0
  
  # Optionally, filter comb_grid to keep only intersecting cells
  comb_grid_intersecting <- comb_grid[intersects_any, ]
  
  # Assuming you want to add a unique ID to each grid cell
  comb_grid_intersecting$gid <- 1:nrow(comb_grid_intersecting)
  
  subcount_visit_n_sf$gid<-  as.numeric(st_intersects(subcount_visit_n_sf, comb_grid_terr))
  
  subcount_visit_n_dt <- data.table(subcount_visit_n_sf)[, geometry := NULL]
  
  subcount_visit_n<-subcount_visit_n_dt[order(-Nvisit, -Ncount, gid),]
  
  subcount_visit_n[, gid_ord := 1:.N, by = gid][order(-Nvisit, -Ncount, gid),][order(gid_ord, -Nvisit, -Ncount),][1:300,]
  
  rank_sites <- NULL
  rank_sites <- subcount_visit_n[, SITE_ID, bms_id]
  
  
  for(k in unique(subcount_visit_n$year)){
    
    subcount_visit_n_year<-subcount_visit_n[year== k,][, gid_ord := 1:.N, by = gid][order(-Nvisit, -Ncount, gid),][order(gid_ord, -Nvisit, -Ncount),]
    subcount_visit_gid_year<-subcount_visit_n_year[,  gid_ord, SITE_ID]
    setnames(subcount_visit_gid_year, "gid_ord", paste("grid_ord", k, sep=""))
    
    rank_sites<-merge(rank_sites, subcount_visit_gid_year, by = "SITE_ID", all.x=TRUE)
    rank_sites <- data.frame(rank_sites)
    rank_sites[is.na(rank_sites)]<- max(rank_sites[,ncol(rank_sites)], na.rm= TRUE) + 1
    
  }
  
  rank_sites$mean<-rowMeans(rank_sites%>%select_if(is.numeric))
  rank_sites <- data.table(rank_sites)
  
  rank_sites <- rank_sites[order(-mean)][, mean, SITE_ID]
  
  unique_rank_sites <- unique(rank_sites$SITE_ID)[1:300]
  
  top_rank_sites <- rank_sites[SITE_ID %in% unique_rank_sites]
  
  subcount_ordered <- merge(top_rank_sites, subcount, by = "SITE_ID", all.x = TRUE)
  
  return(subcount_ordered)
}



# --- Function to calculate start and end months for monitoring season based on visit data --- #

library(dplyr)

calculate_monitoring_season <- function(data) {
  # Ensure necessary columns are present
  if (!"week" %in% names(data)) {
    stop("The data must contain a 'week' column.")
  }
  
  # Step 1: Calculate monitoring season per bms_id
  if ("bms_id" %in% names(data)) {
    season_by_bms_id <- data %>%
      group_by(bms_id, week) %>%
      summarise(total_visit = n(), .groups = 'drop') %>%
      group_by(bms_id) %>%
      mutate(cumulative_visit = cumsum(total_visit)) %>%
      transmute(
        week,
        bms_id,
        cumulative_visit,
        total_year_visit = max(cumulative_visit),
        p5_week = total_year_visit * 0.05,
        p95_week = total_year_visit * 0.95
      ) %>%
      group_by(bms_id) %>%
      summarise(
        week_5th_percentile = week[min(which(cumulative_visit >= p5_week))],
        week_95th_percentile = week[min(which(cumulative_visit >= p95_week))],
        .groups = 'drop'
      ) %>%
      mutate(
        month_start = ceiling(week_5th_percentile / 4.348),
        month_end = ceiling(week_95th_percentile / 4.348)
      ) %>%
      mutate(
        month_start = ifelse(month_start > 12, 12, month_start),
        month_end = ifelse(month_end > 12, 12, month_end)
      )
    
    # Step 2: Assess variability across bms_id
    start_month_range <- range(season_by_bms_id$month_start)
    end_month_range <- range(season_by_bms_id$month_end)
    
    if (n_distinct(season_by_bms_id$month_start) > 1 || n_distinct(season_by_bms_id$month_end) > 1) {
      warning(paste("Start and/or end months vary across bms_ids.",
                    "Start month range:", paste(start_month_range, collapse = " to "),
                    "End month range:", paste(end_month_range, collapse = " to ")))
    }
  }
  
  # Step 3: Calculate monitoring season for the entire dataset
  overall_season <- data %>%
    group_by(week) %>%
    summarise(total_visit = n(), .groups = 'drop') %>%
    mutate(cumulative_visit = cumsum(total_visit)) %>%
    transmute(
      week,
      cumulative_visit,
      total_year_visit = max(cumulative_visit),
      p5_week = total_year_visit * 0.05,
      p95_week = total_year_visit * 0.95
    ) %>%
    summarise(
      week_5th_percentile = week[min(which(cumulative_visit >= p5_week))],
      week_95th_percentile = week[min(which(cumulative_visit >= p95_week))]
    ) %>%
    mutate(
      month_start = ceiling(week_5th_percentile / 4.348),
      month_end = ceiling(week_95th_percentile / 4.348)
    ) %>%
    mutate(
      month_start = ifelse(month_start > 12, 12, month_start),
      month_end = ifelse(month_end > 12, 12, month_end)
    ) %>%
    summarise(
      month_start = unique(month_start),
      month_end = unique(month_end)
    )
  
  # Step 4: Return a tibble with month_start and month_end for the overall data
  return(overall_season)
}

# Example usage
# result <- calculate_monitoring_season_with_comprehensive_check(your_data_frame)
#make sure your data frame includes 'week' and 'bms_id' columns.


# --- Function to calculate day length based on latitude and date --- #

library(suncalc)

calculate_day_length <- function(lat, date) {
  sun_times <- getSunlightTimes(date, lat, 0)  # Longitude 0, adjust as needed
  if (is.na(sun_times$sunset) || is.na(sun_times$sunrise)) {
    # Handle the NA values, perhaps by setting to 0 or 24 depending on the context
    return(NA)  # or an appropriate value for your use case
  }
  day_length <- as.numeric(difftime(sun_times$sunset, sun_times$sunrise, units = "hours"))
  return(day_length)
}


# --- Function to find the corresponding latitudinal geographic region based on transect latitude --- #
find_region <- function(lat) {
  region <- regions$name_region[lat >= regions$start_latitude & lat <= regions$end_latitude]
  if (length(region) == 0) return(NA)
  return(region)
}

