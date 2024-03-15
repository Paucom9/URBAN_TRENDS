# --- rank_sites function to spatially stratify and select the most informative transects for the regional GAM --- #

# Required libraries
library(data.table)
library(sf)
library(raster)

# Define the rank_sites function to rank and select top sites based on visit counts and spatial distribution
rank_sites <- function(subcount, m_visit, m_coord, country_code) {
  # Check for sites exceeding the threshold and proceed only if necessary
  if (n_distinct(subcount$SITE_ID) <= 300) {
    cat("There are 300 or fewer unique SITE_IDs in subcount. No additional processing needed.\n")
    return(subcount)
  }
  
  # Aggregate visit counts by site and year for subcount and m_visit data
  subcount_n <- subcount[, .N, by = .(SITE_ID, year)]
  m_visit_n <- m_visit[, .N, by = .(SITE_ID, year)]
  
  # Combine counts with visit data for each site and year
  combined_data <- merge(subcount_n, m_visit_n, by = c("SITE_ID", "year"), all.x = TRUE)
  # Rename the columns for clarity
  setnames(combined_data, c("N.x", "N.y"), c("Ncount", "Nvisit"))
  
  # Order the data by visit and count numbers (descending)
  combined_data <- combined_data[order(-Nvisit, -Ncount),]
  
  # Merge the ordered data with coordinates
  combined_data <- merge(combined_data, m_coord, by.x = "SITE_ID", by.y = "transect_id", all.x = TRUE)
  
  # Convert merged data to a spatial data frame, ensuring valid longitude values
  sites_sf <- st_as_sf(combined_data[!is.na(combined_data$transect_lon), ], 
                       coords = c("transect_lon", "transect_lat"), crs = 3035)
  
  # Fetch and transform the country map to the same CRS as site data
  country_map <- st_transform(st_as_sf(getData(name = "GADM", country = country_code, level = 0)), 
                              crs = st_crs(sites_sf))
  
  # Generate a grid overlay for the country map
  country_grid <- st_as_sf(st_make_grid(st_bbox(country_map), cellsize = 100000, square = TRUE, what = "polygons"))
  intersects_grid <- as.numeric(st_intersects(country_grid, country_map))
  country_grid_filtered <- country_grid[intersects_grid == 1,]
  country_grid_filtered$gid <- 1:nrow(country_grid_filtered)
  
  # Assign a grid ID to each site based on spatial intersection
  sites_sf$gid <- as.numeric(st_intersects(sites_sf, country_grid_filtered))
  
  # Convert the spatial data frame back to a data table and reorder by visit count, count number, and grid ID
  sites_dt <- data.table(sites_sf)[, geometry := NULL]
  ordered_sites <- sites_dt[order(-Nvisit, -Ncount, gid),]
  
  # Rank sites within each grid and select top 300 based on their overall ranking
  ordered_sites <- ordered_sites[, gid_ord := 1:.N, by = gid][order(-Nvisit, -Ncount, gid),]
  ordered_sites <- ordered_sites[order(gid_ord, -Nvisit, -Ncount),][1:300,]
  selected_sites <- ordered_sites[, .(SITE_ID, bms_id)]
  
  # Rank sites by year for more nuanced selection
  selected_sites_by_year <- list()
  for (k in unique(ordered_sites$year)) {
    temp_data <- ordered_sites[year == k,][, gid_ord := 1:.N, by = gid]
    temp_data <- temp_data[order(gid_ord, -Nvisit, -Ncount),]
    selected_sites_by_year[[k]] <- temp_data[, .(gid_ord, SITE_ID)]
    names(selected_sites_by_year[[k]]) <- c("grid_ord", paste0("SITE_ID_", k))
  }
  
  # Combine yearly rankings into a single data frame and fill NA values
  selected_sites_combined <- Reduce(function(x, y) merge(x, y, by = "SITE_ID", all = TRUE), selected_sites_by_year)
  selected_sites_combined[is.na(selected_sites_combined)] <- max(unlist(selected_sites_combined), na.rm = TRUE) + 1
  
  # Calculate mean rank across years and reorder sites based on this ranking
  selected_sites_combined$mean_rank <- rowMeans(selected_sites_combined[, sapply(selected_sites_combined, is.numeric)], na.rm = TRUE)
  final_ranked_sites <- selected_sites_combined[order(mean_rank)]
  
  # Select the top 300 sites based on mean rank
  top_sites <- final_ranked_sites[1:300,]
  
  # Filter the original subcount data to include only the top ranked sites
  filtered_subcount <- subcount[SITE_ID %in% top_sites$SITE_ID,]
  return(filtered_subcount)
}

# Example usage of the function
# Adjust the function call as per your actual data variable names and country code
# result_data <- rank_sites(subcount = your_subcount_data, m_visit = your_visit_data, m_coord = your_coordinates_data, country_code = "GBR")


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


