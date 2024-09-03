# Remove all objects from the current R session to ensure a clean working environment
rm(list = ls())  

# Required libraries
library(dplyr)
library(broom)
library(data.table)
library(tidyr)
library(mgcv)

# --- Read and manage built data --- #

# ----
setwd("E:/URBAN TRENDS/Urbanisation data") 

# Total built-up surface
built_ebms <- read.csv("embs_ubms_GHS_BUILT_stats.csv", sep = ";", dec = ".")
built_ubms <- read.csv("ubms_sites_GHS_BUILT.csv", sep = ";", dec = ".")

# Rename columns
built_ebms <- built_ebms %>%
  dplyr::rename(
    transect_id = transect_i,
    longitude = geometry,
    latitude = geom1
  )

built_ubms <- built_ubms %>%
  dplyr::rename(
    transect_id = transect_i,
    longitude = transect_1,
    latitude = transect_2
  )

built_ubms <- built_ubms %>%
  dplyr::select(-transect_l)  # Remove the 'transect_l' column

built_ubms <- built_ubms %>%
  mutate(bms_id = "ES_uBMS") %>% # Add the bms_id column with all values set to "ES_uBMS"
  dplyr::select(bms_id, longitude, latitude, everything()) # Reorder column

# rbind ebms and ubms files

built_df <- rbind(built_ebms, built_ubms)

# Reshaping the built data to long format
built_long <- built_df %>%
  pivot_longer(
    cols = c(starts_with("point_"), starts_with("sum_500m_"), starts_with("sum_1000m_"), starts_with("sum_2000m_")),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)$"
  )

# Further reshape the data to have a single measurement column
built_long <- built_long %>%
  pivot_longer(cols = c(point, sum_500m, sum_1000m, sum_2000m),
               names_to = "variable",
               values_to = "value")

# Convert 'year' to numeric
built_long <- built_long %>%
  mutate(year = as.numeric(year))

# Ensure that 'transect_id' and 'bms_id' are factors
built_long$transect_id <- factor(built_long$transect_id)
built_long$bms_id <- factor(built_long$bms_id)

# Transform to a data table
built_dt <- data.table(built_long)

# Remove NA values
built_dt <- na.omit(built_dt)

# Standardizing the 'value' variable
built_dt$value_scaled <- scale(built_dt$value)

# ----


# --- Identify the temporal series of each species-site combination of sindex_results --- #

# ----

setwd("E:/URBAN TRENDS/BMS data/BMS DATA 2024")  

sindex_df <-read.csv("sindex_results.csv", sep=",", dec=".")

# Transform variables
sindex_df$SPECIES <- factor(sindex_df$SPECIES)
sindex_df$SITE_ID <- factor(sindex_df$SITE_ID)

# 
sindex_yrs <- sindex_df %>%
  group_by(SPECIES, SITE_ID) %>%
  summarise(
    STR_YEAR = min(M_YEAR),
    END_YEAR = max(M_YEAR),
    N_YEARS = n_distinct(M_YEAR),
    S_YEARS = n_distinct(M_YEAR[SINDEX > 0]),  # Count unique years with SINDEX > 0
    .groups = "drop"  # Remove the grouping structure
  ) %>%
  mutate(
    Sp_YEARS = S_YEARS / N_YEARS  # Calculate the proportion of S_YEARS
  )

# Filter by a minum number of years and year with positive values of sindex

sindex_yrs <- sindex_yrs %>%
  filter(N_YEARS >= 10, Sp_YEARS >= 0.5)

# ----

# --- Loop to calculate for each species*site the associated urbanization trend --- #
# ----

# Initialize an empty data frame to store the summary information for all variables
urb_trends_all_df <- data.frame(SPECIES = character(),
                                SITE_ID = character(),
                                urb_variable = character(),
                                Best_Model = character(),
                                urb_trend = numeric(),
                                stringsAsFactors = FALSE)  # Prevent conversion to factors

# Unique variables in built_long
unique_variables <- unique(built_long$variable)

# Loop over each unique variable
for(current_variable  in unique_variables) {
  
  # Filter by a specific variable
  built_dt <- built_long %>%
    filter(variable == current_variable ) %>%
    na.omit() %>%

  # Loop over each species*site combination
  for(i in 1:nrow(sindex_yrs)) {
    subset_data <- built_dt %>%
      mutate(transect_id = as.character(transect_id)) %>%
      filter(transect_id == as.character(sindex_yrs$SITE_ID[i]))
    
    # Check if there's enough data to fit the model
    if(nrow(subset_data) > 1) {
      # Fit the four models
      linear_model <- lm(value ~ year, data = subset_data)
      polynomial_model <- lm(value ~ poly(year, 2), data = subset_data)
      exponential_model <- lm(log(value + 1) ~ year, data = subset_data)
      logarithmic_model <- lm(value ~ log(year), data = subset_data)
      
      # Calculate AIC values for each model
      aic_values <- c(AIC(linear_model), AIC(polynomial_model), AIC(exponential_model), AIC(logarithmic_model))
      
      model_names <- c("linear", "polynomial", "exponential", "logarithmic")
      best_model_name <- model_names[which.min(aic_values)]
      best_model <- get(paste0(best_model_name, "_model"))
      
      # Prepare data for prediction
      slope_data <- data.frame(year = c(sindex_yrs$STR_YEAR[i], sindex_yrs$END_YEAR[i]))
      
      # Predict using the best model
      predicted_values <- predict(best_model, newdata = slope_data)
      
      # Calculate the slope for the specific period
      slope <- (predicted_values[2] - predicted_values[1]) / (sindex_yrs$END_YEAR[i] - sindex_yrs$STR_YEAR[i])
      
      # Append to the summary data frame
      urb_trends_all_df <- rbind(urb_trends_all_df, data.frame(SPECIES = sindex_yrs$SPECIES[i],
                                                               SITE_ID = sindex_yrs$SITE_ID[i],
                                                               urb_variable = current_variable,
                                                               Best_Model = best_model_name,
                                                               urb_trend = slope,
                                                               stringsAsFactors = FALSE))
    }
  }
}

# Check the first few rows of the summary
head(urb_trends_all_df)
hist(urb_trends_all_df$urb_trend)

write.csv(urb_trends_all_df, "E:/URBAN TRENDS/Urbanisation data/urb_trends_rev.csv", row.names = FALSE)


#  --- Interpolate year data of built-up fraction using exponential models  --- #
# ----

# --- Predict year data using GAMs ---#
# ----
head(built_dt)

# Assuming built_dt is your dataset

# Convert year to numeric
built_dt[, year := as.numeric(year)]

# Create an empty list to store predictions
predictions_list <- list()

# Iterate over unique combinations of transect_id and variable
for (i in 1:nrow(unique_combinations)) {
  # Get current combination
  transect_id <- unique_combinations$transect_id[i]
  variable <- unique_combinations$variable[i]
  
  # Subset data for the current combination
  subset_data <- built_dt[transect_id == unique_combinations$transect_id[i] & 
                            variable == unique_combinations$variable[i], ]
  
  # Remove rows with NA or zero values
  subset_data <- subset_data[!is.na(value)]
  
  # Check if there are at least 3 data points for modeling
  if (nrow(subset_data) > 2) {
    # Fit GAM model with a smooth term for 'year'
    fit <- gam(value ~ s(year), data = subset_data)
    
    # Predict values for the years 1975 to 2025
    year_range <- 1975:2025
    predictions <- data.frame(year = year_range,
                              predictions = exp(predict(fit, newdata = data.frame(year = year_range))))
    
    # Add transect_id and variable columns to predictions
    predictions <- data.table(predictions)
    predictions[, c("transect_id", "variable") := list(transect_id, variable)]
    
    # Store predictions in the list
    predictions_list[[i]] <- predictions
  } else {
    # Create a data table with NA values if there are insufficient data points
    predictions <- data.table(year = 1975:2025, predictions = rep(NA, length(year_range)))
    predictions[, c("transect_id", "variable") := list(transect_id, variable)]
    
    # Store NA predictions in the list
    predictions_list[[i]] <- predictions
  }
  
  # Update progress bar
  setTxtProgressBar(pb, i)
}

# Close progress bar
close(pb)

# Combine predictions from the list into a single data table
predictions <- rbindlist(predictions_list, fill = TRUE)


# View the predictions
head(predictions)

urban_year_data <- predictions %>%
  pivot_wider(
    id_cols = c(transect_id, year),
    names_from = variable,
    values_from = predictions
  )

# Rename the columns to match the desired format
colnames(urban_year_data) <- c("SITE_ID","year","built_100", "built_500", "built_1000", "built_2000")

write.csv(urban_year_data, "D:/URBAN TRENDS/Urbanisation data/urban_year_data.csv", row.names = FALSE)



# ----

# --- Read and manage MOD data --- #
# ----


setwd("E:/URBAN TRENDS/Urbanisation data") 
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
  dplyr::select(-transect_l)  # Remove the 'transect_l' column

mod_ubms <- mod_ubms %>%
  mutate(bms_id = "ES-uBMS") %>% # Add the bms_id column with all values set to "ES_uBMS"
  dplyr::select(bms_id, longitude, latitude, everything()) # Reorder column

# rbind ebms and ubms files

mod_df <- rbind(mod_ebms, mod_ubms)

head(mod_df)
str(mod_df)

# ----

# --- In how many sites the mod category change during the time series? --- #
# ----

# Select only the 'point_year' columns
point_cols <- mod_df[, grepl("^point_", names(mod_df))]

# Compare each year's column to the next to identify changes, handling NA values
changes <- (point_cols[, -ncol(point_cols)] != point_cols[, -1]) & !is.na(point_cols[, -ncol(point_cols)] | point_cols[, -1])

# Sum these comparisons row-wise to find rows with any changes
rows_with_changes <- rowSums(changes, na.rm = TRUE) > 0

# Count how many rows have at least one change
num_rows_with_changes <- sum(rows_with_changes, na.rm = TRUE)

# Percentage of sites that changed the category
percent_rows <- num_rows_with_changes/nrow(mod_df) * 100
# ----

# --- Select the MOD category at the STR_YEAR for each SPECIES*SITE_ID temporal series --- #
# ----
head(mod_df)
head(sindex_yrs)

# Define a function to map STR_YEAR to the corresponding point_year column name
map_year_to_point_col <- function(year) {
  # Define the breakpoints and names based on the 5-year intervals in mod_df
  breakpoints <- seq(1975, 2025, by = 5)
  names <- paste0("point_", breakpoints)
  
  # Find the right interval for the STR_YEAR
  interval_index <- findInterval(year, breakpoints)
  # Return the corresponding column name
  return(names[interval_index])
}

# Initialize a vector to store the point values
points <- numeric(nrow(sindex_yrs))

# Loop through each row in sindex_yrs to get the point value
for(i in 1:nrow(sindex_yrs)) {
  # Get the current SITE_ID and STR_YEAR
  site_id <- sindex_yrs$SITE_ID[i]
  str_year <- sindex_yrs$STR_YEAR[i]
  
  # Map STR_YEAR to the corresponding point_year column name
  point_col_name <- map_year_to_point_col(str_year)
  
  # Find the row in mod_df that matches the SITE_ID
  row_index <- which(mod_df$transect_id == site_id)
  
  # If a matching row is found and the point column exists in mod_df
  if(length(row_index) > 0 && point_col_name %in% names(mod_df)) {
    # Extract the point value and store it in the points vector
    points[i] <- mod_df[row_index, point_col_name]
  } else {
    # If no matching row or column is found, assign NA
    points[i] <- NA
  }
}

# Now, points vector contains the point values for each row in sindex_yrs based on STR_YEAR
sindex_yrs$point <- points
head(sindex_yrs)

mod_str_yr <- sindex_yrs %>%
  dplyr::select(SPECIES, SITE_ID, code_urban = point) %>%
  dplyr::group_by(SPECIES, SITE_ID) %>% # Ensure we respect the unique combinations
  dplyr::mutate(urban_names = case_when(
    code_urban == 30 ~ "URBAN CENTRE",
    code_urban == 23 ~ "DENSE URBAN CLUSTER",
    code_urban == 22 ~ "SEMI-DENSE URBAN CLUSTER",
    code_urban == 21 ~ "SUBURBAN OR PERI-URBAN",
    code_urban == 13 ~ "RURAL CLUSTER GRID",
    code_urban == 12 ~ "LOW DENSITY RURAL",
    code_urban == 11 ~ "VERY LOW DENSITY RURAL",
    code_urban == 10 ~ "WATER",
    TRUE ~ "UNKNOWN"  # This line handles any codes that don't match the above conditions
  )) %>%
  dplyr::ungroup()


write.csv(mod_str_yr, "D:/URBAN TRENDS/Urbanisation data/mod_str_yr.csv", row.names = FALSE)

# ----



