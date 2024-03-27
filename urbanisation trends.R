# Remove all objects from the current R session to ensure a clean working environment
rm(list = ls())  

# Required libraries
library(dplyr)
library(broom)
library(data.table)

# --- Read and manage built data --- #
setwd("D:/URBAN TRENDS/Urbanisation data") 

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

built_ubms <- built_ubms %>%
  mutate(bms_id = "ES_uBMS") %>% # Add the bms_id column with all values set to "ES_uBMS"
  select(bms_id, longitude, latitude, everything()) # Reorder column

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

# Filter by a specific variable
built_dt <- built_long %>%
  filter(variable == "sum_2000m")

# Remove NA values
built_dt <- na.omit(built_dt)

# Standardizing the 'value' variable
built_dt$value_scaled <- scale(built_dt$value)

# --- Identify the temporal series of each species-site combination of sindex_results --- #

setwd("D:/URBAN TRENDS/BMS data/BMS DATA 2024")  

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
  filter(N_YEARS >= 8, Sp_YEARS >= 0.5)

# --- Loop to calculate for each species*site the associated urbanisation trend --- #

# Initialize an empty data frame to store the summary information
urb_trends_df <- data.frame(SPECIES = character(),
                            SITE_ID = character(),
                            Best_Model = character(),
                            urb_trend = numeric(),
                            stringsAsFactors = FALSE) # Prevent conversion to factors

for(i in 1:nrow(sindex_yrs)) {
  subset_data <- built_dt %>%
    mutate(transect_id = as.character(transect_id)) %>%
    filter(transect_id == as.character(sindex_yrs$SITE_ID[i]))
  
  # Fit the four models
  linear_model <- lm(value_scaled ~ year, data = subset_data)
  polynomial_model <- lm(value_scaled ~ poly(year, 2), data = subset_data)
  exponential_model <- lm(log(value + 1) ~ year, data = subset_data)
  logarithmic_model <- lm(value ~ log(year), data = subset_data)
  
  # Calculate AIC values for each model
  aic_values <- c(AIC(linear_model), AIC(polynomial_model), AIC(exponential_model), AIC(logarithmic_model))
  
  model_names <- c("linear", "polynomial", "exponential", "logarithmic")
  best_model_name <- model_names[which.min(aic_values)]
  best_model <- get(paste0(best_model_name, "_model"))
  
  # Calculate the slope
  slope_data <- data.frame(year = c(sindex_yrs$STR_YEAR[i], sindex_yrs$END_YEAR[i]))
  predicted_values <- predict(best_model, newdata = slope_data)
  slope <- (predicted_values[2] - predicted_values[1]) / (sindex_yrs$END_YEAR[i] - sindex_yrs$STR_YEAR[i])
  
  
  # Append to the summary data frame
  urb_trends_df <- rbind(urb_trends_df, data.frame(SPECIES = sindex_yrs$SPECIES[i],
                                                   SITE_ID = sindex_yrs$SITE_ID[i],
                                                   Best_Model = best_model_name,
                                                   urb_trend = slope,
                                                   stringsAsFactors = FALSE))
}

# Check the first few rows of the summary
head(urb_trends_df)


