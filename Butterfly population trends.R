## Clean enviroment and set directory to files
rm(list=ls())
setwd("D:/URBAN TRENDS/BMS data/BMS DATA 2024")  

#Libraries required
library(dplyr) # For data manipulation tasks like filtering, grouping, and summarizing
library(nlme) # For fitting the generalized least squares (GLS) models with gls function with the temporal correlation structure
library(broom) # To convert statistical analysis objects from R into tidy format using tidy function
library(readr) # For reading CSV files into R with read.csv

# Data
sindex <- read.csv("sindex_results.csv", sep=",", dec=".")
head(sindex)

#Filtering temporal series with a minimum of years and positive values

sindex_filt <- sindex %>%
  group_by(SPECIES, SITE_ID, RCLIM) %>%
  # First, make sure that there are at least 10 different years of data
  filter(n_distinct(M_YEAR) >= 8) %>%
  # Then, for each group, calculate the percentage of years with SINDEX < 1
  mutate(porcentaje_bajo_1 = sum(SINDEX < 1, na.rm = TRUE) / n()) %>%
  # Filter to keep only those groups where less than half of the years have SINDEX < 1
  filter(porcentaje_bajo_1 <= 0.5) %>%
  # Remove the auxiliary column used for the calculation
  select(-porcentaje_bajo_1) %>%
  ungroup()

head(sindex_filt)

# Calculate log_sindex
sindex_filt <- sindex_filt %>%
  group_by(SPECIES, SITE_ID) %>%
  mutate(log_sindex = log(SINDEX + 1)) %>%  
  ungroup()  

# Initialize an empty list to store model summaries
model_summaries_list <- list()

# Create a unique ID for each combination of SPECIES and SITE_ID for iteration
sindex_filt$group_id <- with(sindex_filt, interaction(SPECIES, SITE_ID, drop = TRUE))

# Remove "Inf" values
sindex_filt <- sindex_filt %>%
  filter(!is.infinite(log_sindex))

# Get a vector of unique group IDs
unique_group_ids <- unique(sindex_filt$group_id)

# Loop through each unique group ID to calculate population trends
for(group_id in unique_group_ids) {
  # Subset the data for the current group
  data_subset <- sindex_filt[sindex_filt$group_id == group_id, ]
  
  # Check if there's enough data for modeling
  if(nrow(data_subset) > 1) {
    tryCatch({
      # Attempt to fit the GLS model with correlation structure
      gls_model <- gls(log_sindex ~ M_YEAR, data = data_subset,
                       correlation = corAR1(form = ~ M_YEAR))
      model_summary <- tidy(gls_model)
    }, error = function(e) {
      # Check if the error is due to convergence issues
      if(grepl("false convergence \\(8\\)", e$message) | grepl("singular convergence \\(7\\)", e$message)) {
        # Attempt to fit a simpler GLS model without the correlation structure
        message(sprintf("Fitting simpler model for group_id = '%s' due to convergence issue.", group_id))
        tryCatch({
          gls_model <- gls(log_sindex ~ M_YEAR, data = data_subset)
          model_summary <- tidy(gls_model)
        }, error = function(e) {
          message(sprintf("Simpler model also failed for group_id = '%s': %s", group_id, e$message))
          model_summary <- NULL
        })
      } else {
        message(sprintf("Model failed for group_id = '%s': %s", group_id, e$message))
        model_summary <- NULL
      }
    })
    
    if(!is.null(model_summary)) {
      # Add back SPECIES, SITE_ID, and group_id to the model summary if the model was successful
      model_summary$SPECIES <- unique(data_subset$SPECIES)
      model_summary$SITE_ID <- unique(data_subset$SITE_ID)
      model_summary$group_id <- group_id
      
      # Append the model summary to the list
      model_summaries_list[[as.character(group_id)]] <- model_summary
    }
  } else {
    message(sprintf("Not enough data for group_id = '%s'", group_id))
  }
}

# Combine all model summaries into one dataframe
model_summaries <- do.call(rbind, model_summaries_list)

# Filter for the estimate of M_YEAR only and select required columns
estimate_summary <- model_summaries %>%
  filter(term == "M_YEAR") %>%
  select(SPECIES, SITE_ID, estimate, std.error)

# Show results
print(estimate_summary)

# Save results
file_path <- "D:/URBAN TRENDS/BMS data/BMS DATA 2024/butterfly_population_trends.csv"
write_csv(estimate_summary, file_path)




