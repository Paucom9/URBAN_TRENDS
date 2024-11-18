###### Calculation of butterfly population trends ######

## Clean enviroment and set directory to files
rm(list=ls())
setwd("E:/URBAN TRENDS/sindex_results/")  

#Libraries required
library(dplyr) # For data manipulation tasks like filtering, grouping, and summarizing
library(nlme) # For fitting the generalized least squares (GLS) models with gls function with the temporal correlation structure
library(broom) # To convert statistical analysis objects from R into tidy format using tidy function
library(readr) # For reading CSV files into R with read.csv

# Data
sindex <- read.csv("sindex_results.csv", sep=",", dec=".") # Obtained from running "flight_curves_sindex.R"
head(sindex)

#Filtering temporal series with a minimum of years and positive values

sindex_filt <- sindex %>%
  group_by(SPECIES, SITE_ID, RCLIM) %>%
  # Make sure that there are at least 10 different years of data
  filter(n_distinct(M_YEAR) >= 10) %>%
  # For each group, calculate the percentage of years with SINDEX < 1
  mutate(low_percentage_1 = sum(SINDEX < 1, na.rm = TRUE) / n()) %>%
  # Filter to keep only those groups where less than half of the years have SINDEX < 1
  filter(low_percentage_1 <= 0.5) %>%
  # Remove the auxiliary column used for the calculation
  select(-low_percentage_1) %>%
  ungroup()

head(sindex_filt)

# Calculate log_sindex
sindex_filt <- sindex_filt %>%
  group_by(SPECIES, SITE_ID) %>%
  mutate(log_sindex = log(SINDEX + 1)) %>%  
  ungroup()  

# Remove "Inf" values
sindex_filt <- sindex_filt %>%
  filter(!is.infinite(log_sindex))

# Initialize an empty list to store model summaries
model_summaries_list <- list()

# Create a unique ID for each combination of SPECIES and SITE_ID for iteration
sindex_filt$group_id <- with(sindex_filt, interaction(SPECIES, SITE_ID, drop = TRUE))

# Get a vector of unique group IDs
unique_group_ids <- unique(sindex_filt$group_id)

# Initialize progress bar
pb <- txtProgressBar(min = 0, max = length(unique_group_ids), style = 3)
progress <- 0

# Loop through each unique group ID to calculate population trends
for(group_id in unique_group_ids) {
  # Update progress bar
  progress <- progress + 1
  setTxtProgressBar(pb, progress)
  
  # Subset the data for the current group
  data_subset <- sindex_filt[sindex_filt$group_id == group_id, ]
  
  # Check if there's enough data for modeling
  if(nrow(data_subset) > 1) {
    tryCatch({
      # Attempt to fit the GLS model with correlation structure
      gls_model <- gls(log_sindex ~ M_YEAR, data = data_subset,
                       correlation = corAR1(form = ~ M_YEAR))
      model_summary <- summary(gls_model)
    }, error = function(e) {
      # Check if the error is due to convergence issues
      if(grepl("false convergence \\(8\\)", e$message) | grepl("singular convergence \\(7\\)", e$message)) {
        # Attempt to fit a simpler GLS model without the correlation structure
        message(sprintf("Fitting simpler model for group_id = '%s' due to convergence issue.", group_id))
        tryCatch({
          gls_model <- gls(log_sindex ~ M_YEAR, data = data_subset)
          model_summary <- summary(gls_model)
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

# Close the progress bar
close(pb)

# Initialize a list to store the estimate summaries
estimate_summaries <- list()

# Iterate over each model summary
for (group_id in names(model_summaries_list)) {
  # Get the model summary
  model_summary <- model_summaries_list[[group_id]]
  
  # Extract relevant estimates
  estimate <- model_summary$coefficients["M_YEAR"]
  std_error <-  model_summary$tTable["M_YEAR", "Std.Error"]
  
  # Store the estimates in the list
  estimate_summaries[[group_id]] <- data.frame(
    SPECIES = model_summary$SPECIES,
    SITE_ID = model_summary$SITE_ID,
    estimate = estimate,
    std_error = std_error
  )
}

# Combine all matrices into a single data frame
estimate_summary <- do.call(rbind, estimate_summaries)

# Show results
print(estimate_summary)
hist(estimate_summary$estimate)

# Save results
file_path <- "E:/URBAN TRENDS/BMS data/BMS DATA 2024/butterfly_population_trends.csv"
write_csv(estimate_summary, file_path)






