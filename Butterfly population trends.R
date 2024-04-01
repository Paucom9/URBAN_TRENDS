## Clean enviroment and set directory to files
rm(list=ls())
setwd("D:/URBAN TRENDS/BMS data/BMS DATA 2024")  

#Libraries
library(dplyr)
library(tidyr)
library(purrr)
library(nlme)
library(broom)
library(broom.mixed)
library(readr)

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


# Adjusting the dataset before nesting
# Step 0: Center the data by SPECIES and SITE_ID
sindex_filt <- sindex_filt %>%
  group_by(SPECIES, SITE_ID) %>%
  mutate(centered_year = M_YEAR - mean(M_YEAR),
         log_sindex = log(SINDEX + 1),
         centered_log_sindex = log_sindex - mean(log_sindex)) %>%
  ungroup()  # Ensuring we return to the standard dataframe structure

# Step 1: Nest the centered data by SPECIES and SITE_ID
nested_data <- sindex_filt %>%
  group_by(SPECIES, SITE_ID) %>%
  nest()

# Step 2: Fit generalized least squares models for each combination and tidy the output
model_summaries <- nested_data %>%
  mutate(models = map(data, ~{
    # Ensure there's more than one row for gls to work
    if (nrow(.x) > 1) { 
      tryCatch({
        gls_model <- gls(centered_log_sindex ~ centered_year, data = .x,
                         correlation = corAR1(form = ~ centered_year))
        return(tidy(gls_model))
      }, error = function(e) {
        warning(sprintf("Model failed for SPECIES = '%s' and SITE_ID = '%s': %s",
                        .x$SPECIES[1], .x$SITE_ID[1], e$message))
        return(NULL)  # Return NULL if an error occurs, with a warning
      })
    } else {
      return(NULL)  # Returning NULL for groups with no valid data
    }
  })) %>%
  select(SPECIES, SITE_ID, models)

# Step 3: Extract and unnest the summaries
model_summaries <- model_summaries %>%
  unnest(models)

# Step 4: Filter for the estimate of M_YEAR only and select required columns
estimate_summary <- model_summaries %>%
  filter(term == "centered_year") %>%
  select(SPECIES, SITE_ID, estimate, std.error)

# This will give you a tibble with SPECIES, SITE_ID, the estimate of the slope (M_YEAR),
# and its standard error for each SPECIES*SITE_ID combination.
print(estimate_summary)

file_path <- "D:/URBAN TRENDS/BMS data/BMS DATA 2024/butterfly_population_trends.csv"

# Save the estimate_summary tibble as a CSV file
write_csv(estimate_summary, file_path)



