## Clean enviroment and set directory to files
rm(list=ls())
setwd("D:/URBAN TRENDS/BMS data")

#Libraries
library(dplyr)
library(purrr)
library(lmtest)
library(forecast)
library(dplyr)
library(nlme)
library(broom)
library(purrr)
library(lmtest)
library(tidyr)


sindex <- read.csv("SINDEX_CAT.csv", sep=",", dec=".")
head(sindex)

#Filtering temporal series with a minimum of years and positive values

sindex_filt <- sindex %>%
  group_by(SPECIES, SITE_ID, RCLIM) %>%
  # First, make sure that there are at least 10 different years of data
  filter(n_distinct(M_YEAR) >= 10) %>%
  # Then, for each group, calculate the percentage of years with SINDEX < 1
  mutate(porcentaje_bajo_1 = sum(SINDEX < 1, na.rm = TRUE) / n()) %>%
  # Filter to keep only those groups where less than half of the years have SINDEX < 1
  filter(porcentaje_bajo_1 <= 0.5) %>%
  # Remove the auxiliary column used for the calculation
  select(-porcentaje_bajo_1) %>%
  ungroup()

head(sindex_filt)

library(dplyr)
library(broom)
library(tibble) # For tibble data structures


# Step 1: Nest the data by SPECIES and SITE_ID
nested_data <- sindex_filt %>%
  group_by(SPECIES, SITE_ID) %>%
  nest()

# Step 2: Fit linear models for each combination and tidy the output
model_summaries <- nested_data %>%
  mutate(models = map(data, ~lm(SINDEX ~ M_YEAR, data = .x)),
         tidied = map(models, tidy)) %>%
  select(SPECIES, SITE_ID, tidied)

# Step 3: Extract and unnest the summaries
tidy_summaries <- model_summaries %>%
  unnest(tidied)

# Step 4: Filter for the estimate of M_YEAR only and select required columns
estimate_summary <- tidy_summaries %>%
  filter(term == "M_YEAR") %>%
  select(SPECIES, SITE_ID, estimate, std.error)

# This will give you a tibble with SPECIES, SITE_ID, the estimate of the slope (M_YEAR),
# and its standard error for each SPECIES*SITE_ID combination.
print(estimate_summary)



