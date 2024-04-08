## Clean enviroment and set directory to files
rm(list=ls())
setwd("D:/URBAN TRENDS/BMS data/BMS DATA 2024")  

# Libraries required
library(dplyr)

# Data
sindex <- read.csv("sindex_results.csv", sep=",", dec=".")

# --- Calculating butterfly annual population growth --- #

#Filtering butterfly abundance temporal series with a minimum of years and positive values
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

# Assuming sindex_filt is your dataset
pop_growth <- sindex_filt %>%
  arrange(SPECIES, SITE_ID, M_YEAR) %>%
  group_by(SPECIES, SITE_ID) %>%
  mutate(
    Previous_SINDEX = lag(SINDEX),
    Growth_Rate = log((SINDEX + 1) / (Previous_SINDEX + 1))
  ) %>%
  select(-Previous_SINDEX) # Removing the temporary column

# View the results
head(pop_growth)
