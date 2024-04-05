

setwd("D:/URBAN TRENDS/Climate data") 

# Data

clim_a <- read.csv("climate_a.csv", sep = ",", dec = ".")
clim_b <- read.csv("climate_b.csv", sep = ",", dec = ".")
clim_c <- read.csv("climate_c.csv", sep = ",", dec = ".")

clim_df <- rbind(clim_a, clim_b, clim_c)


# # --- Identify the temporal series of each species-site combination of sindex_results --- #

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

# --- Calculate climate trends between STR_YEAR AND END_YEAR for each SPECIES-SITE temporal series --- #


# Prepare an empty data frame to store results
results_df <- data.frame(
  SPECIES = character(),
  SITE_ID = character(),
  clim_variable = character(),
  STR_YEAR = integer(),
  END_YEAR = integer(),
  N_YEARS = integer(),
  clim_trend = numeric(),
  std_error = numeric(),
  stringsAsFactors = FALSE  # To keep character columns as characters
)

# Specified climatic variables
clim_vars <- c("bio1", "bio4", "bio12", "bio15", "PSMAO", "GDD5", "DMA")

# Loop through each row in sindex_yrs
for (i in 1:nrow(sindex_yrs)) {
  row <- sindex_yrs[i, ]
  
  # Subset clim_df for the years of interest
  clim_subset <- subset(clim_df, Year >= row$STR_YEAR & Year <= row$END_YEAR)
  
  # For each specified climatic variable
  for (var in clim_vars) {
    # Ensure the variable exists in clim_df
    if (var %in% names(clim_subset)) {
      # Conduct linear model: var ~ Year
      model <- lm(formula = paste(var, "~ Year"), data = clim_subset)
      
      # Extract slope (coefficient of Year) and standard error
      slope <- coef(model)["Year"]
      std_error <- summary(model)$coefficients["Year", "Std. Error"]
      
      # Append results to the results_df
      results_df <- rbind(results_df, data.frame(
        SPECIES = row$SPECIES,
        SITE_ID = row$SITE_ID,
        clim_variable = var,
        STR_YEAR = row$STR_YEAR,
        END_YEAR = row$END_YEAR,
        N_YEARS = row$N_YEARS,
        clim_trend = slope,
        std_error = std_error
      ))
    }
  }
}

# View the structure of the results data frame
str(results_df)


output_file_path <- "D:/URBAN TRENDS/BMS data/BMS DATA 2024/ebms_coord_elevation_finalsites3.csv"

# Save the combined data table to the specified file
fwrite(data_frame_2, output_file_path)
