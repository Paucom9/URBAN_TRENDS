###### Create final dataset for stastical analyses ######

# Remove all objects from the current R session to ensure a clean working environment
rm(list = ls()) 

#Libraries required
library(dplyr)
library(tidyr)

# Define the base paths for each data category
base_path <- "E:/URBAN TRENDS"
bms_data_path <- file.path(base_path, "BMS data", "BMS DATA 2024")
urban_data_path <- file.path(base_path, "Urbanisation data")
traits_data_path <- file.path(base_path, "Traits data")
clim_data_path <- file.path(base_path, "Climate data")

# Function to read CSV with common settings, reduces repetition
read_csv_custom <- function(file_path, sep=",", dec=".") {
  read.csv(file_path, sep=sep, dec=dec)
}

# Read files using full paths
butter_trends <- read_csv_custom(file.path(bms_data_path, "butterfly_population_trends.csv")) # Obtained from running "butterfly_population_trends.R"
rclim <- read_csv_custom(file.path(bms_data_path, "ebms_transect_climate.csv")) # Associated climate region (Metzguer et al. 2013) of each transect_id
ebms_coord_df <- read_csv_custom(file.path(bms_data_path, "ebms_transect_coord.csv")) # Transect coordinates
country_codes <- read_csv_custom(file.path(bms_data_path, "country_codes.csv"), sep=";") #Country codes of bms_id
urb_trends <- read_csv_custom(file.path(urban_data_path, "urb_trends.csv")) # Obtained from running "urbanization_trends.R"
mod_str_yr <- read_csv_custom(file.path(urban_data_path, "mod_str_yr.csv")) # Obtained from running "urbanization_trends.R"
traits <- read_csv_custom(file.path(traits_data_path, "traits_table.csv"), sep=";") # Trait data
STI <- read_csv_custom(file.path(traits_data_path, "STI.csv"), sep=";") # STI and STVI traits
clim_trends <- read_csv_custom(file.path(clim_data_path, "climate_trends.csv"))  # Obtained from running "climate_trends.R"

# Transform data frames
traits$Taxon <- gsub("_", " ", traits$Taxon) # Replace underscores in traits

mod_str_yr$urban_names <- as.factor(mod_str_yr$urban_names)

# Filtering out "WATER" and "UNKNOWN" categories
mod_str_yr <- mod_str_yr %>%
  filter(!urban_names %in% c("WATER", "UNKNOWN"))

mod_str_yr_unique <- mod_str_yr %>%
  group_by(SITE_ID) %>%
  summarise(code_urban = first(code_urban),
            urban_names = first(urban_names),
            .groups = 'drop')

STI_selected <- dplyr::select(STI, species.name..Kudrna.et.al..2011., temp.mean, temp.sd)
traits_selected <- dplyr::select(traits, 2, 10, 14, 26)

ebms_coord_df <- ebms_coord_df[, c("bms_id", "transect_id", "transect_lon", "transect_lat")]

# Preview the data
head(butter_trends)
head(urb_trends)
head(STI_selected)
head(traits_selected)

# Merge datasets
merged_data <- merge(butter_trends, urb_trends, by=c("SPECIES", "SITE_ID"), all=FALSE)
subset_data <- subset(merged_data, urb_variable == "sum_2000m")

# Ensure the column names used for joining are consistent across data frames
traits_selected <- dplyr::rename(traits_selected, SPECIES = Taxon)
STI_selected <- dplyr::rename(STI_selected, SPECIES = species.name..Kudrna.et.al..2011.)

# Perform the joins
final_df <- subset_data %>%
  left_join(traits_selected, by = "SPECIES") %>%
  left_join(STI_selected, by = "SPECIES") %>%
  left_join(rclim, by = c("SITE_ID" = "transect_id")) %>%
  left_join(mod_str_yr_unique, by = "SITE_ID") %>%
  left_join(ebms_coord_df, by = c("SITE_ID" = "transect_id")) %>%
  left_join(country_codes, by = "bms_id")

# Join climate trends
# Pivot the data frame to wide format
clim_trends_wide <- clim_trends %>%
  dplyr::select(-std_error) %>%  # Remove the std_error column
  tidyr::spread(key = clim_variable, value = clim_trend)  # Pivot clim_variable to columns

final_df <- left_join(final_df, clim_trends_wide, by = c("SPECIES", "SITE_ID"))

head(final_df) # View the final merged data frame

any(duplicated(final_df)) # Check duplicated values 

write.csv(final_df, "E:/URBAN TRENDS/final_df.csv", row.names = FALSE)




