# Clean environment and set the working directory to the location of the data files
rm(list = ls())  # Remove all objects from the current R session to ensure a clean working environment
setwd("D:/URBAN TRENDS/Urbanisation data")

# Libraries required
library(tidyverse)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(lme4)
library(merTools)

# --> charge data from Urbanisation plots.R


# --- Exploring trends --- #

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


# Reshaping the population data to long format
pop_long <- pop_df %>%
  pivot_longer(
    cols = c(starts_with("point_"), starts_with("sum_500m_"), starts_with("sum_1000m_"), starts_with("sum_2000m_")),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)$"
  )

# Further reshape the data to have a single measurement column
pop_long <- pop_long %>%
  pivot_longer(cols = c(point, sum_500m, sum_1000m, sum_2000m),
               names_to = "variable",
               values_to = "value")

# Convert 'year' to numeric
pop_long <- pop_long %>%
  mutate(year = as.numeric(year))

# Ensure that 'transect_id' and 'bms_id' are factors
pop_long$transect_id <- factor(pop_long$transect_id)
pop_long$bms_id <- factor(pop_long$bms_id)


# plot trends

model_100 <- lmer(value ~ year + (1|bms_id/transect_id), data = filter(longer_df, variable == "point"))
model_500 <- lmer(value ~ year + (1|bms_id/transect_id), data = filter(longer_df, variable == "sum_500m"))
model_1000 <- lmer(value ~ year + (1|bms_id/transect_id), data = filter(longer_df, variable == "sum_1000m"))
model_2000 <- lmer(value ~ year + (1|bms_id/transect_id), data = filter(longer_df, variable == "sum_2000m"))

summary(model_point)

# Adjusting the model to include bms_id as a fixed effect and transect_id as a random effect
model_adjusted <- lmer(value ~ year + bms_id + (1|transect_id), data = filter(longer_df, variable == "point"), REML = FALSE)

# Example for a single bms_id - you would need to loop or apply this for each bms_id
new_data <- expand.grid(
  year = 1975:2025,
  bms_id = unique(longer_df$bms_id),  # All unique bms_id values
  transect_id = factor(levels(longer_df$transect_id)[1])  # Placeholder, considering the random effect structure
)

# Assuming model_point is your fitted model
predictions <- predict(model_adjusted, newdata = new_data, re.form = NA)

# Add predictions to new_data
new_data$predicted_value <- predictions

ggplot(new_data, aes(x = year, y = predicted_value, color = bms_id)) +
  geom_line() +  # Add line for predicted values
  theme_minimal() +
  labs(title = "Predicted Values Over Years by bms_id", x = "Year", y = "Predicted built_up surface") +
  scale_color_viridis_d()  # Use a distinct color palette for clarity





#######################################################################

# --- Plots with mean and desvest values --- #

# Reshape data to calculate stats for all variables
stats_df <- long_df %>%
  pivot_longer(cols = c(point, sum_500m, sum_1000m, sum_2000m), names_to = "variable", values_to = "value") %>%
  group_by(year, variable) %>%
  summarize(
    average_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(year = as.numeric(year))


ggplot(stats_df, aes(x = year, y = average_value, color = variable)) +
  geom_ribbon(aes(ymin = average_value - sd_value, ymax = average_value + sd_value, fill = variable), alpha = 0.2) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Average Value Over Years with Variability for All Variables",
    x = "Year",
    y = "Average Value",
    color = "Variable",
    fill = "Variable"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Individual plots for each variable
plot_point <- ggplot(stats_df %>% filter(variable == "point"), aes(x = year, y = average_value)) +
  geom_ribbon(aes(ymin = average_value - sd_value, ymax = average_value + sd_value), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue") +
  geom_point(size = 2, color = "blue") +
  labs(title = "Point") +
  theme_minimal()

plot_sum_500m <- ggplot(stats_df %>% filter(variable == "sum_500m"), aes(x = year, y = average_value)) +
  geom_ribbon(aes(ymin = average_value - sd_value, ymax = average_value + sd_value), fill = "green", alpha = 0.2) +
  geom_line(color = "green") +
  geom_point(size = 2, color = "green") +
  labs(title = "Sum 500m") +
  theme_minimal()

plot_sum_1000m <- ggplot(stats_df %>% filter(variable == "sum_1000m"), aes(x = year, y = average_value)) +
  geom_ribbon(aes(ymin = average_value - sd_value, ymax = average_value + sd_value), fill = "orange", alpha = 0.2) +
  geom_line(color = "orange") +
  geom_point(size = 2, color = "orange") +
  labs(title = "Sum 1000m") +
  theme_minimal()

plot_sum_2000m <- ggplot(stats_df %>% filter(variable == "sum_2000m"), aes(x = year, y = average_value)) +
  geom_ribbon(aes(ymin = average_value - sd_value, ymax = average_value + sd_value), fill = "red", alpha = 0.2) +
  geom_line(color = "red") +
  geom_point(size = 2, color = "red") +
  labs(title = "Sum 2000m") +
  theme_minimal()


# Combine the plots
combined_plot <- plot_point / plot_sum_500m / plot_sum_1000m / plot_sum_2000m

# Display the combined plot
combined_plot


# --- Identify the temporal series of each species-site combination on sindex --- #

setwd("D:/URBAN TRENDS/sindex_results")
sindex_df <- read.csv("results_R2_G._Cold_and_mesic_Pieris_rapae.csv", sep = ",", dec = ".")

# Transform variables
sindex_df$SPECIES <- factor(sindex_df$SPECIES)
sindex_df$SITE_ID <- factor(sindex_df$SITE_ID)

# 
aggregated_df <- sindex_df %>%
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

# Filter by a minum number of years

filtered_aggregated_df <- aggregated_df %>%
  filter(N_YEARS >= 8, S_YEARS >= 5)





# Ensure built_long$transect_id is of the same type as filtered_aggregated_df$SITE_ID
built_long$transect_id <- as.character(built_long$transect_id)

# Adjusted function to perform the filtering, model fitting, and exclude intercepts
perform_analysis <- function(site_id, start_year, end_year) {
  temp_data <- built_long %>%
    filter(transect_id == site_id, year >= start_year, year <= end_year)
  
  # Proceed if temp_data is not empty
  if (nrow(temp_data) > 0) {
    models <- temp_data %>%
      nest(data = c(year, value)) %>%
      mutate(model = map(data, ~ tidy(lm(value ~ year, data = .x)))) %>%
      select(variable, model) %>%
      unnest(model)
    
    # Filter out intercepts from the models
    models <- models %>%
      filter(term != "(Intercept)")
    
    return(models)
  } else {
    # Return an empty tibble if no data is available
    return(tibble())
  }
}

# Apply the function to each row in filtered_aggregated_df
results <- filtered_aggregated_df %>%
  rowwise() %>%
  mutate(models = list(perform_analysis(SITE_ID, STR_YEAR, END_YEAR))) %>%
  unnest(cols = models) %>%
  ungroup()

# Inspect the results
print(results)





# --- Analyse long-term shifts in built-up fraction and population number --- #

head(built_long)
head(pop_long)
pop_long <- na.omit(pop_long)

# Perform regression analysis for each transect_id and variable
regression_results_built <- built_long %>%
  group_by(transect_id, variable) %>%
  do(model = lm(value ~ year, data = .)) %>%
  ungroup() %>%
  mutate(tidy_model = map(model, tidy)) %>%
  select(-model) %>%
  unnest(tidy_model)

regression_results_pop <- pop_long %>%
  group_by(transect_id, variable) %>%
  do(model = lm(value ~ year, data = .)) %>%
  ungroup() %>%
  mutate(tidy_model = map(model, tidy)) %>%
  select(-model) %>%
  unnest(tidy_model)


# Convert the results into a data table format (if you specifically want a 'data.table')
regression_results_built_dt <- as.data.table(regression_results_built)
regression_results_pop_dt <- as.data.table(regression_results_pop)

# Filter out the intercept rows
regression_results_built_dt <- regression_results_built_dt[term != "(Intercept)"]
regression_results_pop_dt <- regression_results_pop_dt[term != "(Intercept)"]

# Remove the 'term' column
regression_results_built_dt[, term := NULL]
regression_results_pop_dt[, term := NULL]

# Create a mapping of transect_id to longitude and latitude
setDT(built_long)
lon_lat_mapping <- unique(built_long[, .(longitude, latitude), by = transect_id])

# Merge the mapping with regression results
regression_results_built_dt <- merge(regression_results_built_dt, lon_lat_mapping, by = "transect_id", all.x = TRUE)
regression_results_pop_dt <- merge(regression_results_pop_dt, lon_lat_mapping, by = "transect_id", all.x = TRUE)

# Convert these points to sf objects using projection EPSG:3035)
built_sf <- st_as_sf(regression_results_built_dt, coords = c("longitude", "latitude"), crs = 3035)
pop_sf <- st_as_sf(regression_results_pop_dt, coords = c("longitude", "latitude"), crs = 3035)




#Plot the maps

 built_plot<- ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white") + # Draw countries
   geom_sf(data = subset(built_sf, variable == "point"), aes(color = estimate), size = 1) + # Filter and color by estimate
   scale_color_gradientn(colors = c("green", "yellow", "red"),
                        values = scales::rescale(c(0, 0.01, 0.5, 1))) + # Define a continuous color scale  labs(
  labs(title = "Built-up fraction (1975-2025 trend)",
         color = "Estimate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5), # Increase and center title size
  ) +
  coord_sf(crs = st_crs(3035), xlim = c(1602962, 5366783), ylim = c(1000000, 5100000))

 pop_plot<- ggplot() +
   geom_sf(data = world, fill = "lightgrey", color = "white") + # Draw countries
   geom_sf(data = subset(pop_sf, variable == "point"), aes(color = estimate), size = 1) + # Filter and color by estimate
   scale_color_gradientn(colors = c("green", "yellow", "red"),
                         values = scales::rescale(c(0, 0.01, 0.5, 1))) + # Define a continuous color scale  labs(
   labs(title = "Human population number (1975-2025 trend)",
        color = "Estimate"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(size = 14, hjust = 0.5), # Increase and center title size
   ) +
   coord_sf(crs = st_crs(3035), xlim = c(1602962, 5366783), ylim = c(1000000, 5100000))
 
 
combined_plot <- pop_plot + built_plot + plot_layout(ncol = 2)
combined_plot <- combined_plot + plot_annotation(title = "100x100m", 
                                                 theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
combined_plot




