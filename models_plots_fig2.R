###### Analyses on the role of species traits to explain urbanization and climate effects --- Figure 2 ---  ######

# Remove all objects from the current R session to ensure a clean working environment
rm(list = ls())  

# Required libraries

library(glmmTMB)
library(ggplot2)
library(patchwork)
library(ggeffects)
library(extrafont)
library(dplyr)

# Import data

setwd("E:/URBAN TRENDS")  

final_df <- read.csv("final_df.csv", sep = ",", dec = ".")

head(final_df)
str(final_df)

# Variables preparation
final_df$inverse_variance_weights <- 1 / (final_df$std.error^2)# Inverse of the variance (the higher value the higher precision of the estimate)
final_df$estimate <- as.numeric(final_df$estimate) # Butterfly population trend
final_df$HSI <- as.numeric(final_df$HSI) # Host plant index
final_df$WIn <- as.numeric(final_df$WIn) # Wing index
final_df$temp.mean <- as.numeric(final_df$temp.mean) # STI
final_df$temp.sd <- as.numeric(final_df$temp.sd) # STVI
final_df$bio1 <- as.numeric(final_df$bio1) # Temperature trend
final_df$bio12 <- as.numeric(final_df$bio12) # Precipitation trend
final_df$DMA <- as.numeric(final_df$DMA) # De Martonne index trend
final_df$DMA_inverted <- -final_df$DMA # Aridity index trend
final_df$SPECIES <- as.factor(final_df$SPECIES) 
final_df$SITE_ID <- as.factor(final_df$SITE_ID)
final_df$Country.Name <- as.factor(final_df$Country.Name)
final_df$genzname <- as.factor(final_df$genzname) # Climate region
final_df$urban_names <- as.factor(final_df$urban_names) 
final_df <- final_df %>%  # Create a binomial variable for urban_names
  mutate(urban_type = case_when(
    urban_names %in% c("URBAN CENTRE", "SUBURBAN OR PERI-URBAN", "DENSE URBAN CLUSTER", "SEMI-DENSE URBAN CLUSTER") ~ "Urban",
    urban_names %in% c("LOW DENSITY RURAL", "VERY LOW DENSITY RURAL", "RURAL CLUSTER GRID") ~ "Rural",
    TRUE ~ NA_character_ # Handles <NA> values or any other unexpected cases
  ))

# Data subsets

final_df_clean <- na.omit(final_df)
final_df_rural <- subset(final_df_clean, final_df_clean$urban_type == "Rural")
final_df_urban <- subset(final_df_clean, final_df_clean$urban_type == "Urban")


# --- Models and plots --- #

##### Urbanization effects ####

#  Interaction urbanization * host plant specialization #

# Rural

mod_hsi_rural <-  glmmTMB(estimate ~ scale(urb_trend)*scale(HSI)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)
summary(mod_hsi_rural)

# Generate predictions
pred_values <- seq(0, 1, by = 0.05)
predictions <- ggpredict(mod_hsi_rural, terms = c("urb_trend", paste("HSI [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$HSI <- as.numeric(as.character(predictions$group))

# Create the plot
plot_hsi_rural <- ggplot(predictions, aes(x = x, y = predicted, group = HSI)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = HSI), alpha = 0.1) +
  geom_line(aes(color = HSI), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(0, 0.06, by = 0.02), labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(breaks = seq(-0.1, 0.05, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  labs(title = "(a) Urbanization - Rural" , x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Garamond", size = 15),
    plot.subtitle = element_text(hjust = 0.5, family = "Garamond", size = 10),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

print(plot_hsi_rural)


# Urban

mod_hsi_urban <-  glmmTMB(estimate ~ scale(urb_trend)*scale(HSI)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)
summary(mod_hsi_urban)

# Generate predictions

pred_values <- seq(0, 1, by = 0.05)
predictions <- ggpredict(mod_hsi_urban, terms = c("urb_trend", paste("HSI [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$HSI <- as.numeric(as.character(predictions$group))


# Create the plot
plot_hsi_urban <- ggplot(predictions, aes(x = x, y = predicted, group = HSI)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = HSI), alpha = 0.1) +
  geom_line(aes(color = HSI), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(0, 0.06, by = 0.01), labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  labs(title = "(b) Urbanization - Urban", x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Garamond", size = 15),
    plot.subtitle = element_text(hjust = 0.5, family = "Garamond", size = 10),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )


print(plot_hsi_urban)


#  Interaction urbanization * wing index #

# Rural

mod_win_rural <-  glmmTMB(estimate ~ scale(urb_trend)*scale(WIn)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_win_rural)


# Generate predictions
pred_values <- seq(-0.05, 0.15, by = 0.01)
predictions <- ggpredict(mod_win_rural, terms = c("urb_trend", paste("WIn [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$WIn <- as.numeric(as.character(predictions$group))


# Create the plot
plot_win_rural <- ggplot(predictions, aes(x = x, y = predicted, group = WIn)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = WIn), alpha = 0.1) +
  geom_line(aes(color = WIn), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.025), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_win_rural)


# Urban

mod_win_urban <-  glmmTMB(estimate ~ scale(urb_trend)*scale(WIn)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_win_urban)

# Generate predictions
pred_values <- seq(-0.05, 0.15, by = 0.01)
predictions <- ggpredict(mod_win_urban, terms = c("urb_trend", paste("WIn [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$WIn <- as.numeric(as.character(predictions$group))

# Create the plot
plot_win_urban <- ggplot(predictions, aes(x = x, y = predicted, group = WIn)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = WIn), alpha = 0.1) +
  geom_line(aes(color = WIn), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_win_urban)


#  Interaction urbanization * flight month average #

# Rural

mod_fmo_rural <-  glmmTMB(estimate ~ scale(urb_trend)*scale(FMo_Average)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_fmo_rural)


# Generate predictions
pred_values <- seq(2, 9.5, by = 0.375)
predictions <- ggpredict(mod_fmo_rural, terms = c("urb_trend", paste("FMo_Average [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$FMo_Average <- as.numeric(as.character(predictions$group))


# Create the plot
plot_fmo_rural <- ggplot(predictions, aes(x = x, y = predicted, group = FMo_Average)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = FMo_Average), alpha = 0.1) +
  geom_line(aes(color = FMo_Average), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(0, 0.06, by = 0.02), labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  labs(x = "", y = "Butterfly population trend") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 12),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_fmo_rural)


# Urban

mod_fmo_urban <-  glmmTMB(estimate ~ scale(urb_trend)*scale(FMo_Average)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_fmo_urban)


# Generate predictions
pred_values <- seq(2, 9.5, by = 0.375)
predictions <- ggpredict(mod_fmo_urban, terms = c("urb_trend", paste("FMo_Average [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$FMo_Average <- as.numeric(as.character(predictions$group))


# Create the plot
plot_fmo_urban <- ggplot(predictions, aes(x = x, y = predicted, group = FMo_Average)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = FMo_Average), alpha = 0.1) +
  geom_line(aes(color = FMo_Average), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_fmo_urban)


#  Interaction urbanization * STI #

# Rural

mod_tmean_rural <-  glmmTMB(estimate ~ scale(urb_trend)*scale(temp.mean)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                            data = final_df_rural,
                            weights = inverse_variance_weights,
                            family = gaussian)

summary(mod_tmean_rural)

# Generate predictions
pred_values <- seq(3.5, 15, by = 0.25)
predictions <- ggpredict(mod_tmean_rural, terms = c("urb_trend", paste("temp.mean [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.mean <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tmean_rural <- ggplot(predictions, aes(x = x, y = predicted, group = temp.mean)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.mean), alpha = 0.1) +
  geom_line(aes(color = temp.mean), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(0, 0.06, by = 0.02), labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tmean_rural)


# Urban

mod_tmean_urban <-  glmmTMB(estimate ~ scale(urb_trend)*scale(temp.mean)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                            data = final_df_urban,
                            weights = inverse_variance_weights,
                            family = gaussian)

summary(mod_tmean_urban)

# Generate predictions
pred_values <- seq(3.5, 15, by = 0.25)
predictions <- ggpredict(mod_tmean_urban, terms = c("urb_trend", paste("temp.mean [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.mean <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tmean_urban <- ggplot(predictions, aes(x = x, y = predicted, group = temp.mean)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.mean), alpha = 0.1) +
  geom_line(aes(color = temp.mean), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tmean_urban)


#  Interaction urbanization * STVI #

# Rural

mod_tsd_rural <-  glmmTMB(estimate ~ scale(urb_trend)*scale(temp.sd)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_tsd_rural)

# Generate predictions
pred_values <- seq(1, 4, by = 0.2)
predictions <- ggpredict(mod_tsd_rural, terms = c("urb_trend", paste("temp.sd [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.sd <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tsd_rural <- ggplot(predictions, aes(x = x, y = predicted, group = temp.sd)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.sd), alpha = 0.1) +
  geom_line(aes(color = temp.sd), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(0, 0.06, by = 0.02), labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  labs(x = "Urbanization trend", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 12),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tsd_rural)


# Urban

mod_tsd_urban <-  glmmTMB(estimate ~ scale(urb_trend)*scale(temp.sd)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_tsd_urban)

# Generate predictions
pred_values <- seq(1, 4, by = 0.2)
predictions <- ggpredict(mod_tsd_urban, terms = c("urb_trend", paste("temp.sd [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.sd <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tsd_urban <- ggplot(predictions, aes(x = x, y = predicted, group = temp.sd)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.sd), alpha = 0.1) +
  geom_line(aes(color = temp.sd), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  labs(x = "Urbanization trend", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 12),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )


print(plot_tsd_urban)


# Remove legends from rural plots
remove_legend <- function(plot) {
  plot + theme(legend.position = "none")
}

# Apply remove_legend function to rural plots
plot_hsi_rural <- remove_legend(plot_hsi_rural)
plot_win_rural <- remove_legend(plot_win_rural)
plot_fmo_rural <- remove_legend(plot_fmo_rural)
plot_tmean_rural <- remove_legend(plot_tmean_rural)
plot_tsd_rural <- remove_legend(plot_tsd_rural)
plot_hsi_urban <- remove_legend(plot_hsi_urban)
plot_win_urban<- remove_legend(plot_win_urban)
plot_fmo_urban<- remove_legend(plot_fmo_urban)
plot_tmean_urban<- remove_legend(plot_tmean_urban)
plot_tsd_urban<- remove_legend(plot_tsd_urban)

# Function to highlight specific plots
highlight_plot <- function(plot) {
  plot + theme(plot.margin = margin(10, 10, 10, 10), 
               panel.border = element_rect(colour = "red", fill = NA, size = 2))
}

plot_tsd_rural <- highlight_plot(plot_tsd_rural)
plot_win_urban <- highlight_plot(plot_win_urban)

fig2_urb <- (plot_hsi_rural | plot_hsi_urban) / 
  (plot_win_rural | plot_win_urban) / 
  (plot_fmo_rural | plot_fmo_urban) / 
  (plot_tmean_rural | plot_tmean_urban) / 
  (plot_tsd_rural | plot_tsd_urban) + 
  plot_layout(guides = 'keep', widths = c(1, 1)) &
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1))



# Display the combined plot
print(fig2_urb)


##### Temperature trend effects ####

#  Interaction temperature trend * host plant specialization #

# Rural

mod_hsi_rural <-  glmmTMB(estimate ~ scale(bio1)*scale(HSI)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_hsi_rural)

# Generate predictions
pred_values <- seq(0, 1, by = 0.05)
predictions <- ggpredict(mod_hsi_rural, terms = c("bio1", paste("HSI [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$HSI <- as.numeric(as.character(predictions$group))


# Create the plot
plot_hsi_rural <- ggplot(predictions, aes(x = x, y = predicted, group = HSI)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = HSI), alpha = 0.1) +
  geom_line(aes(color = HSI), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-0.1, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  labs(title = "(c) Temperature - Rural" , x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Garamond", size = 15),
    plot.subtitle = element_text(hjust = 0.5, family = "Garamond", size = 10),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

print(plot_hsi_rural)


# Urban

mod_hsi_urban <-  glmmTMB(estimate ~ scale(bio1)*scale(HSI)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_hsi_urban)

# Generate predictions
pred_values <- seq(0, 1, by = 0.05)
predictions <- ggpredict(mod_hsi_urban, terms = c("bio1", paste("HSI [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$HSI <- as.numeric(as.character(predictions$group))


# Create the plot
plot_hsi_urban <- ggplot(predictions, aes(x = x, y = predicted, group = HSI)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = HSI), alpha = 0.1) +
  geom_line(aes(color = HSI), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-0.1, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.2), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  labs(title = "(d) Temperature - Urban", x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Garamond", size = 15),
    plot.subtitle = element_text(hjust = 0.5, family = "Garamond", size = 10),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )


print(plot_hsi_urban)


#  Interaction temperature trend * wing index #

## Rural

mod_win_rural <-  glmmTMB(estimate ~ scale(bio1)*scale(WIn)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_win_rural)

# Generate predictions
pred_values <- seq(-0.05, 0.15, by = 0.01)
predictions <- ggpredict(mod_win_rural, terms = c("bio1", paste("WIn [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$WIn <- as.numeric(as.character(predictions$group))


# Create the plot
plot_win_rural <- ggplot(predictions, aes(x = x, y = predicted, group = WIn)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = WIn), alpha = 0.1) +
  geom_line(aes(color = WIn), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-0.1, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_win_rural)

# Urban

mod_win_urban <-  glmmTMB(estimate ~ scale(bio1)*scale(WIn)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_win_urban)


# Generate predictions
pred_values <- seq(-0.05, 0.15, by = 0.01)
predictions <- ggpredict(mod_win_urban, terms = c("bio1", paste("WIn [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$WIn <- as.numeric(as.character(predictions$group))


# Create the plot
plot_win_urban <- ggplot(predictions, aes(x = x, y = predicted, group = WIn)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = WIn), alpha = 0.1) +
  geom_line(aes(color = WIn), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-0.1, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.2), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_win_urban)


#  Interaction temperature trend * flight month average #

# Rural

mod_fmo_rural <-  glmmTMB(estimate ~ scale(bio1)*scale(FMo_Average)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_fmo_rural)

# Generate predictions
pred_values <- seq(2, 9.5, by = 0.375)
predictions <- ggpredict(mod_fmo_rural, terms = c("bio1", paste("FMo_Average [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$FMo_Average <- as.numeric(as.character(predictions$group))


# Create the plot
plot_fmo_rural <- ggplot(predictions, aes(x = x, y = predicted, group = FMo_Average)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = FMo_Average), alpha = 0.1) +
  geom_line(aes(color = FMo_Average), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-0.1, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_fmo_rural)


# Urban

mod_fmo_urban <-  glmmTMB(estimate ~ scale(bio1)*scale(FMo_Average)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_fmo_urban)

# Generate predictions
pred_values <- seq(2, 9.5, by = 0.375)
predictions <- ggpredict(mod_fmo_urban, terms = c("bio1", paste("FMo_Average [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$FMo_Average <- as.numeric(as.character(predictions$group))


# Create the plot
plot_fmo_urban <- ggplot(predictions, aes(x = x, y = predicted, group = FMo_Average)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = FMo_Average), alpha = 0.1) +
  geom_line(aes(color = FMo_Average), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-0.1, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.2), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_fmo_urban)


#  Interaction temperature trend * STI #

# Rural

mod_tmean_rural <-  glmmTMB(estimate ~ scale(bio1)*scale(temp.mean)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                            data = final_df_rural,
                            weights = inverse_variance_weights,
                            family = gaussian)

summary(mod_tmean_rural)

# Generate predictions
pred_values <- seq(3.5, 15, by = 0.25)
predictions <- ggpredict(mod_tmean_rural, terms = c("bio1", paste("temp.mean [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.mean <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tmean_rural <- ggplot(predictions, aes(x = x, y = predicted, group = temp.mean)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.mean), alpha = 0.1) +
  geom_line(aes(color = temp.mean), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-0.1, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tmean_rural)

# Urban

mod_tmean_urban <-  glmmTMB(estimate ~ scale(bio1)*scale(temp.mean)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                            data = final_df_urban,
                            weights = inverse_variance_weights,
                            family = gaussian)

summary(mod_tmean_urban)

# Generate predictions
pred_values <- seq(3.5, 15, by = 0.25)
predictions <- ggpredict(mod_tmean_urban, terms = c("bio1", paste("temp.mean [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.mean <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tmean_urban <- ggplot(predictions, aes(x = x, y = predicted, group = temp.mean)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.mean), alpha = 0.1) +
  geom_line(aes(color = temp.mean), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-0.1, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.2), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tmean_urban)


# Rural

mod_tsd_rural <-  glmmTMB(estimate ~ scale(bio1)*scale(temp.sd)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_tsd_rural)

# Generate predictions
pred_values <- seq(1, 4, by = 0.2)
predictions <- ggpredict(mod_tsd_rural, terms = c("bio1", paste("temp.sd [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.sd <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tsd_rural <- ggplot(predictions, aes(x = x, y = predicted, group = temp.sd)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.sd), alpha = 0.1) +
  geom_line(aes(color = temp.sd), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-0.1, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  labs(x = "Temperature trend", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 12),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tsd_rural)


# Urban

mod_tsd_urban <-  glmmTMB(estimate ~ scale(bio1)*scale(temp.sd)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_tsd_urban)

# Generate predictions
pred_values <- seq(1, 4, by = 0.2)
predictions <- ggpredict(mod_tsd_urban, terms = c("bio1", paste("temp.sd [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.sd <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tsd_urban <- ggplot(predictions, aes(x = x, y = predicted, group = temp.sd)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.sd), alpha = 0.1) +
  geom_line(aes(color = temp.sd), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-0.1, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.2), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  labs(x = "Temperature trend", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 12),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tsd_urban)


# Remove legends from rural plots
remove_legend <- function(plot) {
  plot + theme(legend.position = "none")
}

# Apply remove_legend function to rural plots
plot_hsi_rural <- remove_legend(plot_hsi_rural)
plot_win_rural <- remove_legend(plot_win_rural)
plot_fmo_rural <- remove_legend(plot_fmo_rural)
plot_tmean_rural <- remove_legend(plot_tmean_rural)
plot_tsd_rural <- remove_legend(plot_tsd_rural)
plot_win_urban <- remove_legend(plot_win_urban)
plot_fmo_urban <- remove_legend(plot_fmo_urban)
plot_tmean_urban <- remove_legend(plot_tmean_urban)
plot_tsd_urban <- remove_legend(plot_tsd_urban)
plot_hsi_urban <- remove_legend(plot_hsi_urban)


# Function to highlight specific plots
highlight_plot <- function(plot) {
  plot + theme(plot.margin = margin(10, 10, 10, 10), 
               panel.border = element_rect(colour = "red", fill = NA, size = 2))
}

plot_tmean_rural <- highlight_plot(plot_tmean_rural)
plot_tmean_urban <- highlight_plot(plot_tmean_urban)


fig2_bio1 <- (plot_hsi_rural | plot_hsi_urban) / 
  (plot_win_rural | plot_win_urban) / 
  (plot_fmo_rural | plot_fmo_urban) / 
  (plot_tmean_rural | plot_tmean_urban) / 
  (plot_tsd_rural | plot_tsd_urban) + 
  plot_layout(guides = 'keep', widths = c(1, 1)) &
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1))



# Display the combined plot
print(fig2_bio1)



##### Precipitation trend effects ####

#  Interaction precipitation trend * host plant specialization #

# Rural

mod_hsi_rural <-  glmmTMB(estimate ~ scale(bio12)*scale(HSI)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_hsi_rural)

# Generate predictions
pred_values <- seq(0, 1, by = 0.05)
predictions <- ggpredict(mod_hsi_rural, terms = c("bio12", paste("HSI [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$HSI <- as.numeric(as.character(predictions$group))


# Create the plot
plot_hsi_rural <- ggplot(predictions, aes(x = x, y = predicted, group = HSI)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = HSI), alpha = 0.1) +
  geom_line(aes(color = HSI), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-10, 15, by = 5), labels = scales::number_format(accuracy = 1.0)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  labs(title = "(e) Precipitation - Rural" , x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Garamond", size = 15),
    plot.subtitle = element_text(hjust = 0.5, family = "Garamond", size = 10),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

print(plot_hsi_rural)


# Urban 

mod_hsi_urban <-  glmmTMB(estimate ~ scale(bio12)*scale(HSI)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_hsi_urban)

# Generate predictions
pred_values <- seq(0, 1, by = 0.05)
predictions <- ggpredict(mod_hsi_urban, terms = c("bio12", paste("HSI [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$HSI <- as.numeric(as.character(predictions$group))

# Create the plot
plot_hsi_urban <- ggplot(predictions, aes(x = x, y = predicted, group = HSI)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = HSI), alpha = 0.1) +
  geom_line(aes(color = HSI), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-10, 15, by = 5), labels = scales::number_format(accuracy = 1.0)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  labs(title = "(f) Precipitation - Urban", x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Garamond", size = 15),
    plot.subtitle = element_text(hjust = 0.5, family = "Garamond", size = 10),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )


print(plot_hsi_urban)


#  Interaction precipitation trend * wing index #

# Rural

mod_win_rural <-  glmmTMB(estimate ~ scale(bio12)*scale(WIn)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_win_rural)

# Generate predictions
pred_values <- seq(-0.05, 0.15, by = 0.01)
predictions <- ggpredict(mod_win_rural, terms = c("bio12", paste("WIn [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$WIn <- as.numeric(as.character(predictions$group))


# Create the plot
plot_win_rural <- ggplot(predictions, aes(x = x, y = predicted, group = WIn)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = WIn), alpha = 0.1) +
  geom_line(aes(color = WIn), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-10, 15, by = 5), labels = scales::number_format(accuracy = 1.0)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_win_rural)

# Urban

mod_win_urban <-  glmmTMB(estimate ~ scale(bio12)*scale(WIn)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_win_urban)

# Generate predictions
pred_values <- seq(-0.05, 0.15, by = 0.01)
predictions <- ggpredict(mod_win_urban, terms = c("bio12", paste("WIn [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$WIn <- as.numeric(as.character(predictions$group))


# Create the plot
plot_win_urban <- ggplot(predictions, aes(x = x, y = predicted, group = WIn)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = WIn), alpha = 0.1) +
  geom_line(aes(color = WIn), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-10, 15, by = 5), labels = scales::number_format(accuracy = 1.0)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_win_urban)


#  Interaction precipitation trend * flight month average #

# Rural

mod_fmo_rural <-  glmmTMB(estimate ~ scale(bio12)*scale(FMo_Average)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_fmo_rural)

# Generate predictions
pred_values <- seq(2, 9.5, by = 0.375)
predictions <- ggpredict(mod_fmo_rural, terms = c("bio12", paste("FMo_Average [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$FMo_Average <- as.numeric(as.character(predictions$group))


# Create the plot
plot_fmo_rural <- ggplot(predictions, aes(x = x, y = predicted, group = FMo_Average)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = FMo_Average), alpha = 0.1) +
  geom_line(aes(color = FMo_Average), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-10, 15, by = 5), labels = scales::number_format(accuracy = 1.0)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_fmo_rural)


# Urban

mod_fmo_urban <-  glmmTMB(estimate ~ scale(bio12)*scale(FMo_Average)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_fmo_urban)

# Generate predictions
pred_values <- seq(2, 9.5, by = 0.375)
predictions <- ggpredict(mod_fmo_urban, terms = c("bio12", paste("FMo_Average [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$FMo_Average <- as.numeric(as.character(predictions$group))

# Create the plot
plot_fmo_urban <- ggplot(predictions, aes(x = x, y = predicted, group = FMo_Average)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = FMo_Average), alpha = 0.1) +
  geom_line(aes(color = FMo_Average), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-10, 15, by = 5), labels = scales::number_format(accuracy = 1.0)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_fmo_urban)


#  Interaction precipitation trend * STI #

# Rural

mod_tmean_rural <-  glmmTMB(estimate ~ scale(bio12)*scale(temp.mean)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                            data = final_df_rural,
                            weights = inverse_variance_weights,
                            family = gaussian)

summary(mod_tmean_rural)

# Generate predictions
pred_values <- seq(3.5, 15, by = 0.25)
predictions <- ggpredict(mod_tmean_rural, terms = c("bio12", paste("temp.mean [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.mean <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tmean_rural <- ggplot(predictions, aes(x = x, y = predicted, group = temp.mean)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.mean), alpha = 0.1) +
  geom_line(aes(color = temp.mean), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-10, 15, by = 5), labels = scales::number_format(accuracy = 1.0)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tmean_rural)

# Urban

mod_tmean_urban <-  glmmTMB(estimate ~ scale(bio12)*scale(temp.mean)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                            data = final_df_urban,
                            weights = inverse_variance_weights,
                            family = gaussian)

summary(mod_tmean_urban)

# Generate predictions
pred_values <- seq(3.5, 15, by = 0.25)
predictions <- ggpredict(mod_tmean_urban, terms = c("bio12", paste("temp.mean [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.mean <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tmean_urban <- ggplot(predictions, aes(x = x, y = predicted, group = temp.mean)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.mean), alpha = 0.1) +
  geom_line(aes(color = temp.mean), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-10, 15, by = 5), labels = scales::number_format(accuracy = 1.0)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tmean_urban)


#  Interaction precipitation trend * STVI #

# Rural

mod_tsd_rural <-  glmmTMB(estimate ~ scale(bio12)*scale(temp.sd)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_tsd_rural)

# Generate predictions
pred_values <- seq(1, 4, by = 0.2)
predictions <- ggpredict(mod_tsd_rural, terms = c("bio12", paste("temp.sd [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.sd <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tsd_rural <- ggplot(predictions, aes(x = x, y = predicted, group = temp.sd)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.sd), alpha = 0.1) +
  geom_line(aes(color = temp.sd), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-10, 15, by = 5), labels = scales::number_format(accuracy = 1.0)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  labs(x = "Precipitation trend", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 14),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tsd_rural)


# Urban

mod_tsd_urban <-  glmmTMB(estimate ~ scale(bio12)*scale(temp.sd)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_tsd_urban)

# Generate predictions
pred_values <- seq(1, 4, by = 0.2)
predictions <- ggpredict(mod_tsd_urban, terms = c("bio12", paste("temp.sd [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.sd <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tsd_urban <- ggplot(predictions, aes(x = x, y = predicted, group = temp.sd)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.sd), alpha = 0.1) +
  geom_line(aes(color = temp.sd), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-10, 15, by = 5), labels = scales::number_format(accuracy = 1.0)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  labs(x = "Precipitation trend", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 12),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tsd_urban)


# Remove legends from rural plots
remove_legend <- function(plot) {
  plot + theme(legend.position = "none")
}

# Apply remove_legend function to rural plots
plot_hsi_rural <- remove_legend(plot_hsi_rural)
plot_win_rural <- remove_legend(plot_win_rural)
plot_fmo_rural <- remove_legend(plot_fmo_rural)
plot_tmean_rural <- remove_legend(plot_tmean_rural)
plot_tsd_rural <- remove_legend(plot_tsd_rural)
plot_win_urban <- remove_legend(plot_win_urban)
plot_fmo_urban <- remove_legend(plot_fmo_urban)
plot_tmean_urban <- remove_legend(plot_tmean_urban)
plot_tsd_urban <- remove_legend(plot_tsd_urban)
plot_hsi_urban<- remove_legend(plot_hsi_urban)

# Function to highlight specific plots
highlight_plot <- function(plot) {
  plot + theme(plot.margin = margin(10, 10, 10, 10), 
               panel.border = element_rect(colour = "red", fill = NA, size = 2))
}

plot_tmean_rural <- highlight_plot(plot_tmean_rural)
plot_hsi_urban <- highlight_plot(plot_hsi_urban)

fig2_bio12 <- (plot_hsi_rural | plot_hsi_urban) / 
  (plot_win_rural | plot_win_urban) / 
  (plot_fmo_rural | plot_fmo_urban) / 
  (plot_tmean_rural | plot_tmean_urban) / 
  (plot_tsd_rural | plot_tsd_urban) + 
  plot_layout(guides = 'keep', widths = c(1, 1)) &
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1))



# Display the combined plot
print(fig2_bio12)


##### Aridity trend effects ####

#  Interaction precipitation trend * host plant specialization #

# Rural

mod_hsi_rural <-  glmmTMB(estimate ~ DMA_inverted*HSI  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_hsi_rural)

# Generate predictions
pred_values <- seq(0, 1, by = 0.05)
predictions <- ggpredict(mod_hsi_rural, terms = c("DMA_inverted", paste("HSI [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$HSI <- as.numeric(as.character(predictions$group))


# Create the plot
plot_hsi_rural <- ggplot(predictions, aes(x = x, y = predicted, group = HSI)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = HSI), alpha = 0.1) +
  geom_line(aes(color = HSI), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-1, 0.5, by = 0.5), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  labs(title = "(g) Aridity - Rural" , x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Garamond", size = 15),
    plot.subtitle = element_text(hjust = 0.5, family = "Garamond", size = 10),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

print(plot_hsi_rural)

# Urban

mod_hsi_urban <-  glmmTMB(estimate ~ scale(DMA_inverted)*scale(HSI)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_hsi_urban)


# Generate predictions
pred_values <- seq(0, 1, by = 0.05)
predictions <- ggpredict(mod_hsi_urban, terms = c("DMA_inverted", paste("HSI [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$HSI <- as.numeric(as.character(predictions$group))


# Create the plot
plot_hsi_urban <- ggplot(predictions, aes(x = x, y = predicted, group = HSI)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = HSI), alpha = 0.1) +
  geom_line(aes(color = HSI), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-1, 0.5, by = 0.5), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "HSI", breaks = seq(0, 1, by = 1)) +
  labs(title = "(h) Aridity - Urban", x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 14, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Garamond", size = 15),
    plot.subtitle = element_text(hjust = 0.5, family = "Garamond", size = 10),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 10, family = "Garamond", face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )


print(plot_hsi_urban)


#  Interaction aridity trend * wing index #

# Rural

mod_win_rural <-  glmmTMB(estimate ~ DMA_inverted*WIn  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_win_rural)

# Generate predictions
pred_values <- seq(-0.05, 0.15, by = 0.01)
predictions <- ggpredict(mod_win_rural, terms = c("DMA_inverted", paste("WIn [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$WIn <- as.numeric(as.character(predictions$group))

# Create the plot
plot_win_rural <- ggplot(predictions, aes(x = x, y = predicted, group = WIn)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = WIn), alpha = 0.1) +
  geom_line(aes(color = WIn), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-1, 0.5, by = 0.5), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_win_rural)


# Urban

mod_win_urban <-  glmmTMB(estimate ~ scale(DMA_inverted)*scale(WIn)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_win_urban)

# Generate predictions
pred_values <- seq(-0.05, 0.15, by = 0.01)
predictions <- ggpredict(mod_win_urban, terms = c("DMA_inverted", paste("WIn [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$WIn <- as.numeric(as.character(predictions$group))

# Create the plot
plot_win_urban <- ggplot(predictions, aes(x = x, y = predicted, group = WIn)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = WIn), alpha = 0.1) +
  geom_line(aes(color = WIn), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-1, 0.5, by = 0.5), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "WI", breaks = seq(-0.05, 0.15, by = 0.2)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 14, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_win_urban)

#  Interaction aridity trend * flight month average #

# Rural

mod_fmo_rural <-  glmmTMB(estimate ~ scale(DMA_inverted)*scale(FMo_Average)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_fmo_rural)

# Generate predictions
pred_values <- seq(2, 9.5, by = 0.375)
predictions <- ggpredict(mod_fmo_rural, terms = c("DMA_inverted", paste("FMo_Average [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$FMo_Average <- as.numeric(as.character(predictions$group))


# Create the plot
plot_fmo_rural <- ggplot(predictions, aes(x = x, y = predicted, group = FMo_Average)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = FMo_Average), alpha = 0.1) +
  geom_line(aes(color = FMo_Average), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-1, 0.5, by = 0.5), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_fmo_rural)

# Urban

mod_fmo_urban <-  glmmTMB(estimate ~ scale(DMA_inverted)*scale(FMo_Average)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_fmo_urban)

# Generate predictions
pred_values <- seq(2, 9.5, by = 0.375)
predictions <- ggpredict(mod_fmo_urban, terms = c("DMA_inverted", paste("FMo_Average [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$FMo_Average <- as.numeric(as.character(predictions$group))

# Create the plot
plot_fmo_urban <- ggplot(predictions, aes(x = x, y = predicted, group = FMo_Average)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = FMo_Average), alpha = 0.1) +
  geom_line(aes(color = FMo_Average), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-1, 0.5, by = 0.5), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "FMA", breaks = seq(2, 9.5, by = 7.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 14, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_fmo_urban)


#  Interaction aridity trend * STI #

# Rural

mod_tmean_rural <-  glmmTMB(estimate ~ DMA_inverted*temp.mean  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                            data = final_df_rural,
                            weights = inverse_variance_weights,
                            family = gaussian)

summary(mod_tmean_rural)

# Generate predictions
pred_values <- seq(3.5, 15, by = 0.25)
predictions <- ggpredict(mod_tmean_rural, terms = c("DMA_inverted", paste("temp.mean [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.mean <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tmean_rural <- ggplot(predictions, aes(x = x, y = predicted, group = temp.mean)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.mean), alpha = 0.1) +
  geom_line(aes(color = temp.mean), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-1, 0.5, by = 0.5), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.2, 0.4, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tmean_rural)

# Urban

mod_tmean_urban <-  glmmTMB(estimate ~ scale(DMA_inverted)*scale(temp.mean)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                            data = final_df_urban,
                            weights = inverse_variance_weights,
                            family = gaussian)

summary(mod_tmean_urban)

# Generate predictions
pred_values <- seq(3.5, 15, by = 0.25)
predictions <- ggpredict(mod_tmean_urban, terms = c("DMA_inverted", paste("temp.mean [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.mean <- as.numeric(as.character(predictions$group))

# Create the plot
plot_tmean_urban <- ggplot(predictions, aes(x = x, y = predicted, group = temp.mean)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.mean), alpha = 0.1) +
  geom_line(aes(color = temp.mean), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-1, 0.5, by = 0.5), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STI", breaks = seq(3.5, 15, by = 11.5)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 14, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tmean_urban)


#  Interaction aridity trend * STVI #

# Rural

mod_tsd_rural <-  glmmTMB(estimate ~ scale(DMA_inverted)*scale(temp.sd)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_rural,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_tsd_rural)

# Generate predictions
pred_values <- seq(1, 4, by = 0.2)
predictions <- ggpredict(mod_tsd_rural, terms = c("DMA_inverted", paste("temp.sd [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.sd <- as.numeric(as.character(predictions$group))


# Create the plot
plot_tsd_rural <- ggplot(predictions, aes(x = x, y = predicted, group = temp.sd)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.sd), alpha = 0.1) +
  geom_line(aes(color = temp.sd), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-1, 0.5, by = 0.5), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  labs(x = "Aridity trend", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 10, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 12),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tsd_rural)


# Urban

mod_tsd_urban <-  glmmTMB(estimate ~ scale(DMA_inverted)*scale(temp.sd)  + (1|SPECIES) + (1|SITE_ID) + (1|genzname), 
                          data = final_df_urban,
                          weights = inverse_variance_weights,
                          family = gaussian)

summary(mod_tsd_urban)

# Generate predictions
pred_values <- seq(1, 4, by = 0.2)
predictions <- ggpredict(mod_tsd_urban, terms = c("DMA_inverted", paste("temp.sd [", paste(pred_values, collapse = ","), "]", sep = "")), ci_level = 0.9)

predictions$temp.sd <- as.numeric(as.character(predictions$group))

# Create the plot
plot_tsd_urban <- ggplot(predictions, aes(x = x, y = predicted, group = temp.sd)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = temp.sd), alpha = 0.1) +
  geom_line(aes(color = temp.sd), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(-1, 0.5, by = 0.5), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "STVI", breaks = seq(1, 4, by = 3)) +
  labs(x = "Aridity trend", y = "") +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_text(size = 14, face = "bold", family = "Garamond", margin = margin(b = 15)),
    legend.text = element_text(size = 12, family = "Garamond"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", family = "Garamond", size = 12),
    axis.text = element_text(family = "Garamond", size = 12),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

print(plot_tsd_urban)


# Remove legends from rural plots
remove_legend <- function(plot) {
  plot + theme(legend.position = "none")
}

# Apply remove_legend function to rural plots
plot_hsi_rural <- remove_legend(plot_hsi_rural)
plot_win_rural <- remove_legend(plot_win_rural)
plot_fmo_rural <- remove_legend(plot_fmo_rural)
plot_tmean_rural <- remove_legend(plot_tmean_rural)
plot_tsd_rural <- remove_legend(plot_tsd_rural)


# Function to highlight specific plots
highlight_plot <- function(plot) {
  plot + theme(plot.margin = margin(10, 10, 10, 10), 
               panel.border = element_rect(colour = "red", fill = NA, size = 2))
}

plot_tmean_rural <- highlight_plot(plot_tmean_rural)
plot_hsi_urban <- highlight_plot(plot_hsi_urban)

fig2_DMA <- (plot_hsi_rural | plot_hsi_urban) / 
  (plot_win_rural | plot_win_urban) / 
  (plot_fmo_rural | plot_fmo_urban) / 
  (plot_tmean_rural | plot_tmean_urban) / 
  (plot_tsd_rural | plot_tsd_urban) + 
  plot_layout(guides = 'keep', widths = c(1, 1)) &
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1))

# Display the combined plot
print(fig2_DMA)



##### Plot figure 2 #####


super_combined_plot <- (fig2_urb | fig2_bio1 | fig2_bio12 | fig2_DMA)

ggsave("E:/URBAN TRENDS/Resultats/Figures/figure_2.png", plot = super_combined_plot, width = 22, height = 15, dpi = 600, bg = "white")


