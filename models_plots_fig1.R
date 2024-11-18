###### interactions of urbanization and climate trends (separately) with site type (rural or urban) --- Figure 1 --- Contrasting effects of urbanization and climate on rural and urban populations ######

# Remove all objects from the current R session to ensure a clean working environment
rm(list = ls())  

# Required libraries
library(extrafont)
library(ggeffects)
library(ggplot2)
library(patchwork)
library(glmmTMB)


setwd("E:/URBAN TRENDS")  

final_df <- read.csv("final_df.csv", sep = ",", dec = ".")

head(final_df)
str(final_df)


# --- Data preparation --- #


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


# Identify common species in rural and urban sites
rural_species <- unique(final_df$SPECIES[final_df$urban_type == "Rural"])
urban_species <- unique(final_df$SPECIES[final_df$urban_type == "Urban"])
common_species <- intersect(rural_species, urban_species)

# Filter by common species in rural and urban sites
filtered_df <- final_df %>% 
  filter(SPECIES %in% common_species)

table(filtered_df$SPECIES, filtered_df$urban_type)

filtered_df<- na.omit(filtered_df)


# --- Models and plots --- #


# Urbanization trend

mod1 <- glmmTMB(estimate ~ urb_trend*urban_type + (1|SPECIES) + (1|SITE_ID) + (1 | genzname), 
                  data = filtered_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod1)

predictions <- ggpredict(mod1, terms = c("urb_trend", "urban_type"), ci_level = 0.9)


# Crear el gráfico personalizado
urb_trend_plot <- ggplot(predictions, aes(x = x, y = predicted, group = group)) +
  # Add raw data points with their own color mapping
  geom_point(data = final_df, aes(x = urb_trend, y = estimate, color = urban_type), size = 1, alpha = 0.2, inherit.aes = FALSE) +
  # Add lines with specific colors for the groups, overriding the color mapping for points
  geom_line(aes(color = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Slightly thicker dashed line at y = 0
  # Scale for points only
  scale_color_manual(values = c("Rural" = "#3CB371", "Urban" = "#FF6347"), name = "Site type") +
  # Separate fill scale
  scale_fill_manual(values = c("Rural" = "#3CB371", "Urban" = "#FF6347"), name = "Site type") +
  scale_x_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  labs(
    x = "Urbanization trend",
    y = "Population trend") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "Garamond"),
        plot.subtitle = element_text(hjust = 0.5, family = "Garamond"),
        axis.title = element_text(face = "bold", family = "Garamond", size = 10),
        axis.text = element_text(family = "Garamond", size = 10),  # Change axis numbers font
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        legend.position = "none",  # Adjust the height of legend elements
        panel.background = element_rect(fill = "white", color = NA))  # Set white background

plot(urb_trend_plot)


# Temperature trend

mod2 <- glmmTMB(estimate ~ bio1*urban_type + (1|SPECIES) + (1|SITE_ID) + (1 | genzname), 
                  data = filtered_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod2)

predictions <- ggpredict(mod2, terms = c("bio1", "urban_type"), ci_level = 0.9)

bio1_plot <- ggplot(predictions, aes(x = x, y = predicted, group = group)) +
  # Add raw data points with their own color mapping
  geom_point(data = final_df, aes(x = bio1, y = estimate, color = urban_type), size = 1, alpha = 0.2, inherit.aes = FALSE) +
  # Add lines with specific colors for the groups, overriding the color mapping for points
  geom_line(aes(color = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Slightly thicker dashed horizontal line at y = 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Slightly thicker dashed vertical line at x = 0
  # Scale for points only
  scale_color_manual(values = c("Rural" = "#3CB371", "Urban" = "#FF6347"), name = "Site type") +
  # Separate fill scale
  scale_fill_manual(values = c("Rural" = "#3CB371", "Urban" = "#FF6347"), name = "Site type") +
  scale_x_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  labs(
    x = "Temperature trend",
    y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "Garamond"),
        plot.subtitle = element_text(hjust = 0.5, family = "Garamond"),
        axis.title = element_text(face = "bold", family = "Garamond", size = 10),
        axis.text = element_text(family = "Garamond", size = 10),  # Change axis numbers font
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        legend.position = "none",  # Adjust the height of legend elements
        panel.background = element_rect(fill = "white", color = NA))  # Set white background

plot(bio1_plot)


# Precipitation trend

mod3 <- glmmTMB(estimate ~ bio12*urban_type + (1|SPECIES) + (1|SITE_ID) + (1 | genzname), 
                  data = filtered_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod3)

predictions <- ggpredict(mod3, terms = c("bio12", "urban_type"), ci_level = 0.9)


bio12_plot <- ggplot(predictions, aes(x = x, y = predicted, group = group)) +
  # Add raw data points with their own color mapping
  geom_point(data = final_df, aes(x = bio12, y = estimate, color = urban_type), size = 1, alpha = 0.2, inherit.aes = FALSE) +
  # Add lines with specific colors for the groups, overriding the color mapping for points
  geom_line(aes(color = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Slightly thicker dashed horizontal line at y = 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Slightly thicker dashed vertical line at x = 0
  # Scale for points only
  scale_color_manual(values = c("Rural" = "#3CB371", "Urban" = "#FF6347"), name = "Site type") +
  # Separate fill scale
  scale_fill_manual(values = c("Rural" = "#3CB371", "Urban" = "#FF6347"), name = "Site type") +
  scale_x_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  labs(
    x = "Precipitation trend",
    y = "Population trend") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Garamond"),
    plot.subtitle = element_text(hjust = 0.5, family = "Garamond"),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 10),  # Change axis numbers font
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    legend.position = "none",  # Adjust the height of legend elements
    panel.background = element_rect(fill = "white", color = NA))  # Set white background

plot(bio12_plot)


# Aridity trend


mod4 <- glmmTMB(estimate ~ DMA_inverted*urban_type + (1|SPECIES) + (1|SITE_ID) + (1 | genzname), 
                  data = filtered_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod4)

predictions <- ggpredict(mod4, terms = c("DMA_inverted", "urban_type"), ci_level = 0.9)


DMA_plot <- ggplot(predictions, aes(x = x, y = predicted, group = group)) +
  # Add raw data points with their own color mapping
  geom_point(data = final_df, aes(x = DMA_inverted, y = estimate, color = urban_type), size = 1, alpha = 0.2, inherit.aes = FALSE) +
  # Add lines with specific colors for the groups, overriding the color mapping for points
  geom_line(aes(color = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Slightly thicker dashed horizontal line at y = 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Slightly thicker dashed vertical line at x = 0
  # Scale for points only
  scale_color_manual(values = c("Rural" = "#3CB371", "Urban" = "#FF6347"), name = "Site type") +
  # Separate fill scale
  scale_fill_manual(values = c("Rural" = "#3CB371", "Urban" = "#FF6347"), name = "Site type") +
  scale_x_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  labs(
    x = "Aridity trend",
    y = "") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Garamond"),
    plot.subtitle = element_text(hjust = 0.5, family = "Garamond"),
    axis.title = element_text(face = "bold", family = "Garamond", size = 10),
    axis.text = element_text(family = "Garamond", size = 10),  # Change axis numbers font
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    legend.position = "none",  # Adjust the height of legend elements
    panel.background = element_rect(fill = "white", color = NA))  # Set white background

plot(DMA_plot)



# --- Combined plot --- #


theme_reduced <- theme(
  plot.margin = unit(c(0, 0, 0, 0), "cm"),
  panel.spacing = unit(0.1, "lines"),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  panel.border = element_blank()  # Remove panel borders
)

# Apply reduced theme to all plots
urb_trend_plot <- urb_trend_plot + theme_reduced
bio1_plot <- bio1_plot + theme_reduced
bio12_plot <- bio12_plot + theme_reduced
DMA_plot <- DMA_plot + theme_reduced

# Combined plot
combined_plot <- (urb_trend_plot + bio1_plot) / (bio12_plot + DMA_plot) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") & 
  theme(plot.tag = element_text(family = "Garamond", face = "bold", size = 10),
        plot.tag.position = c(0.028, 1))

# Mostrar el gráfico combinado
combined_plot



ggsave("E:/URBAN TRENDS/Resultats/Figures/Figure1.png", plot = combined_plot, width = 6, height = 4.5, dpi = 600, bg = "white")


