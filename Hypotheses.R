library(readr)
final_df <- read_csv("E:/URBAN TRENDS/Urban_trends_R_Project/final_df.csv")

#  --- Variables --- # 

# Response variable
  #"estimate": Slope of the population (species*site) abundance (sindex) temporal trends (temporal series >= 10 years with positive values of at least half of the years) considering temporal autocorrelation.
    # Structure of the linear model to calculate estimates (see "Butterfly population trends.R"): log(sindex + 1) ~ year, correlation = corAR1(form = ~ year)

# Main predictor
  #"urb_trend": Slope of the temporal trend of urbanization (built-up fraction) at site level associated to a specific temporal series (species*site combination).
    # urb_trends are calculated at 4 different scales: 100*100m; 500*500m; 1000*1000m; and 2000*2000m. All models will be replicated at these four scales to test variability in model results.
    # To calculate urb_trends we followed the following process (see "urbanization trends.R" lines 75-168): 
      #1. Modeling urbanization from 1975 to 2025, with available data at intervals of 5 years, using four different model types: linear; polynomial, exponential and logarithmic.
      #2. Select the model with the lowest AIC and predict the urbanization for the years corresponding to the species*site temporal series: 99.3% exponential; 0.7% linear.
      #3. Calculate the linear slope for the subset of years. *If the best model in step 2 was the linear model, then slope of 1975-2025 will be equal to the slope for the subset years.

# Random factors
  #1. "SPECIES": Unique species (n = 145).
  #2. "SITE_ID: Unique sites (i.e. transect locations) (n = 1137).
  #3. "genzname": Climate region classification following Metzger et al.2013.
    #Rclim levels: Cold and wet (0.12%); Cold and mesic (28.34%); Cool temperate and dry (10.1%); Cool temperate and xeric (0.52%); Cool temperate and moist (22.3%); Warm temperate and mesix (34.7%); Warm temperate and xeric (3.9%).

# Covariables (could also be included as random factors)
  #1."urban_name": Site type based on urbanization (considering both population number and built-up fraction) at the start of the temporal series (see "urbanization trends.R" lines 230-294)
    # Only in 14.6% species*site cases a site change the urban_name category over the temporal series.
    # Urban name levels: Urban center (3.35%); Dense urban (1.27%); Semi-dense urban (0.61%); suburban/peri-urban (7.9%); rural (1.6%); low density rural (14.8%); very low density rural (70.5%)
    # * Data biased towards rural sites.
    # Maybe create a new factor with only two levels (urban vs. rural)

# Species traits interacting with urbanization trends
  #1. "HSI": Quantitative measure of hostplant specialization (Middleton-Weeling et al. 2020). ---> TROPHIC SPECIALIZATION
  # Hostplant index ranges from 0 for species which are highly polyphagous to 1 for species that are completely monophagous.  
    #Other possible variables measuring trophic specialization: Number of adult food types (1-8: herbs, ergot, shrub/tree flower, honeydew, sap, decaying plant, animal, mineral)
  #2. "WIn": Wing index. Composite variable representing a single measurement of overall size generated from forewing length and wingspan measures (Middleton-Weeling et al. 2020). ---> MOBILITY
  #3 "temp.mean". The mean temperature within the species range (Schweiger et al. 2014) ---> CLIMATE NICHE POSITION
    #Consider using temp.max/temp.min depending on the hypotheses ---> Upper/Lower climatic limit
  #4. "temp.sd". Standard deviation of temperature across the species range (Schweiger et al. 2014) ---> CLIMATE NICHE BREADTH
    # Alternatively "range.ann.temp": annual range in monthly temperature (warmest month - coldest month) or 
  #5."FMo_Average": Average number of months of the year a species is observed flying as a proxy of voltinism.*In Callaghan et al. 2021 FMo_Average is considered a proxy of thermal tolerance.

# Climate trends
    # Slope of the temporal trend of each climate variable at site level associated to a specific temporal series (species*site combination).
      # "bio1": Mean annual temperature
      # "bio4": Temperature seasonality
      # "bio12": Mean annual precipitation
      # "bio15": Precipitation seasonality
      # "GDD5": Degree-days above 5°C, growing degree-days
      # "DMA": 	De Martonne aridity index. (Mean annual precip.)/(Mean annual temp. + Mean temperature of the warmest quarter) + 12*Precipitation of the driest month/(Mean temperature of the driest month)) / 2

#Weights: Logarithm of the inverse of the variance (log(1/std.error)). Trends with higher precision are weighted more heavily than those with greater uncertainty.


# --- Hypotheses and models --- #

# Libraries required
library(glmmTMB)
library(phyr)
library(DHARMa)
library(effects)
library(dplyr)

<<<<<<< HEAD
=======
final_df <- read.csv("final_df.csv")
>>>>>>> e3c9a326bc7637da00c9994c433da31e52a8aa15

# Variables preparation
final_df$inverse_variance_weights <- 1/(final_df$std_error^2) # Inverse of the variance (the higher value the higher precision of the estimate)
final_df$estimate <- as.numeric(final_df$estimate)
final_df$HSI <- as.numeric(final_df$HSI)
final_df$WIn <- as.numeric(final_df$WIn)
final_df$temp.mean <- as.numeric(final_df$temp.mean)
final_df$temp.sd <- as.numeric(final_df$temp.sd)
final_df$bio1 <- as.numeric(final_df$bio1)
final_df$bio4 <- as.numeric(final_df$bio4)
final_df$bio12 <- as.numeric(final_df$bio12)
final_df$bio15 <- as.numeric(final_df$bio15)
final_df$GDD5 <- as.numeric(final_df$GDD5)
final_df$DMA <- as.numeric(final_df$DMA)
final_df$SPECIES <- as.factor(final_df$SPECIES)
final_df$SITE_ID <- as.factor(final_df$SITE_ID)
final_df$Country.Name <- as.factor(final_df$Country.Name)
final_df$urban_names <- as.factor(final_df$urban_names)
final_df$genzname <- as.factor(final_df$genzname)
final_df <- final_df %>%  # Create a binomial variable for urban_names
  mutate(urban_type = case_when(
    urban_names %in% c("URBAN CENTRE", "SUBURBAN OR PERI-URBAN", "DENSE URBAN CLUSTER", "SEMI-DENSE URBAN CLUSTER") ~ "Urban",
    urban_names %in% c("LOW DENSITY RURAL", "VERY LOW DENSITY RURAL", "RURAL CLUSTER GRID") ~ "Rural",
    TRUE ~ NA_character_ # Handles <NA> values or any other unexpected cases
  ))
final_df$POPULATION_ID <- paste(final_df$SPECIES, final_df$SITE_ID, sep="_") # Create an ID factor for unique combinations of species*site
final_df$Vol_mean <- (final_df$Vol_max + final_df$Vol_min)/2
final_df$ADS <- as.numeric(final_df$ADS)
# Create the new variable rclim with the specified categories
final_df <- final_df%>%
  mutate(rclim = case_when(
    genzname %in% c("E. Cold and wet", "G. Cold and mesic") ~ "Cold",
    genzname %in% c("H. Cool temperate and dry", "J. Cool temperate and moist") ~ "Cool temperate",
    genzname %in% c("K. Warm temperate and mesic", "L. Warm temperate and xeric") ~ "Warm temperate",
    TRUE ~ NA_character_  # Assign NA for any other values, if they exist
  ))


  
# ***** urb_trends in final_df are calculated at the 2000x2000 scale. Repeat models at other different scales (100x100, 500x500, 1000x1000)
  
# - H1: There is a consistent association across species, sites, and regions between increased urbanization trends and more significant negative impacts on butterfly populations. - #

mod_h1 <- glmmTMB(estimate ~ urb_trend + (1|SPECIES) + (1| SITE_ID) + (1|genzname), 
                    data = final_df,
                    weights = inverse_variance_weights,
                    family = gaussian)

mod_h1_phylo_spat <- pglmm(estimate ~ urb_trend + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                      data = final_df,
                      family = "gaussian",
                      cov_ranef = list(SPECIES = pruned_tree ,SITE_ID = norm_dist_matrix))

summary(mod_h1) # Marginal negative effect of urbanization
summary(mod_h1_phylo_spat)


# - H2: The effect of urbanisation interact with the type of site. The effect of urbanisation is more negative in rural populations than in urban populations. - #
  # If this interaction is significant consider conducting models with triple interaction urb_trend*urban_name*trait

# urban_names = all urban type categories; Urban type = urban vs. rural 
mod_h2 <- glmmTMB(estimate ~ urb_trend*urban_type  + (1|SPECIES) + (1|SITE_ID) + (1 | genzname), 
                   data = final_df,
                   weights = inverse_variance_weights,
                   family = gaussian)

mod_h2_phylo_spat <- pglmm(estimate ~ urb_trend*urban_type + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                            data = final_df,
                            family = "gaussian",
                            cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h2) # Marginal negative effect of urbanization;
                # Marginal significant interaction among urbanization and site type: the impact of urbanization on species abundance trends is more positive in urban areas than in rural areas.
summary(mod_h2_phylo_spat)

effects <- Effect(c("urb_trend", "urban_type"), mod_h3)
plot(effects)


# - H3: Given that host plant specialization has been negatively associated with urban affinity (Callaghan et al. 2021), it is expected that more specialized butterfly species will experience more negative population trends due to urbanization compared to generalist species. - #


mod_h3 <- glmmTMB(estimate ~ urb_trend*HSI  + (1|SPECIES) + (1|SITE_ID) + (1 | genzname), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

mod_h3_phylo_spat <- pglmm(estimate ~ urb_trend*HSI + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                            data = final_df,
                            family = "gaussian",
                            cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h3) # Significant effect of HSI (Counterintuitively monophagous species decline more); 
                # Significant interaction between urbanization and HSI: the negative impact of urbanization on butterfly trends is amplified at higher levels of HSI.
summary(mod_h3_phylo_spat)


effects <- Effect(c("urb_trend", "HSI"), mod_h3)
plot(effects)


# - H4: Given that wing index (used as proxy of mobility/dispersal capacity) has been positively associated with urban affinity (Callaghan et al. 2021), it is expected that species with low disperal capacity will experience more negative population trends due to urbanization compared to more mobile species. - #


mod_h4 <- glmmTMB(estimate ~ urb_trend*WIn  + (1|SPECIES) + (1| SITE_ID) + (1|genzname), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

mod_h4_phylo_spat <- pglmm(estimate ~ urb_trend*WIn + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                           data = final_df,
                           family = "gaussian",
                           cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h4) # Marginal effect of urbanization; Significant effect of wing size (the larger the species, the more positive trend); 
                # Significant interaction between urbanization and body size: as wing size increases, the negative impact of urbanization on butterfly trends is magnified.
summary(mod_h4_phylo_spat)


effects <- Effect(c("urb_trend", "WIn"), mod_h4)
plot(effects)


# - H5: Given that the mean temperature in range has been positively associated with urban affinity (Callaghan et al. 2021), species adapted to low temperatures are expected to experience more negative population trends due to urbanization compared to species that prefer warmer, thermophilic conditions. - #

mod_h5 <- glmmTMB(estimate ~ urb_trend*temp.mean  + (1|SPECIES) + (1|SITE_ID) + (1 | genzname), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

mod_h5_phylo_spat <- pglmm(estimate ~ urb_trend*temp.mean + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                           data = final_df,
                           family = "gaussian",
                           cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h5) # Significant effect of mean temperature (thermophil species have more positive trends)
summary(mod_h5_phylo_spat)

effects <- Effect(c("urb_trend", "temp.mean"), mod_h5)
plot(effects)


# - H6: Species that exhibit a wider climate niche breadth will be less affected by urbanization compared to species with a narrower climate niche breadth. - #

mod_h6 <- glmmTMB(estimate ~ urb_trend*temp.sd  + (1|SPECIES) + (1|SITE_ID) + (1 | genzname), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

mod_h6_phylo_spat <- pglmm(estimate ~ urb_trend*temp.sd + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                           data = final_df,
                           family = "gaussian",
                           cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))


summary(mod_h6) # Significant effect of urbanization; Significant effect of climate niche breadth (Counterintuitively species with greater niche breadth decline more)
                # Significant interaction among urbanization and climate niche breadth: as climate niche breadth increases, the negative impact of urbanization on population trend is lessened.
summary(mod_h6_phylo_spat)


effects <- Effect(c("urb_trend", "temp.sd"), mod_h6)
plot(effects)

# - H7: In the last decades multivoltine butterflies have had more positive long-term population trends than univoltine species (Colom et al. 2022; Macgregor et al. 2019; Michielini et al. 2021; Wepprich et al. 2019).
# Long-term population trends are positively correlated with increasing voltinism over time (Wepprich et al. 2024 in revision), suggesting that the addition of generations in multivoltine species can be a mechanism of adaption
# to climate change/urbanization in contrast to inflexible univoltine species - # 

mod_h7 <- glmmTMB(estimate ~ urb_trend*FMo_Average  + (1|SPECIES) + (1|SITE_ID) + (1 | genzname), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

mod_h7_phylo_spat <- pglmm(estimate ~ urb_trend*FMo_Average + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                           data = final_df,
                           family = "gaussian",
                           cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))


summary(mod_h7) # Significant effect of urbanization; Significant effect of mean flight months (the higher the voltinism the higher positive trend)
                # Significant interaction among urbanization and mean flight months: for species with more generations per year (higher FMo_Average), the negative impact of urbanization is moderated or less severe than for species with a unique or few generations per year.

summary(mod_h7_phylo_spat)


effects <- Effect(c("urb_trend", "FMo_Average"), mod_h7)
plot(effects)


mod_h7b <-  glmmTMB(estimate ~ urb_trend*Vol_min  + (1|SPECIES) + (1|SITE_ID)  + (1 | genzname), 
                    data = final_df,
                    weights = inverse_variance_weights,
                    family = gaussian)

mod_h7b_phylo_spat <- pglmm(estimate ~ urb_trend*Vol_min + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                           data = final_df,
                           family = "gaussian",
                           cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h7b)
summary(mod_h7b_phylo_spat)

effects_min <- Effect(c("urb_trend", "Vol_min"), mod_h7b)
plot(effects_min)

mod_h7c <-  glmmTMB(estimate ~ urb_trend*Vol_max  + (1|SPECIES) + (1|SITE_ID)  + (1 | genzname), 
                    data = final_df,
                    weights = inverse_variance_weights,
                    family = gaussian)

mod_h7c_phylo_spat <- pglmm(estimate ~ urb_trend*Vol_max + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                           data = final_df,
                           family = "gaussian",
                           cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h7c)
summary(mod_h7c_phylo_spat)

effects_min <- Effect(c("urb_trend", "Vol_min"), mod_h7c)
plot(effects_min)

mod_h7d <-  glmmTMB(estimate ~ urb_trend*Vol_mean  + (1|SPECIES) + (1|SITE_ID)  + (1 | genzname), 
                    data = final_df,
                    weights = inverse_variance_weights,
                    family = gaussian)

mod_h7d_phylo_spat <- pglmm(estimate ~ urb_trend*Vol_mean + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                            data = final_df,
                            family = "gaussian",
                            cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h7d)
summary(mod_h7d_phylo_spat)


# -H8: Interaction urbanization*climate (mean bio1 40 years)

mod_h8 <-  glmmTMB(estimate ~ urb_trend*mean_bio1  + (1|SPECIES) + (1|SITE_ID)  + (1 | genzname), 
                    data = final_df,
                    weights = inverse_variance_weights,
                    family = gaussian)

mod_h8_phylo_spat <- pglmm(estimate ~ urb_trend*mean_bio1 + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                           data = final_df,
                           family = "gaussian",
                           cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h8)
summary(mod_h8_phylo_spat)




# -H9: Interaction urbanization*climate trends

#a) mean annual trend
mod_h9a <-  glmmTMB(estimate ~ urb_trend*bio1  + (1|SPECIES) + (1|SITE_ID)  + (1 | genzname), 
                   data = final_df,
                   weights = inverse_variance_weights,
                   family = gaussian)

mod_h9a_phylo_spat <- pglmm(estimate ~ urb_trend*bio1 + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                           data = final_df,
                           family = "gaussian",
                           cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h9a)
summary(mod_h9a_phylo_spat)


effects <- Effect(c("urb_trend", "bio1"), mod_h9)
plot(effects)

# "bio4": Temperature seasonality


mod_h9b <-  glmmTMB(estimate ~ urb_trend*bio4  + (1|SPECIES) + (1|SITE_ID)  + (1 | genzname), 
                   data = final_df,
                   weights = inverse_variance_weights,
                   family = gaussian)

mod_h9b_phylo_spat <- pglmm(estimate ~ urb_trend*bio4 + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                            data = final_df,
                            family = "gaussian",
                            cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))


summary(mod_h9b)
summary(mod_h9b_phylo_spat)



# "bio12": Mean annual precipitation


mod_h9c <-  glmmTMB(estimate ~ urb_trend*bio12  + (1|SPECIES) + (1|SITE_ID)  + (1 | genzname), 
                    data = final_df,
                    weights = inverse_variance_weights,
                    family = gaussian)

mod_h9c_phylo_spat <- pglmm(estimate ~ urb_trend*bio12 + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                            data = final_df,
                            family = "gaussian",
                            cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h9c)
summary(mod_h9c_phylo_spat)


effects <- Effect(c("urb_trend", "bio12"), mod_h9c)
plot(effects)

# "bio15": Precipitation seasonality

mod_h9d <-  glmmTMB(estimate ~ urb_trend*bio15  + (1|SPECIES) + (1|SITE_ID)  + (1 | genzname), 
                    data = final_df,
                    weights = inverse_variance_weights,
                    family = gaussian)

mod_h9d_phylo_spat <- pglmm(estimate ~ urb_trend*bio15 + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                            data = final_df,
                            family = "gaussian",
                            cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h9d)
summary(mod_h9d_phylo_spat)


effects <- Effect(c("urb_trend", "bio15"), mod_h9d)
plot(effects)


# "GDD5": Degree-days above 5°C, growing degree-days

mod_h9e <-  glmmTMB(estimate ~ urb_trend*GDD5  + (1|SPECIES) + (1|SITE_ID)  + (1 | genzname), 
                    data = final_df,
                    weights = inverse_variance_weights,
                    family = gaussian)

mod_h9e_phylo_spat <- pglmm(estimate ~ urb_trend*GDD5 + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                            data = final_df,
                            family = "gaussian",
                            cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h9e)
summary(mod_h9e_phylo_spat)

effects <- Effect(c("urb_trend", "GDD5"), mod_h9e)
plot(effects)

# "DMA": 	De Martonne aridity index.

mod_h9f <-  glmmTMB(estimate ~ urb_trend*DMA  + (1|SPECIES) + (1|SITE_ID)  + (1 | genzname), 
                    data = final_df,
                    weights = inverse_variance_weights,
                    family = gaussian)

mod_h9f_phylo_spat <- pglmm(estimate ~ urb_trend*DMA + (1 | SPECIES__) +  (1 | SITE_ID__)  + (1 | genzname), 
                            data = final_df,
                            family = "gaussian",
                            cov_ranef = list(SPECIES = pruned_tree, SITE_ID = norm_dist_matrix))

summary(mod_h9f)
summary(mod_h9f_phylo_spat)

effects <- Effect(c("urb_trend", "DMA"), mod_h9f)
plot(effects)


