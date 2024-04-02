#  --- Variables --- # 

# Response variable
  #"estimate": Slope of the population (species*site) abundance (sindex) temporal trends (temporal series >= 8 years with positive values of at least half of the yars) considering temporal autocorrelation.
    # Structure of the linear model to calculate estimates (see "Butterfly population trends.R"): centered_log_sindex* ~ centered_year*, correlation = corAR1(form = ~ centered_year)
      #*We centre the year and abundances of each population time series at zero (subtracting each year by the mean year in each population, and subtracting the log of each abundance by the mean log abundance value in each population). 
      #This centring fixes the y and x intercepts at zero for each slope, and is a convenient solution to account for variance captured by the random intercepts without increasing the number of parameters.
  
# Main predictor
  #"urb_trend": Slop of the temporal trend of urbanisation (built-up fraction) at site level associated to a specific temporal series (species*site combination).
    # urb_trends are calculated at 4 different scales: 100*100m; 500*500m; 1000*1000m; and 2000*2000m. All models will be replicated at these four scales to test variability in model results.
    # To calculate urb_trends we followed the following process (see "urbanisation trends.R" lines 75-168): 
      #1. Modelize urbanisation from 1975 to 2025, with available data at intervals of 5 years, using four different model types: linear; polynomial, exponential and logarithmic.
      #2. Select the model with the lowest AIC and predict the urbanisation for the years corresponding to the species*site temporal series.
      #3. Calculate the linear slope for the subset of years. *If the best model in step 2 was the linear model, then slope of 1975-2025 will be equal to the slope for the subset years.

# Random factors
  #1. "SPECIES": Unique species.
  #2. "SITE_ID: Unique sites (i.e. transect locations). *SITE_ID could be nested in "climate region" which at the same time this could be nested in "latitudinal region" thogh this nested structure may lead to convergence issues of the model.

# Covariables (could also be included as random factors)
  #1."urban_name": Site type based on urbanisation (considering both population number and built-up fraction) at the start of the temporal series (see "urbanisation trends.R" lines 230-294)
    # Only in 14.6% species*site cases a site change the urban_name cateogory over the temporal series.
    # Urban name levels: Urban centre (3.35%); Dense urban (1.27%); Semi-dense urban (0.61%); suburban/peri-urban (7.9%); rural (1.6%); low density rural (14.8%); very low density rural (70.5%)
    # * Data biased towards rural sites.
    # Maybe create a new factor with only two levels (urban vs. rural)
  #2. "genzname": Climate region classification following Metzger et al.2013.
    #Rclim levels: Cold and wet (0.12%); Cold and mesic (28.34%); Cool temperate and dry (10.1%); Cool temperate and xeric (0.52%); Cool temperate and moist (22.3%); Warm temperate and mesix (34.7%); Warm temperate and xeric (3.9%).
    # *Maybe delete sites of underrepresented climate regions (<1%)?

# Species traits interacting with urbanisation trends
  #1. "HSI": Quantitative measure of hostplant specialization (Middleton-Weeling et al. 2020) ---> TROPHIC SPECIALIZATION
    #Other possible variables measuring trophic specialization: Number of adult food types (1-8: herbs, ergot, shrub/tree flower, honeydew, sap, decaying plant, animal, mineral)
  #2. "WIn": Wing index. Composite variable representing a single measurment of overall size generated from forewing length and wingspan measures (Middleton-Weeling et al. 2020). ---> MOBILITY
  #3 "temp.mean". The mean temperature within the species range (Schweiger et al. 2014) ---> CLIMATE NICHE POSITION
    #Consider using temp.max/temp.min depending on the hypotheses ---> Upper/Lower climatic limit
  #4. "temp.sd". Standard deviation of temperature across the species range (Schweiger et al. 2014) ---> CLIMATE NICHE BREADTH
    # Alternatively "range.ann.temp": annual range in monthly temperature (warmest month - coldest month) or "FMo_Average": Avergage number of months of the year a species is observed flying can be also used as proxy of thermal tholerance.

#Weights: Logarithm of the inverse of the variance (log(1/std.error)). Trends with higher precision are weighted more heavily than those with greater uncertainty.

# --- Hypotheses and models --- #

# H1: There is a consistent association across species, sites, and regions between increased urbanization trends and more significant negative impacts on butterfly populations.

library(glmmTMB)

mod_h1 <- glmmTMB(estimate ~ urb_trend  + (1|SPECIES) + (1|SITE_ID), 
                    data = final_df,
                    weights = inverse_variance_weights,
                    family = gaussian)

# H2: The effect of urbanisation interact with the type of site. The effect of urbanisation is more negative in rural populations than in urban populations.
  # If this interaction is significant consider conducting models with triple interaction urb_trend*urban_name*trait

mod_h2 <- glmmTMB(estimate ~ urb_trend*urban_names  + (1|SPECIES) + (1|SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

# H3: The impact of urbanization on butterfly populations varies depending on the type of site, with rural populations experiencing more negative effects from urbanization than urban populations. 
# Urban environments in warmer regions are less tolerable to butterflies compared to those in colder regions.

mod_h3 <- glmmTMB(estimate ~ urb_trend*genzname  + (1|SPECIES) + (1|SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

# H4: Given that host plant specialization has been negatively associated with urban affinity (Callaghan et al. 2021), it is expected that more specialized butterfly species will experience more negative population trends due to urbanization compared to generalist species.

mod_h4 <- glmmTMB(estimate ~ urb_trend*HSI  + (1|SPECIES) + (1|SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

# H5: Given that wing index (used as proxy of mobility/dispersal capacity) has been positively associated with urban affinity (Callaghan et al. 2021), it is expected that species with low disperal capacity will experience more negative population trends due to urbanization compared to more mobile species.

mod_h5 <- glmmTMB(estimate ~ urb_trend*WIn  + (1|SPECIES) + (1|SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

# H6: Given that the mean temperature in rangge has been positively associated with urban affinity (Callaghan et al. 2021), species adapted to low temperatures are expected to experience more negative population trends due to urbanization compared to species that prefer warmer, thermophilic conditions.

mod_h6 <- glmmTMB(estimate ~ urb_trend* temp.mean  + (1|SPECIES) + (1|SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

# H7: Species that exhibit a wider climate niche breadth will be less affected by urbanization compared to species with a narrower climate niche breadth.

mod_h7 <- glmmTMB(estimate ~ urb_trend*temp.sd  + (1|SPECIES) + (1|SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)




