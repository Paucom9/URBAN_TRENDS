#  --- Variables --- # 

# Response variable
  #"estimate": Slope of the population (species*site) abundance (sindex) temporal trends (temporal series >= 8 years with positive values of at least half of the yars) considering temporal autocorrelation.
    # Structure of the linear model to calculate estimates (see "Butterfly population trends.R"): centered_log_sindex* ~ centered_year*, correlation = corAR1(form = ~ centered_year)
      #*We centre the year and abundances of each population time series at zero (subtracting each year by the mean year in each population, and subtracting the log of each abundance by the mean log abundance value in each population). 

# Main predictor
  #"urb_trend": Slop of the temporal trend of urbanization (built-up fraction) at site level associated to a specific temporal series (species*site combination).
    # urb_trends are calculated at 4 different scales: 100*100m; 500*500m; 1000*1000m; and 2000*2000m. All models will be replicated at these four scales to test variability in model results.
    # To calculate urb_trends we followed the following process (see "urbanization trends.R" lines 75-168): 
      #1. Modeling urbanization from 1975 to 2025, with available data at intervals of 5 years, using four different model types: linear; polynomial, exponential and logarithmic.
      #2. Select the model with the lowest AIC and predict the urbanization for the years corresponding to the species*site temporal series: 99.3% exponential; 0.7% linear.
      #3. Calculate the linear slope for the subset of years. *If the best model in step 2 was the linear model, then slope of 1975-2025 will be equal to the slope for the subset years.

# Random factors
  #1. "SPECIES": Unique species (n = 145).
  #2. "SITE_ID: Unique sites (i.e. transect locations) (n = 1137).
  #3. "Country.Name": Indicates the country where the SITE_ID is located. SITE_ID is nested in Country_Name. Alternatively, SITE_ID could be nested in "climate region," which might also be nested in "latitudinal region." However, this nested structure may cause convergence issues in the model due to increased model complexity.
    #Country.Name levels: Belgium (0.02%); Czech Republic (1.12%); Finland (16.6%); France (3.74%); Germany (13.8%); Ireland (0.5%); Luxembourg (0.3%); Netherlands (1%); Norway (0.4%); Slovenia (0.73%); Spain (40.3%); Sweden (3.5%); UK (18%). 

# Covariables (could also be included as random factors)
  #1."urban_name": Site type based on urbanization (considering both population number and built-up fraction) at the start of the temporal series (see "urbanization trends.R" lines 230-294)
    # Only in 14.6% species*site cases a site change the urban_name category over the temporal series.
    # Urban name levels: Urban center (3.35%); Dense urban (1.27%); Semi-dense urban (0.61%); suburban/peri-urban (7.9%); rural (1.6%); low density rural (14.8%); very low density rural (70.5%)
    # * Data biased towards rural sites.
    # Maybe create a new factor with only two levels (urban vs. rural)
  #2. "genzname": Climate region classification following Metzger et al.2013.
    #Rclim levels: Cold and wet (0.12%); Cold and mesic (28.34%); Cool temperate and dry (10.1%); Cool temperate and xeric (0.52%); Cool temperate and moist (22.3%); Warm temperate and mesix (34.7%); Warm temperate and xeric (3.9%).
    # *Maybe delete sites of underrepresented climate regions (<1%)?

# Species traits interacting with urbanization trends
  #1. "HSI": Quantitative measure of hostplant specialization (Middleton-Weeling et al. 2020) ---> TROPHIC SPECIALIZATION
    #Other possible variables measuring trophic specialization: Number of adult food types (1-8: herbs, ergot, shrub/tree flower, honeydew, sap, decaying plant, animal, mineral)
  #2. "WIn": Wing index. Composite variable representing a single measurement of overall size generated from forewing length and wingspan measures (Middleton-Weeling et al. 2020). ---> MOBILITY
  #3 "temp.mean". The mean temperature within the species range (Schweiger et al. 2014) ---> CLIMATE NICHE POSITION
    #Consider using temp.max/temp.min depending on the hypotheses ---> Upper/Lower climatic limit
  #4. "temp.sd". Standard deviation of temperature across the species range (Schweiger et al. 2014) ---> CLIMATE NICHE BREADTH
    # Alternatively "range.ann.temp": annual range in monthly temperature (warmest month - coldest month) or "FMo_Average": Average number of months of the year a species is observed flying can be also used as proxy of thermal tolerance.

#Weights: Logarithm of the inverse of the variance (log(1/std.error)). Trends with higher precision are weighted more heavily than those with greater uncertainty.


# --- Hypotheses and models --- #

# Libraries required
library(glmmTMB)
library(DHARMa)

# Variables preparation
final_df$inverse_variance_weights <- log(1/(final_df$std.error))
final_df$estimate <- as.numeric(final_df$estimate)
final_df$urb_trend <- as.numeric(final_df$urb_trend)
final_df$HSI <- as.numeric(final_df$HSI)
final_df$WIn <- as.numeric(final_df$WIn)
final_df$temp.mean <- as.numeric(final_df$temp.mean)
final_df$temp.sd <- as.numeric(final_df$temp.sd)
final_df$SPECIES <- as.factor(final_df$SPECIES)
final_df$Country.Name <- as.factor(final_df$Country.Name)
final_df$urban_names <- as.factor(final_df$urban_names)
final_df$genzname <- as.factor(final_df$genzname)


# - H1: There is a consistent association across species, sites, and regions between increased urbanization trends and more significant negative impacts on butterfly populations. - #

mod_h1 <- glmmTMB(estimate ~ urb_trend  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                    data = final_df,
                    weights = inverse_variance_weights,
                    family = gaussian)

summary(mod_h1)
sim_res <- simulateResiduals(fittedModel = mod_h1)
testUniformity(sim_res)
testDispersion(sim_res)
plot(sim_res)


# - H2: The effect of urbanisation interact with the type of site. The effect of urbanisation is more negative in rural populations than in urban populations. - #
  # If this interaction is significant consider conducting models with triple interaction urb_trend*urban_name*trait

mod_h2 <- glmmTMB(estimate ~ urb_trend*urban_names  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod_h2)


# - H3: The impact of urbanization on butterfly populations varies depending on the type of site, with rural populations experiencing more negative effects from urbanization than urban populations. - #
# Urban environments in warmer regions are less tolerable to butterflies compared to those in colder regions.

mod_h3 <- glmmTMB(estimate ~ urb_trend*genzname  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod_h3) #Model convergence problem

# - H4: Given that host plant specialization has been negatively associated with urban affinity (Callaghan et al. 2021), it is expected that more specialized butterfly species will experience more negative population trends due to urbanization compared to generalist species. - #

mod_h4 <- glmmTMB(estimate ~ urb_trend*HSI  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod_h4)


# - H5: Given that wing index (used as proxy of mobility/dispersal capacity) has been positively associated with urban affinity (Callaghan et al. 2021), it is expected that species with low disperal capacity will experience more negative population trends due to urbanization compared to more mobile species. - #

mod_h5 <- glmmTMB(estimate ~ urb_trend*WIn  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod_h5)


# - H6: Given that the mean temperature in rangge has been positively associated with urban affinity (Callaghan et al. 2021), species adapted to low temperatures are expected to experience more negative population trends due to urbanization compared to species that prefer warmer, thermophilic conditions. - #

mod_h6 <- glmmTMB(estimate ~ urb_trend*temp.mean  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod_h6)


# - H7: Species that exhibit a wider climate niche breadth will be less affected by urbanization compared to species with a narrower climate niche breadth. - #

mod_h7 <- glmmTMB(estimate ~ urb_trend*temp.sd  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)


summary(mod_h7)


