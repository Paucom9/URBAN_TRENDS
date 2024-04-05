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
  #1. "HSI": Quantitative measure of hostplant specialization (Middleton-Weeling et al. 2020). ---> TROPHIC SPECIALIZATION
  # Hostplant index ranges from 0 for species which are highly polyphagous to 1 for species that are completely monophagous.  
    #Other possible variables measuring trophic specialization: Number of adult food types (1-8: herbs, ergot, shrub/tree flower, honeydew, sap, decaying plant, animal, mineral)
  #2. "WIn": Wing index. Composite variable representing a single measurement of overall size generated from forewing length and wingspan measures (Middleton-Weeling et al. 2020). ---> MOBILITY
  #3 "temp.mean". The mean temperature within the species range (Schweiger et al. 2014) ---> CLIMATE NICHE POSITION
    #Consider using temp.max/temp.min depending on the hypotheses ---> Upper/Lower climatic limit
  #4. "temp.sd". Standard deviation of temperature across the species range (Schweiger et al. 2014) ---> CLIMATE NICHE BREADTH
    # Alternatively "range.ann.temp": annual range in monthly temperature (warmest month - coldest month) or 
  #5."FMo_Average": Average number of months of the year a species is observed flying as a proxy of voltinism.*In Callaghan et al. 2021 FMo_Average is considered a proxy of thermal tolerance.

#Weights: Logarithm of the inverse of the variance (log(1/std.error)). Trends with higher precision are weighted more heavily than those with greater uncertainty.


# --- Hypotheses and models --- #

# Libraries required
library(glmmTMB)
library(DHARMa)

# Variables preparation
final_df$inverse_variance_weights <- log(1/(final_df$std.error)) #Calculate log inverse of the variance (the higher value the higher precision of the estimate)
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
final_df <- final_df %>%  # Create a binomial variable for urban_names
  mutate(urban_type = case_when(
    urban_names %in% c("URBAN CENTRE", "SUBURBAN OR PERI-URBAN", "DENSE URBAN CLUSTER", "SEMI-DENSE URBAN CLUSTER") ~ "urban",
    urban_names %in% c("LOW DENSITY RURAL", "VERY LOW DENSITY RURAL", "RURAL CLUSTER GRID") ~ "rural",
    TRUE ~ NA_character_ # Handles <NA> values or any other unexpected cases
  ))


# Variables transformation
  # Check normality of the response variable
  hist(final_df$estimate)
  qqnorm(final_df$estimate); qqline(final_df$estimate) # No transformation (logarithmic, square root, inverse, Box-Cox) succeeded in enhancing normality.
  # Check normality of the main predictor
  hist(final_df$urb_trend) # Left-skewed
  qqnorm(final_df$urb_trend); qqline(final_df$urb_trend) 
  final_df$urb_trend_sqrt <- sqrt(final_df$urb_trend - min(final_df$urb_trend)) # Square root transformation
  hist(final_df$urb_trend_sqrt)
  qqnorm(final_df$urb_trend_sqrt); qqline(final_df$urb_trend_sqrt) # Normality increased with sqrt transformation
  
  
# ***** urb_trends in final_df are calculated at the 2000x2000 scale. Repeat models at other different scales (100x100, 500x500, 1000x1000)
  
# - H1: There is a consistent association across species, sites, and regions between increased urbanization trends and more significant negative impacts on butterfly populations. - #

mod_h1 <- glmmTMB(estimate ~ urb_trend_sqrt  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                    data = final_df,
                    weights = inverse_variance_weights,
                    family = gaussian)


summary(mod_h1) # Marginal negative effect of urbanization
sim_res <- simulateResiduals(fittedModel = mod_h1)
testUniformity(sim_res) # No normality of the residuals
testDispersion(sim_res) # OK
plot(sim_res)


# - H2: The effect of urbanisation interact with the type of site. The effect of urbanisation is more negative in rural populations than in urban populations. - #
  # If this interaction is significant consider conducting models with triple interaction urb_trend*urban_name*trait

mod_h2a <- glmmTMB(estimate ~ urb_trend_sqrt*urban_names  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod_h2a) 
sim_res <- simulateResiduals(fittedModel = mod_h2a)
testUniformity(sim_res)
testDispersion(sim_res)
plot(sim_res)


mod_h2b <- glmmTMB(estimate ~ urb_trend_sqrt*urban_type  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                   data = final_df,
                   weights = inverse_variance_weights,
                   family = gaussian)

summary(mod_h2b) # Marginal negative effect of urbanization
sim_res <- simulateResiduals(fittedModel = mod_h2b)
testUniformity(sim_res)
testDispersion(sim_res)
plot(sim_res)



# - H3: The impact of urbanization on butterfly populations varies depending on the type of site, with rural populations experiencing more negative effects from urbanization than urban populations. - #
# Urban environments in warmer regions are less tolerable to butterflies compared to those in colder regions.

mod_h3 <- glmmTMB(estimate ~ urb_trend_sqrt*genzname  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod_h3) # Marginal negative effect of urbanization; significant differences of population trends among climate regions
sim_res <- simulateResiduals(fittedModel = mod_h3)
testUniformity(sim_res)
testDispersion(sim_res)
plot(sim_res)

# - H4: Given that host plant specialization has been negatively associated with urban affinity (Callaghan et al. 2021), it is expected that more specialized butterfly species will experience more negative population trends due to urbanization compared to generalist species. - #

# *Revise possible transformations of HSI
hist(final_df$HSI) #Left-skewed

mod_h4 <- glmmTMB(estimate ~ urb_trend_sqrt*HSI  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod_h4) # Significant effect of HSI (Counterintuitively monophagous species decline more); 
                # Significant interaction between urbanization and HSI: the negative impact of urbanization on butterfly trends is amplified at higher levels of HSI.
sim_res <- simulateResiduals(fittedModel = mod_h4)
testUniformity(sim_res)
testDispersion(sim_res)
plot(sim_res)


# - H5: Given that wing index (used as proxy of mobility/dispersal capacity) has been positively associated with urban affinity (Callaghan et al. 2021), it is expected that species with low disperal capacity will experience more negative population trends due to urbanization compared to more mobile species. - #

# *Revise possible transformations of WIn
hist(final_df$WIn)

mod_h5 <- glmmTMB(estimate ~ urb_trend_sqrt*WIn  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod_h5) # Marginal effect of urbanization; Significant effect of wing size (the larger the species, the more positive trend); 
                # Significant interaction between urbanization and body size: as wing size increases, the negative impact of urbanization on butterfly trends is magnified.
sim_res <- simulateResiduals(fittedModel = mod_h5)
testUniformity(sim_res)
testDispersion(sim_res)
plot(sim_res)


# - H6: Given that the mean temperature in range has been positively associated with urban affinity (Callaghan et al. 2021), species adapted to low temperatures are expected to experience more negative population trends due to urbanization compared to species that prefer warmer, thermophilic conditions. - #

hist(final_df$temp.mean) # OK

mod_h6 <- glmmTMB(estimate ~ urb_trend*temp.mean  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)

summary(mod_h6) # Significant effect of mean temperature (thermophil species have more positive trends)
sim_res <- simulateResiduals(fittedModel = mod_h6)
testUniformity(sim_res)
testDispersion(sim_res)
plot(sim_res)


# - H7: Species that exhibit a wider climate niche breadth will be less affected by urbanization compared to species with a narrower climate niche breadth. - #

# *Revise possible transformations of temp.sd
hist(final_df$temp.sd) # Right-skewed

mod_h7 <- glmmTMB(estimate ~ urb_trend*temp.sd  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)


summary(mod_h7) # Significant effect of urbanization; Significant effect of climate niche breadth (Counterintuitively species with greater niche breadth decline more)
                # Significant interaction among urbanization and climate niche breadth: as climate niche breadth increases, the negative impact of urbanization on population trend is lessened.
sim_res <- simulateResiduals(fittedModel = mod_h7)
testUniformity(sim_res)
testDispersion(sim_res)
plot(sim_res)

# - H8: In the last decades multivoltine butterflies have had more positive long-term population trends than univoltine species (Colom et al. 2022; Macgregor et al. 2019; Michielini et al. 2021; Wepprich et al. 2019).
# Long-term population trends are positively correlated with increasing voltinism over time (Wepprich et al. 2024 in revision), suggesting that the addition of generations in multivoltine species can be a mechanism of adaption
# to climate change/urbanization in contrast to inflexible univoltine species - # 

# *Revise possible transformations of FMo_Average
hist(final_df$FMo_Average)

mod_h8 <- glmmTMB(estimate ~ urb_trend*$ FMo_Average  + (1|SPECIES) + (1| Country.Name / SITE_ID), 
                  data = final_df,
                  weights = inverse_variance_weights,
                  family = gaussian)


summary(mod_h8) # Significant effect of urbanization; Significant effect of mean flight months (the higher the voltinism the higher positive trend)
                # Significant interaction among urbanization and mean flight months: for species with more generations per year (higher FMo_Average), the negative impact of urbanization is moderated or less severe than for species with a unique or few generations per year.
sim_res <- simulateResiduals(fittedModel = mod_h8)
testUniformity(sim_res)
testDispersion(sim_res)
plot(sim_res)

