## Clean enviroment and set directory to files
rm(list=ls())
setwd("D:/URBAN TRENDS/BMS data")

#Libraries
library(dplyr)
library(purrr)
library(lmtest)
library(forecast)
library(dplyr)
library(nlme)
library(broom)
library(purrr)
library(lmtest)
library(tidyr)


sindex <- read.csv("SINDEX_CAT.csv", sep=",", dec=".")
head(sindex)

#Filtering temporal series with a minimum of years and positive values

sindex_filt <- sindex %>%
  group_by(SPECIES, SITE_ID, RCLIM) %>%
  # Asegúrate primero de que hay al menos 10 años distintos de datos
  filter(n_distinct(M_YEAR) >= 10) %>%
  # Luego, para cada grupo, calcula el porcentaje de años con SINDEX < 1
  mutate(porcentaje_bajo_1 = sum(SINDEX < 1, na.rm = TRUE) / n()) %>%
  # Filtra para mantener solo aquellos grupos donde menos de la mitad de los años tienen SINDEX < 1
  filter(porcentaje_bajo_1 <= 0.5) %>%
  # Elimina la columna auxiliar usada para el cálculo
  select(-porcentaje_bajo_1) %>%
  ungroup()

print(sindex_filt)

#Check temporal autocorrelation in each temporal series

resultados_autocorrelacion <- sindex_filt %>%
  group_by(SPECIES, RCLIM, SITE_ID) %>%
  do({
    data_serie = .
    modelo = lm(SINDEX ~ M_YEAR, data = data_serie)
    residuos = resid(modelo)
    
    # Determina el número de lags basándose en la longitud de la serie
    # Por ejemplo, usa el mínimo entre 10 y un tercio de la longitud de la serie
    n_lags = min(10, max(1, floor(length(residuos) / 3)))
    
    test = Box.test(residuos, type = "Ljung-Box", lag = n_lags)
    data.frame(p_value = test$p.value, n_lags = n_lags)
  }) %>%
  ungroup() %>%
  mutate(autocorrelation_present = p_value < 0.05) # Añade una columna indicando presencia de autocorrelación

# Verifica los resultados
print(resultados_autocorrelacion)

# Resumen de la presencia de autocorrelación
resumen_autocorrelacion <- resultados_autocorrelacion %>%
  summarise(
    Con_Autocorrelacion = sum(autocorrelation_present, na.rm = TRUE),
    Sin_Autocorrelacion = sum(!autocorrelation_present, na.rm = TRUE),
    NA_Autocorrelacion = sum(is.na(autocorrelation_present))
  )

# Muestra el resumen
print(resumen_autocorrelacion)

grupos_con_autocorrelacion <- resumen_autocorrelacion %>%
  filter(Con_Autocorrelacion)


# Linear models accounting for temporal autocorrelation if it is needed

# Paso 1 revisado: Ajustar modelos lineales básicos y extraer coeficientes y p-valores
modelos_basicos_coefs <- sindex_filt %>%
  group_by(SPECIES, SITE_ID, RCLIM) %>%
  do({
    modelo = lm(SINDEX ~ M_YEAR, data = .)
    residuos = resid(modelo)
    n_lags = min(10, max(1, floor(length(residuos) / 3)))
    autocorr_test = Box.test(resid(modelo), type = "Ljung", lag = n_lags)
    modelo_summary = summary(modelo)
    slope_coefs = coef(modelo_summary)["M_YEAR", ]
    data.frame(Slope = slope_coefs["Estimate"], p_value_slope = slope_coefs["Pr(>|t|)"], p_value_autocorr = autocorr_test$p.value, Modelo = "LM")
  }) %>%
  ungroup()

# Ajustar modelos GLS con término de autocorrelación y extraer coeficientes y p-valores
modelos_con_autocorrelacion_coefs <- sindex_filt %>%
  semi_join(grupos_con_autocorrelacion, by = c("SPECIES", "SITE_ID", "RCLIM")) %>%
  group_by(SPECIES, SITE_ID, RCLIM) %>%
  do({
    gls_model = gls(SINDEX ~ M_YEAR, data = ., correlation = corAR1(form = ~ M_YEAR))
    gls_summary = summary(gls_model)
    # Extrae los coeficientes y p-valores directamente
    slope_estimate = gls_summary$tTable["M_YEAR", "Value"]
    slope_p_value = gls_summary$tTable["M_YEAR", "p-value"]
    data.frame(Slope = slope_estimate, p_value_slope = slope_p_value, Modelo = "GLS")
  }) %>%
  ungroup()

# Combinar los coeficientes en una única tabla
coeficientes_combinados <- bind_rows(modelos_basicos_coefs, modelos_con_autocorrelacion_coefs)

# Muestra los coeficientes combinados
print(coeficientes_combinados)


