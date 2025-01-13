###  Calculations of surface water top and bottom CO2 and CH4 concentrations ###

library("tidyverse")
library("readxl")
library("dplyr")
library("writexl")
library("ggplot2")
library("dplyr")
library("emmeans")
library("ggforce")
library("ggpubr")

setwd("C:/Users/pb577/OneDrive - Cornell University/Documents/DEC_Ponds/Kaci")

DEC_Ponds_2024_calculated_water_GHG_3Oct2024 <- read_excel("DEC_Ponds_2024_calculated_water-GHG_3Oct2024.xlsx")

data <- DEC_Ponds_2024_calculated_water_GHG_3Oct2024

#Put negative N20 data to zero
data <- data %>% mutate(`Calculated N2O final (ppm)` = if_else(`Calculated N2O final (ppm)` < 0, 0, `Calculated N2O final (ppm)`))

#1) Convert temperature in Kelvin
data$Water_temp_C <- as.numeric(data$Water_temp_C)
data$Water_temp_K <- data$Water_temp_C + 273.15

#2) Atmospheric pressure #CHECK WITH MH/KG!

#inHg to atm
data$pressure_kesterel_atm <- data$`Pressure (Kestrel reading)`* 0.03342 #https://www.convertunits.com/from/in+Hg/to/atm

#3) Convert water and headspace volume
data$Vol_water_L <- 100/1000
data$Vol_air_L <- 40/1000

#4) Concentration calculations accounting for air that we added as headspace
#4.1) Solubility coefficients in mol L-1 atm-1
#CO2: Weiss 1974
#CH4: Wiesenburg and Guinasso 1979
#N2O: Weiss and Price 1980
data$CO2_kh <- exp(-58.0931+90.5069*(100/data$Water_temp_K) + 22.294*log(data$Water_temp_K/100))
data$CH4_kh <- exp(-68.8862+101.4956*(100/data$Water_temp_K) + 28.7314*log(data$Water_temp_K/100))/(0.08206*data$Water_temp_K/data$pressure_kesterel_atm)
data$N2O_kh <- exp(-62.7062+97.3066*(100/data$Water_temp_K) + 24.1406*log(data$Water_temp_K/100))

#4.2) uatm calculations for how much gases in sampled headspace (after equilibration)
data$CO2_sample_uatm <- data$`Calculated CO2 final (ppm)`*data$pressure_kesterel_atm
data$CH4_sample_uatm <- data$`Calculated CH4 final (ppm)`*data$pressure_kesterel_atm
data$N2O_sample_uatm <- data$`Calculated N2O final (ppm)`*data$pressure_kesterel_atm

#4.5) Concentration calculations (for headspace created with atmospheric air)
data$CO2_umol_L <- ((data$CO2_sample_uatm* data$Vol_water_L* data$CO2_kh)+((data$CO2_sample_uatm* data$Vol_air_L)/(0.0821*data$Water_temp_K)))/data$Vol_water_L
data$pCO2_uatm  <- data$CO2_umol_L / data$CO2_kh

data$CH4_umol_L <- ((data$CH4_sample_uatm* data$Vol_water_L* data$CH4_kh) +((data$CH4_sample_uatm* data$Vol_air_L)/(0.0821*data$Water_temp_K)))/data$Vol_water_L
data$pCH4_uatm  <- data$CH4_umol_L / data$CH4_kh

data$N2O_umol_L <- ((data$N2O_sample_uatm* data$Vol_water_L* data$N2O_kh) +((data$N2O_sample_uatm* data$Vol_air_L)/(0.0821*data$Water_temp_K)))/data$Vol_water_L
data$pN2O_uatm  <- data$N2O_umol_L / data$N2O_kh

data[,c("pCO2_uatm", "pCH4_uatm", "pN2O_uatm")] %>% summary()

#Export data
write_xlsx(data,"C:/Users/pb577/OneDrive - Cornell University/Documents/DEC_Ponds/Kaci/DEC_Ponds_2024_data.xlsx")
