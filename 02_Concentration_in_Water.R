## CONCENTRAION OF GAS IN WATER ----

# 00. Set Up R Environment ----

  # Set working directory 
  # setwd("/Users/altagannon/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/05_Research_Projects/02_GHG_Under_Ice") # Laptop KAG
  setwd("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/05_Research_Projects/02_GHG_Under_Ice")    # Desktop SEEC
  
  # Load packages and functions 
  source("02_Analysis/GC_Data_Workup/00_libraries.R")
  source("02_Analysis/GC_Data_Workup/00_functions.R")

  # Load Data 
  samples_clean <- read_excel("01_Data/GC/04_Cleaned_Data/20241216_samples_clean.xlsx")
  exetainer_ids <- read_excel("01_Data/Field_Sheets/Exetainer_IDs/20250106_GHG_Under_Ice_Exetainers.xlsx")
  
  # Fix columne names in exetainer Ids 
  names(samples_clean)[names(samples_clean) == "Sample_ID"] <- "Exetainer_ID"
  names(exetainer_ids)[names(exetainer_ids) == "Lake Name"] <- "Lake_Name"
  names(exetainer_ids)[names(exetainer_ids) == "Collection Method"] <- "Collection_Method"

  # Merge clean sample data and exetainer IDs 
  head(samples_clean)
  head(exetainer_ids)
  gc_data <- left_join(samples_clean, exetainer_ids)
  
# 01. Convert to umol/L using ideal gas law and Henry's constant 
  
  # pulled from Wetlands pipeline 
      # 6. Convert CH4_ppm_per_sec and CO2_ppm_per_sec (slope values from portable or GC) from ppm_sec to atm_sec so that they match the R 
      flux_data$CH4_atm_per_sec <- (flux_data$CH4_ppm_per_sec/ 1E6 ) * flux_data$Atmospheric_Pressure   #Divide parts per million by one million and multiply by the atmospheric pressure at the site 
      flux_data$CO2_atm_per_sec <- (flux_data$CO2_ppm_per_sec / 1E6 ) * flux_data$Atmospheric_Pressure   #Divide parts per million by one million and multiply by the atmospheric pressure at the site 
      # The first part of this is to go from ppm (which we assume is equal to microatmospheres ) to atmospheres 
      # and the second part is to the elevation adjusted atmospheric pressure  
      
      # 7. Solve for moles CH4, N2O, and CO2 using ideal gas law (PV= nRT)          
      R <- (8.20573 * (10^(-5)))  #Ideal Gas Constant ( m^3 * atm * mol^(-1))
      flux_data$CH4_mol_per_sec <- ( flux_data$CH4_atm_per_sec * flux_data$Vol_m3) / (R * flux_data$Temp_K)
      flux_data$CO2_mol_per_sec <- ( flux_data$CO2_atm_per_sec * flux_data$Vol_m3) / (R * flux_data$Temp_K)






