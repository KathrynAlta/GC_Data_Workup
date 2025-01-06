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
  site_data <-  read_excel("01_Data/Field_Sheets/Site_Data/20250106_GHG_Under_Ice_Site_Data.xlsx")
  
  # also need to pull in barometric pressure 
  
  # Fix columne names in exetainer Ids 
  names(samples_clean)[names(samples_clean) == "Sample_ID"] <- "Exetainer_ID"
  names(exetainer_ids)[names(exetainer_ids) == "Lake Name"] <- "Lake_Name"
  names(exetainer_ids)[names(exetainer_ids) == "Water_Temp"] <- "Water_Temp_C"
  names(exetainer_ids)[names(exetainer_ids) == "Collection Method"] <- "Collection_Method"
  
  # Merge clean sample data and exetainer IDs 
  head(samples_clean)
  head(exetainer_ids)
  gc_data <- left_join(samples_clean, exetainer_ids)
  
  
## need to convert umol/L using ideal gas law and Henry's constant 
  
# 01. Convert to ppm to moles ----
  
  # Convert barometric pressure from inHg to atm 
  site_data$Baro_atm <- site_data$Baro_inHG * 0.0334
  
  # Add barometric pressure in atm and ambient temp in C to gc_data
  site_data$Lake_Date <- paste(site_data$Lake_ID, site_data$Date, sep="_") #make a lake date column in site data 
  site_data_sub <- subset(site_data, select = c("Lake_Date", "Ambient_Temp_C","Baro_atm")) # subset to only the columns that you need, use ambient temp for air samples 
  gc_data$Lake_Date <- paste(gc_data$Lake_ID, gc_data$Date_Collected, sep = "_") # Make a lake date columb in gc data 
  gc_data <- left_join(gc_data, site_data_sub)
  
  # Convert from ppm to atm 
  gc_data$CH4_atm <- (gc_data$CH4_ppm / 1E6) * gc_data$Baro_atm #Divide parts per million by one million and multiply by the atmospheric pressure at the site 
  gc_data$CO2_atm <- (gc_data$CO2_ppm / 1E6) * gc_data$Baro_atm
  gc_data$N2O_atm <- (gc_data$N2O_ppm / 1E6) * gc_data$Baro_atm
    # The first part of this is to go from ppm (which we assume is equal to microatmospheres) to atmospheres and the second part is to the elevation adjusted atmospheric pressure  
  
  # Convert ambient temp and water temp from C to K 
  gc_data$Ambient_Temp_K <- gc_data$Ambient_Temp_C + 273.15
  gc_data$Water_Temp_C <- as.numeric(gc_data$Water_Temp_C)
  gc_data$Water_Temp_K <- gc_data$Water_Temp_C + 273.15
  
  # Set volume of gas used in equilibration based on method and convert from mL to m3
      # Field headspace equilibration with syringes = 60 mL 
      # Air samples collected in the field = 20 mL 
      # Winter samples collected in glass bottles = ??
  gc_data$Equilibration_Volume_gas_mL <- ifelse(gc_data$Sample_Type == "AR", 20, 
                                             ifelse(gc_data$Collection_Method == "FHE", 60, NA))
  
  gc_data$Equilibration_Volume_gas_m3 <- gc_data$Equilibration_Volume_gas_mL * 1E-6 # 1mL = 1E-6 m3 
  
  # Set equilibration temperature to use (for air samples use ambient air temp, for all water samples use water temp)
  gc_data$Equilibration_Temp_K <- ifelse(gc_data$Sample_Type == "AR", gc_data$Ambient_Temp_K, gc_data$Water_Temp_K)
  
  #Solve for moles CH4, N2O, and CO2 using ideal gas law (PV= nRT) -> n = (PV)/(RT)         
      # P = partial pressure of each gas in atms (calculated using barometric pressure at the site on the day)
      # V = volume of gas in equilibration syringe (60mL for field headspace equilibrium)
      # R = ideal gas constant
      # T = temperature of the water or ambient air in K 
  R <- (8.20573 * (10^(-5)))  #Ideal Gas Constant ( m^3 * atm * mol^(-1))
  gc_data$CH4_mol_headspace <- (gc_data$CH4_atm * gc_data$Equilibration_Volume_gas_m3) / (R * gc_data$Equilibration_Temp_K)
  gc_data$CO2_mol_headspace <- (gc_data$CO2_atm * gc_data$Equilibration_Volume_gas_m3) / (R * gc_data$Equilibration_Temp_K)
  gc_data$N2O_mol_headspace <- (gc_data$N2O_atm * gc_data$Equilibration_Volume_gas_m3) / (R * gc_data$Equilibration_Temp_K)
  
# Then use Henry's law to go from the number of moles in the headspace to the concentration in the water 
  # Weiss (1974) and Wiesenburg and Guinasso (1979) check for constants (cited by Holgerson et al in TXh and MUD )
  
  






