## CONCENTRAION OF GAS IN WATER ----
## need to convert umol/L using ideal gas law and Henry's constant 

# 00. Set Up R Environment ----

  # Set working directory 
    # setwd("/Users/altagannon/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/05_Research_Projects/02_GHG_Under_Ice") # Laptop KAG
    setwd("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/05_Research_Projects/02_GHG_Under_Ice")    # Desktop SEEC
    
  # Load packages and functions 
    source("02_Analysis/GC_Data_Workup/00_libraries.R")
    source("02_Analysis/GC_Data_Workup/00_functions.R")

  # Load Data and solubility constants 
    samples_clean <- read_excel("01_Data/GC/04_Cleaned_Data/20250110_samples_clean.xlsx")
    exetainer_ids <- read_excel("01_Data/Field_Sheets/Exetainer_IDs/20250106_GHG_Under_Ice_Exetainers.xlsx")
    site_data <-  read_excel("01_Data/Field_Sheets/Site_Data/20250106_GHG_Under_Ice_Site_Data.xlsx")
    
    solubility_co2 <- read_excel("01_Data/GC/00_Constants/CO2_Solubility_Constants.xlsx")
    solubility_ch4 <- read_excel("01_Data/GC/00_Constants/CH4_Solubility_Constants.xlsx")
    solubility_n2o <- read_excel("01_Data/GC/00_Constants/N2O_Solubility_Constants.xlsx")
    
  # Format Data 
    # Fix columne names in exetainer Ids 
    names(samples_clean)[names(samples_clean) == "Sample_ID"] <- "Exetainer_ID"
    names(exetainer_ids)[names(exetainer_ids) == "Lake Name"] <- "Lake_Name"
    names(exetainer_ids)[names(exetainer_ids) == "Water_Temp"] <- "Water_Temp_C"
    names(exetainer_ids)[names(exetainer_ids) == "Collection Method"] <- "Collection_Method"
    
    # Merge clean sample data and exetainer IDs 
    head(samples_clean)
    head(exetainer_ids)
    gc_data <- left_join(samples_clean, exetainer_ids)
    
    
    ## I think this is backwards! use ppm in headspace and henry's law to get partial pressure in water in atm -> then use Ideal Gas law in water to go from partial pressure to mol/L
  
# 01. Ideal Gas Law: Convert from ppm to moles of each gas----
  
  # Convert barometric pressure from inHg to atm 
  site_data$Baro_atm <- site_data$Baro_inHG * 0.0334
  
  # Add barometric pressure (atm) and ambient temp (C) columns to gc_data
  site_data$Lake_Date <- paste(site_data$Lake_ID, site_data$Date, sep="_") #make a lake date column in site data 
  site_data_sub <- subset(site_data, select = c("Lake_Date", "Ambient_Temp_C","Baro_atm")) # subset to only the columns that you need, use ambient temp for air samples 
  gc_data$Lake_Date <- paste(gc_data$Lake_ID, gc_data$Date_Collected, sep = "_") # Make a lake date columb in gc data 
  gc_data <- left_join(gc_data, site_data_sub)
  
  # Convert partial pressure of each gas from ppm to atm 
  gc_data$CH4_atm <- (gc_data$CH4_ppm / 1E6) * gc_data$Baro_atm #Divide parts per million by one million and multiply by the atmospheric pressure at the site 
  gc_data$CO2_atm <- (gc_data$CO2_ppm / 1E6) * gc_data$Baro_atm
  gc_data$N2O_atm <- (gc_data$N2O_ppm / 1E6) * gc_data$Baro_atm
    # The first part of this is to go from ppm (which we assume is equal to microatmospheres) to atmospheres and the second part is to the elevation adjusted atmospheric pressure  
  
  # Convert ambient temp and water temp from C to K 
  gc_data$Ambient_Temp_K <- gc_data$Ambient_Temp_C + 273.15
  gc_data$Water_Temp_C <- as.numeric(gc_data$Water_Temp_C)
  gc_data$Water_Temp_K <- gc_data$Water_Temp_C + 273.15
  
  # Set volume of gas used in equilibration based on method used 
      # Field headspace equilibration with syringes = 60 mL of gas
      # Air samples collected in the field = 20 mL of gas 
      # Winter samples collected in glass bottles = ??
  gc_data$Equilibration_Volume_gas_mL <- ifelse(gc_data$Sample_Type == "AR", 20, 
                                             ifelse(gc_data$Collection_Method == "FHE", 60, NA))
  
  # Convert from mL of gas to m3 so that it matches the R constant 
  gc_data$Equilibration_Volume_gas_m3 <- gc_data$Equilibration_Volume_gas_mL * 1E-6 # 1mL = 1E-6 m3 
  
  # Set equilibration temperature to use (for air samples use ambient air temp, for all water samples use water temp)
  gc_data$Equilibration_Temp_K <- ifelse(gc_data$Sample_Type == "AR", gc_data$Ambient_Temp_K, gc_data$Water_Temp_K)
  
  #Solve for moles CH4, N2O, and CO2 using ideal gas law 
      # Algebra: (PV= nRT) -> n = (PV)/(RT)         
      # P = partial pressure of each gas in atms (calculated using barometric pressure at the site on the day)
      # V = volume of gas in equilibration syringe (60mL for field headspace equilibrium)
      # R = ideal gas constant
      # T = temperature of the water or ambient air in K 
  R <- (8.20573 * (10^(-5)))  #Ideal Gas Constant ( m^3 * atm * mol^(-1))
  gc_data$CH4_mol_headspace <- (gc_data$CH4_atm * gc_data$Equilibration_Volume_gas_m3) / (R * gc_data$Equilibration_Temp_K)
  gc_data$CO2_mol_headspace <- (gc_data$CO2_atm * gc_data$Equilibration_Volume_gas_m3) / (R * gc_data$Equilibration_Temp_K)
  gc_data$N2O_mol_headspace <- (gc_data$N2O_atm * gc_data$Equilibration_Volume_gas_m3) / (R * gc_data$Equilibration_Temp_K)
  
# For air samples you are done here! 
  
# 02. Henry's Law: From moles in headspace to conc in water ----
# For water samples you need to use Henry's law to go from the number of moles in the headspace to the concentration in the water 
  # Constants from from Weiss (1974), Wiesenburg and Guinasso (1979), Weiss and Price 1979 
  # Methods and equations come from Ray and Holgerson 2023 and Aho and Raymond 2019 
  
  # Add the K0 solubility constants for each gas to the gc data (remember that the solubility constant depends on the temperature!)
  
      # Start by doing some data formatting 
      gc_data$Water_Temp_C_rounded <- round(gc_data$Water_Temp_C) # Round the water temp to the nearest whole number so that it is compaitble with the solubility numbers 
      
      # Format column names for the solubility constant df for each gas 
      names(solubility_ch4)[names(solubility_ch4) == "Temp_C"] <- "Water_Temp_C_rounded"
      names(solubility_ch4)[names(solubility_ch4) == "CH4_K0_Salinity0"] <- "CH4_K0"
      solubility_ch4 <- subset(solubility_ch4, select = c("Water_Temp_C_rounded", "CH4_K0"))
      head(solubility_ch4)
      
      names(solubility_co2)[names(solubility_co2) == "Temp_C"] <- "Water_Temp_C_rounded"
      names(solubility_co2)[names(solubility_co2) == "CO2_K0_Salinity0"] <- "CO2_K0"
      solubility_co2 <- subset(solubility_co2, select = c("Water_Temp_C_rounded", "CO2_K0"))
      head(solubility_co2)
      
      names(solubility_n2o)[names(solubility_n2o) == "Temp_C"] <- "Water_Temp_C_rounded"
      names(solubility_n2o)[names(solubility_n2o) == "N2O_K0_Salinity0"] <- "N2O_K0"
      solubility_n2o <- subset(solubility_n2o, select = c("Water_Temp_C_rounded", "N2O_K0"))
      head(solubility_n2o)
    
      # join each of the solubility constant dfs to the gc data df by temperature 
      gc_data <- left_join(gc_data, solubility_ch4, relationship = "many-to-many")
      gc_data <- left_join(gc_data, solubility_co2, relationship = "many-to-many")
      gc_data <- left_join(gc_data, solubility_n2o, relationship = "many-to-many")
      
  # Calculate concentration of each gas in the water in mols using Henry's law 
      # Is this mols of gas in the water in the equilibration syringe? 
      #***** Not 100% sure if this is right! -KAG 20250110
      # [X] = pX * Kh
      # [X] = concentration of the gas (solute, ex: CH4) in the water (solvent)
      # pX = partial pressure of the gas (solute, ex: CH4) in the head space above the water 
      # Kh = solubility constant 
      gc_data$CH4_mol_water <- gc_data$CH4_mol_headspace * gc_data$CH4_K0
      gc_data$CO2_mol_water <- gc_data$CO2_mol_headspace * gc_data$CO2_K0
      gc_data$N2O_mol_water <- gc_data$N2O_mol_headspace * gc_data$N2O_K0
      
  # Then do you take that number of moles and divide it by the volume of water you used to get mols per liter? 
      #***** Not 100% sure if this is right! -KAG 20250110
        
      # Set the amount of water used as a column 
      #    AR: Not applicable for air samples 
      #   FHE: Field headspace equilibrium with syringes used 60 mL of gas and 80 mL of water 
      #   Add the volumes you use for the glass jar lab equilibrations 
      gc_data$Equilibration_Volume_water_mL <- ifelse(gc_data$Sample_Type == "AR", NA, 
                                                    ifelse(gc_data$Collection_Method == "FHE", 80, NA))
      
      gc_data$Equilibration_Volume_water_L <- gc_data$Equilibration_Volume_water_mL / 1000
      
      # Divide the mols of gas in the syringe
      gc_data$CH4_mol_per_L <- gc_data$CH4_mol_water / gc_data$Equilibration_Volume_water_L
      gc_data$CO2_mol_per_L <- gc_data$CO2_mol_water / gc_data$Equilibration_Volume_water_L
      gc_data$N2O_mol_per_L <- gc_data$N2O_mol_water / gc_data$Equilibration_Volume_water_L
      
      






