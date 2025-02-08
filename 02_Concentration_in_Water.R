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
    samples_clean <- read_excel("01_Data/GC/04_Calibrated_GC_Data/20250110_samples_clean.xlsx")
    exetainer_ids <- read_excel("01_Data/Field_Sheets/Exetainer_IDs/20250106_GHG_Under_Ice_Exetainers.xlsx")
    site_data <-  read_excel("01_Data/Field_Sheets/Site_Data/20250106_GHG_Under_Ice_Site_Data.xlsx")

    
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
    
    # Convert barometric pressure from inHg to atm 
    site_data$Baro_atm <- site_data$Baro_inHG * 0.0334
    
    # Add barometric pressure (atm) and ambient temp (C) columns to gc_data
    site_data$Lake_Date <- paste(site_data$Lake_ID, site_data$Date, sep="_") #make a lake date column in site data 
    site_data_sub <- subset(site_data, select = c("Lake_Date", "Ambient_Temp_C","Baro_atm")) # subset to only the columns that you need, use ambient temp for air samples 
    gc_data$Lake_Date <- paste(gc_data$Lake_ID, gc_data$Date_Collected, sep = "_") # Make a lake date columb in gc data 
    gc_data <- left_join(gc_data, site_data_sub)
    
    # Make a new column with the amount of water used for equilibration 
    #    AR: Not applicable for air samples 
    #   FHE: Field headspace equilibrium with syringes used 60 mL of gas and 80 mL of water 
    #   Under Ice Lab equilibration = 135 mL of water 
    gc_data$Equilibration_Volume_water_mL <- ifelse(gc_data$Sample_Type == "AR", NA, 
                                                    ifelse(gc_data$Collection_Method == "FHE", 80, NA))
    # Convert from mL of gas to L so that it matches the R constant 
    gc_data$Equilibration_Volume_water_L <- gc_data$Equilibration_Volume_water_mL / 1000
    
    # Make a new column with the amount of gas  used for equilibration 
        # Field headspace equilibration with syringes = 60 mL of gas
        # Air samples collected in the field = 20 mL of gas 
        # Winter samples collected in glass bottles = 100 mL of zero air 
    gc_data$Equilibration_Volume_gas_mL <- ifelse(gc_data$Sample_Type == "AR", 20, 
                                                  ifelse(gc_data$Collection_Method == "FHE", 60, NA))
    
    # Convert from mL of gas to L so that it matches the R constant 
    gc_data$Equilibration_Volume_gas_L <- gc_data$Equilibration_Volume_gas_mL / 1000
    
    # Convert partial pressure of each gas from ppm to micro atm 
    gc_data$CH4_uatm <- gc_data$CH4_ppm * gc_data$Baro_atm #multiply by the atmospheric pressure at the site 
    gc_data$CO2_uatm <- gc_data$CO2_ppm * gc_data$Baro_atm
    gc_data$N2O_uatm <- gc_data$N2O_ppm * gc_data$Baro_atm
    
    # Convert ambient temp and water temp from C to K 
    gc_data$Ambient_Temp_K <- gc_data$Ambient_Temp_C + 273.15
    gc_data$Water_Temp_C <- as.numeric(gc_data$Water_Temp_C)
    gc_data$Water_Temp_K <- gc_data$Water_Temp_C + 273.15
    
    # Set equilibration temperature to use (for air samples use ambient air temp, for all water samples use water temp)
    gc_data$Equilibration_Temp_K <- ifelse(gc_data$Sample_Type == "AR", gc_data$Ambient_Temp_K, gc_data$Water_Temp_K)
    
    
# 01. Solve for micro moles of Each Gas in the equilibrated Headspace using Ideal Gas Law----
  
  #Solve for moles CH4, N2O, and CO2 using ideal gas law 
      # Algebra: (PV= nRT) -> n = (PV)/(RT)         
      # P = partial pressure of each gas in atms (calculated using barometric pressure at the site on the day)
      # V = volume of gas in equilibration syringe (60mL for field headspace equilibrium)
      # R = ideal gas constant
      # T = temperature of the water or ambient air in K 
  R <- 0.0821  #Ideal Gas Constant ( L * atm * mol^(-1) * L^(-1) )
    
  gc_data$CH4_umol_equilibrated_headspace <- (gc_data$CH4_uatm * gc_data$Equilibration_Volume_gas_L) / (R * gc_data$Equilibration_Temp_K)
  gc_data$CO2_umol_equilibrated_headspace <- (gc_data$CO2_uatm * gc_data$Equilibration_Volume_gas_L) / (R * gc_data$Equilibration_Temp_K)
  gc_data$N2O_umol_equilibrated_headspace <- (gc_data$N2O_uatm * gc_data$Equilibration_Volume_gas_L) / (R * gc_data$Equilibration_Temp_K)
  
  # For air samples you are done here! 
  
# 02. Solve for micro moles of gas in the equilibrated water after  using Henry's Law ----
  # Constants from from 
      #CO2: Weiss 1974
      #CH4: Wiesenburg and Guinasso 1979
      #N2O: Weiss and Price 1980
  # Methods explaination https://www.hydropower.org/publications/ghg-measurement-guidelines-for-freshwater-reservoirs 
  # Methods and equations come from Ray and Holgerson 2023 and Holgerson and Raymond 2019 
  
  #2.1) Calculate Solubility coefficients in mol L-1 atm-1 for each gas 
    
  gc_data$CO2_kh <- exp( (-58.0931) + 90.5069*(100/gc_data$Equilibration_Temp_K) + 22.294*log(gc_data$Equilibration_Temp_K/100))
  gc_data$CH4_kh <- exp(-68.8862+101.4956*(100/gc_data$Equilibration_Temp_K) + 28.7314*log(gc_data$Equilibration_Temp_K/100))/(0.08206*gc_data$Equilibration_Temp_K/gc_data$Baro_atm)
  gc_data$N2O_kh <- exp(-62.7062+97.3066*(100/gc_data$Equilibration_Temp_K) + 24.1406*log(gc_data$Equilibration_Temp_K/100))
  
  # 2.2) Use that solubility coefficient to calculate the number of moles of each gas is in the water 
  gc_data$CO2_umol_equilibrated_water <- (gc_data$CO2_uatm) * (gc_data$Equilibration_Volume_gas_L ) * gc_data$CO2_kh
  gc_data$CH4_umol_equilibrated_water <- (gc_data$CH4_uatm) * (gc_data$Equilibration_Volume_gas_L ) * gc_data$CH4_kh
  gc_data$N2O_umol_equilibrated_water <- (gc_data$N2O_uatm) * (gc_data$Equilibration_Volume_gas_L ) * gc_data$N2O_kh
    
# 03. Concentration in initial water sample ----  
    # Add up the total moles of gas in the headspace and the water after equillibration (because it all came from the water before equilibration) 
    # and divide by the volume of water you collected to get from moles to moles per liter (divide volume in mL by 1000 to get L )
  
  # Concentration: umol per liter 
  gc_data$CO2_umol_L <- (gc_data$CO2_umol_equilibrated_headspace + gc_data$CO2_umol_equilibrated_water ) / (gc_data$Equilibration_Volume_water_L)
  gc_data$CH4_umol_L <- (gc_data$CH4_umol_equilibrated_headspace + gc_data$CH4_umol_equilibrated_water ) / (gc_data$Equilibration_Volume_water_L)
  gc_data$N2O_umol_L <- (gc_data$CH4_umol_equilibrated_headspace + gc_data$N2O_umol_equilibrated_water ) / (gc_data$Equilibration_Volume_water_L)
  
  # Partial pressure: uatm 
  gc_data$pCO2_uatm  <- gc_data$CO2_umol_L / gc_data$CO2_kh
  gc_data$pCH4_uatm  <- gc_data$CH4_umol_L / gc_data$CH4_kh
  gc_data$pN2O_uatm  <- gc_data$N2O_umol_L / gc_data$N2O_kh
  
  # Subset to simplified columns 
  gas_conc_data <- subset(gc_data, select = c("Lake_ID", "Date_Collected", "Sample_Type", "Depth_BS_m", 
                                              "CO2_umol_L", "CH4_umol_L", "N2O_umol_L", 
                                              "pCO2_uatm", "pCH4_uatm",  "pN2O_uatm" ))
      
  # **** you will need to figure out how to deal with your air samples   
  gas_conc_data <- gas_conc_data[! gas_conc_data$Sample_Type == "AR", ]
  
  # Save output Files 
  write_xlsx(gas_conc_data, "01_Data/GC/05_Output_Concentration_Data/20250207_gas_conc_data.xlsx") 
  write_xlsx(gc_data, "01_Data/GC/05_Output_Concentration_Data/20250207_gc_data.xlsx") 
  
# Playing with Plots ----
  
  # Format data to plot 
  gas_conc_data$Lake_Site <- paste(gas_conc_data$Lake_ID, gas_conc_data$Sample_Type, sep = "_")
  gas_conc_data$Date_Formatted <- paste(substring(gas_conc_data$Date_Collected, 1, 4), 
                                        substring(gas_conc_data$Date_Collected, 5, 6), 
                                        substring(gas_conc_data$Date_Collected, 7, 8), sep= "-") %>% 
                                  as.POSIXct()
  
# Make Preliminary Plot of Raw data 
  CO2_plot <- ggplot(gas_conc_data, aes(x= Date_Formatted, y= CO2_umol_L, color = Lake_ID, shape = Sample_Type, group = Lake_Site)) +
    # geom_line() + 
    geom_point() +
    theme_bw() + 
    labs(x = "Date", y = "CO2 Concentration (umol/L)", 
         title =  "CO2 over time") 
  CO2_plot   
  
  N2O_plot <- ggplot(gas_conc_data, aes(x= Date_Formatted, y= N2O_umol_L, color = Lake_ID, shape = Sample_Type, group = Lake_Site)) +
    # geom_line() + 
    geom_point() +
    theme_bw() + 
    labs(x = "Date", y = "N2O Concentration (umol/L)", 
         title =  "N2O over time") 
  N2O_plot   
      
  CH4_plot <- ggplot(gas_conc_data, aes(x= Date_Formatted, y= CH4_umol_L, color = Lake_ID, shape = Sample_Type, group = Lake_Site)) +
    # geom_line() + 
    geom_point() +
    theme_bw() + 
    labs(x = "Date", y = "CH4 Concentration (umol/L)", 
         title =  "CH4 over time") 
  CH4_plot   

  
# Average by Lake site 
  
# Summarize data: Calculate mean CH4 for each Lake_Site and Date_Formatted
  avg_gas_conc_data_ch4 <- gas_conc_data %>%
    group_by(Date_Formatted, Lake_Site) %>%
    summarise(CH4_mean = mean(CH4_umol_L, na.rm = TRUE), .groups = "drop")
  
  avg_gas_conc_data_co2 <- gas_conc_data %>%
    group_by(Date_Formatted, Lake_Site) %>%
    summarise(CO2_mean = mean(CO2_umol_L, na.rm = TRUE), .groups = "drop")
  
  avg_gas_conc_data_n2o <- gas_conc_data %>%
    group_by(Date_Formatted, Lake_Site) %>%
    summarise(N2O_mean = mean(N2O_umol_L, na.rm = TRUE), .groups = "drop")
  
  avg_gas_conc_data <- full_join(avg_gas_conc_data_ch4, avg_gas_conc_data_co2)
  avg_gas_conc_data <- full_join(avg_gas_conc_data, avg_gas_conc_data_n2o)
  avg_gas_conc_data$Lake <- substring(avg_gas_conc_data$Lake_Site, 1, 3)
  avg_gas_conc_data$Site <- substring(avg_gas_conc_data$Lake_Site, 5, 6)
  
  head(avg_gas_conc_data)
  
  # Create the plot
  
  # CH4 AVG ___
  CH4_avg_plot <- ggplot(avg_gas_conc_data, aes(x = Date_Formatted, y = CH4_mean, 
                                                color = Lake, shape = Site, group = Lake_Site)) +
    geom_point(size = 2) + 
    theme_bw() + 
    labs(x = "Date", y = "Average CH4 Concentration (umol/L)", 
         title = "Average CH4 over Time")
  CH4_avg_plot
  
  # CO2 AVG ___
  CO2_avg_plot <- ggplot(avg_gas_conc_data, aes(x = Date_Formatted, y = CO2_mean, 
                                                color = Lake, shape = Site, group = Lake_Site)) +
    geom_point(size = 2) + 
    theme_bw() + 
    labs(x = "Date", y = "Average CO2 Concentration (umol/L)", 
         title = "Average CO2 over Time")
  CO2_avg_plot
  
  # N20 Avg ___
  N2O_avg_plot <- ggplot(avg_gas_conc_data, aes(x = Date_Formatted, y = N2O_mean, 
                                                color = Lake, shape = Site, group = Lake_Site)) +
    geom_point(size = 2) + 
    theme_bw() + 
    labs(x = "Date", y = "Average N2O Concentration (umol/L)", 
         title = "Average N2O over Time")
  N2O_avg_plot
  




