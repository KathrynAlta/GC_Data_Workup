## CALIBRATION CURVE ----

# 00. Set Up R Environment ----

    # Set working directory 
    setwd("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/05_Research_Projects/02_GHG_Under_Ice")
    
    # Load packages and functions 
    source("02_Analysis/GC_Data_Workup/00_libraries.R")
    source("02_Analysis/GC_Data_Workup/00_functions.R")
    
    # Load data 
    calib <- read_excel("01_Data/GC/02_WorkUp/RWorkUp_InputData/20241216_Calibration.xlsx")
    drift <- read_excel("01_Data/GC/02_WorkUp/RWorkUp_InputData/20241216_Drift.xlsx")
    samples <- read_excel("01_Data/GC/02_WorkUp/RWorkUp_InputData/20241216_Samples.xlsx")
    
## 01. Set Up Calibration Data  ----
    # Enter Standard Concentrations used 
        chk_co2 <- 600
        atm_co2 <- 422.3
        std_co2 <- 1000
        
        chk_ch4 <- 5
        atm_ch4 <- 1.97
        std_ch4 <- 100
        
        chk_n2o <- 1
        atm_n2o <- 0.34
        std_n2o <- 5
        
    # Seperate Calib data by gas (Add if used more than one standard for each gas)
        head(calib)
        co2_calib_samp_types <- c("atm", "chk", "std_CO2")
        ch4_calib_samp_types <- c("atm", "chk", "std_CH4")
        n2o_calib_samp_types <- c("atm", "chk", "std_N2O")
        
        calib_co2 <- calib[calib$Sample_Type %in% co2_calib_samp_types, ]
        calib_ch4 <- calib[calib$Sample_Type %in% ch4_calib_samp_types , ]
        calib_n2o <- calib[calib$Sample_Type %in% n2o_calib_samp_types, ]
        
    # Subset to only columns and peaks that you need for each gas 
        calib_co2 <- subset(calib_co2, select = c("Run_Date", "Sample_ID", "Sample_Type", "TCD_peak"))
        calib_ch4 <- subset(calib_ch4, select = c("Run_Date", "Sample_ID", "Sample_Type", "FID_peak"))
        calib_n2o <- subset(calib_n2o, select = c("Run_Date", "Sample_ID", "Sample_Type", "ECD_peak"))
        
    # Add columns for expected to each calibration 
        head(calib_co2)
        calib_co2$expected_co2 <- ifelse(calib_co2$Sample_Type == "atm", atm_co2, 
                                         ifelse(calib_co2$Sample_Type == "chk", chk_co2, 
                                                ifelse(calib_co2$Sample_Type == "std_CO2", std_co2, NA)))
        
        calib_ch4$expected_ch4 <- ifelse(calib_ch4$Sample_Type == "atm", atm_ch4, 
                                         ifelse(calib_ch4$Sample_Type == "chk", chk_ch4, 
                                                ifelse(calib_ch4$Sample_Type == "std_CH4", std_ch4, NA)))
        
        calib_n2o$expected_n2o <- ifelse(calib_n2o$Sample_Type == "atm", atm_n2o, 
                                         ifelse(calib_n2o$Sample_Type == "chk", chk_n2o, 
                                                ifelse(calib_n2o$Sample_Type == "std_N2O", std_n2o, NA)))
          
# 01. Plot Expected vs. Area for each gas 
        # NEXT STEP: Make this pretty and add info 
        # CO2 
        co2_calib_curve <- ggplot(data = calib_co2, aes(x = TCD_peak, y = expected_co2)) +
          geom_point()
        co2_calib_curve
        
# 02. Linear regression for each gas 
      
# 03. Save calib 
        
        
        # [Once you have the slope and intercept for each gas for a day save it as a df and then you can just pull when you need and you don't have to re-run the calibration curve every time]
        # [ same for drift! ]
        