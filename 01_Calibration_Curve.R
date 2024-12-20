## CALIBRATION CURVE ----

# 00. Set Up R Environment ----

    # Set working directory 
    setwd("/Users/altagannon/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/05_Research_Projects/02_GHG_Under_Ice")
    
    # Load packages and functions 
    source("02_Analysis/GC_Data_Workup/00_libraries.R")
    source("02_Analysis/GC_Data_Workup/00_functions.R")
    
    # Load data 
    calib <- read_excel("01_Data/GC/02_WorkUp/RWorkUp_InputData/20241216_Calibration.xlsx")
    drift <- read_excel("01_Data/GC/02_WorkUp/RWorkUp_InputData/20241216_Drift.xlsx")
    samples <- read_excel("01_Data/GC/02_WorkUp/RWorkUp_InputData/20241216_Samples.xlsx")
    
## 01. Format Calibration Data  ----
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
        calib_co2 <- calib[calib$Sample_Type %in% c("atm", "chk", "std_CO2"), ]
        calib_ch4 <- calib[calib$Sample_Type %in% c("atm", "chk", "std_CH4"), ]
        calib_n2o <- calib[calib$Sample_Type %in% c("atm", "chk", "std_N2O"), ]
        
    # Subset to only columns and peaks that you need for each gas 
        calib_co2 <- subset(calib_co2, select = c("Run_Date", "Sample_ID", "Sample_Type", "TCD_peak"))
        calib_ch4 <- subset(calib_ch4, select = c("Run_Date", "Sample_ID", "Sample_Type", "FID_peak"))
        calib_n2o <- subset(calib_n2o, select = c("Run_Date", "Sample_ID", "Sample_Type", "ECD_peak"))
        
    # Add columns for expected to each calibration based on the standards you used and entered above 
        calib_co2$expected_co2 <- ifelse(calib_co2$Sample_Type == "atm", atm_co2, 
                                         ifelse(calib_co2$Sample_Type == "chk", chk_co2, 
                                                ifelse(calib_co2$Sample_Type == "std_CO2", std_co2, NA)))
        
        calib_ch4$expected_ch4 <- ifelse(calib_ch4$Sample_Type == "atm", atm_ch4, 
                                         ifelse(calib_ch4$Sample_Type == "chk", chk_ch4, 
                                                ifelse(calib_ch4$Sample_Type == "std_CH4", std_ch4, NA)))
        
        calib_n2o$expected_n2o <- ifelse(calib_n2o$Sample_Type == "atm", atm_n2o, 
                                         ifelse(calib_n2o$Sample_Type == "chk", chk_n2o, 
                                                ifelse(calib_n2o$Sample_Type == "std_N2O", std_n2o, NA)))
        
    # Format column types 
        format_columns_FUNC <- function(df){
          df <- as.data.frame(df)
          df$Run_Date <- as.character(df$Run_Date)
          df$Sample_ID <- as.character( df$Sample_ID)
          df$Sample_Type <- as.character(df$Sample_Type)
          df[ , 4] <- as.numeric(df[, 4])
          df[ , 5] <- as.numeric(df[, 5])
          df
        }
        
        calib_co2 <- format_columns_FUNC(calib_co2)
        calib_ch4 <- format_columns_FUNC(calib_ch4)
        calib_n2o <- format_columns_FUNC(calib_n2o)
       
        
##02. Plot Calibration Curves ---- 
#Plot Expected vs. Area for each gas 
      
  # Write Plotting Fucntions  
    # CO2  
        Calib_Curve_CO2_FUNC <- function(calib_co2){
          calib_co2 %>%
            ggplot(aes(
              x = TCD_peak,
              y = expected_co2)) + 
            geom_point(color= "#39B600") +
            theme_bw() + 
            labs(x = "TCD Peak Area",
                 y = "Expected CO2 (ppm)", 
                 title = "Calibration Curve CO2") + 
            geom_smooth(method = "lm",  #add a linear trend
                        se = TRUE, 
                        color = "#39B600") + #Include error bars around the trend
            stat_cor(#These next few lines add the model summaries to your graph
              aes(label = paste(..rr.label..,
                                ..p.label..,
                                sep = "~`,`~")),
              label.y = 800, #You may need to adjust this label position
              digits = 2,#How many significant digits
              size=5) +  #specify font size
            stat_regline_equation(label.y = 750,#Add the equation to the graph
                                  size=5) #specify font size
        }
       
        
    # CH4
        Calib_Curve_CH4_FUNC <- function(calib_ch4){
          calib_ch4 %>%
            ggplot(aes(
              x = FID_peak,
              y = expected_ch4)) + 
            geom_point(color= "#FF3D00") +
            theme_bw() + 
            labs(x = "FID Peak Area",
                 y = "Expected CH4 (ppm)", 
                 title = "Calibration Curve CH4") + 
            geom_smooth(method = "lm",  #add a linear trend
                        se = TRUE, 
                        color = "#FF3D00") + #Include error bars around the trend
            stat_cor(#These next few lines add the model summaries to your graph
              aes(label = paste(..rr.label..,
                                ..p.label..,
                                sep = "~`,`~")),
              label.y = 90, #You may need to adjust this label position
              digits = 2,#How many significant digits
              size=5) +  #specify font size
            stat_regline_equation(label.y = 80,#Add the equation to the graph
                                  size=5) #specify font size
        }
        
    #N2O 
        Calib_Curve_N2O_FUNC <- function(calib_n2o){
          calib_n2o %>%
            ggplot(aes(
              x = ECD_peak,
              y = expected_n2o)) + 
            geom_point(color= "#C77CFF") +
            theme_bw() + 
            labs(x = "ECD Peak Area",
                 y = "Expected N2O (ppm)", 
                 title = "Calibration Curve N2O") + 
            geom_smooth(method = "lm",  #add a linear trend
                        se = TRUE, 
                        color = "#C77CFF") + #Include error bars around the trend
            stat_cor(#These next few lines add the model summaries to your graph
              aes(label = paste(..rr.label..,
                                ..p.label..,
                                sep = "~`,`~")),
              label.y = 5, #You may need to adjust this label position
              digits = 2,#How many significant digits
              size=5) +  #specify font size
            stat_regline_equation(label.y = 4,#Add the equation to the graph
                                  size=5) #specify font size
        }

        
    # Check Calibration Curves 
        calibration_curve_co2 <- Calib_Curve_CO2_FUNC(calib_co2)
        calibration_curve_co2
        
        calibration_curve_ch4 <- Calib_Curve_CH4_FUNC(calib_ch4)
        calibration_curve_ch4
        
        calibration_curve_n2o <- Calib_Curve_N2O_FUNC(calib_n2o)
        calibration_curve_n2o
        
    # Remove any Points 
        
        # CO2
        calibration_curve_co2
        calib_co2_full <- calib_co2
        calib_co2 <- calib_co2[!calib_co2$Sample_ID =="1000ppm CO2 4",]
        
    # Record what points (if any) were removed from calibration curve for each gas 
        removed_co2 <- "1000ppm CO2 4"
        removed_ch4 <- "NA"
        removed_n2o <- "NA"
        
    # Add any Calibration Notes 
        calib_notes_co2 <- "NA"
        calib_notes_ch4 <- "NA"
        calib_notes_n2o <- "NA"
        
# 02. Linear regression for each gas and save slope and intercept  
        
      # CO2
        calib_co2_lm <- lm(expected_co2 ~ TCD_peak, data=calib_co2) # run linear model 
        co2_r2 <- summary(calib_co2_lm)$r.squared %>% as.numeric() # save R2
        co2_intercept <- coef(calib_co2_lm)[1] %>% as.numeric() #save intercept 
        co2_slope <- coef(calib_co2_lm)[2] %>% as.numeric()  # save slope 
        
      # CH4
        calib_ch4_lm <- lm(expected_ch4 ~ FID_peak, data=calib_ch4) # run linear model 
        ch4_r2 <- summary(calib_ch4_lm)$r.squared %>% as.numeric() # save R2
        ch4_intercept <- coef(calib_ch4_lm)[1] %>% as.numeric() #save intercept 
        ch4_slope <- coef(calib_ch4_lm)[2] %>% as.numeric()  # save slope 
        
      # N2O
        calib_n2o_lm <- lm(expected_n2o ~ ECD_peak, data=calib_n2o) # run linear model 
        n2o_r2 <- summary(calib_n2o_lm)$r.squared %>% as.numeric() # save R2
        n2o_intercept <- coef(calib_n2o_lm)[1] %>% as.numeric() #save intercept 
        n2o_slope <- coef(calib_n2o_lm)[2] %>% as.numeric()  # save slope 
      
# 03. Save calib coeficients as a dataframe 
        co2_calib_coefs <- c("CO2", co2_r2, co2_intercept, co2_slope, removed_co2, calib_notes_co2)
        ch4_calib_coefs <- c("CH4", ch4_r2, ch4_intercept, ch4_slope, removed_ch4, calib_notes_ch4)
        n2o_calib_coefs <- c("N2O", n2o_r2, n2o_intercept, n2o_slope, removed_n2o, calib_notes_n2o)
        
        calib_coefs <- rbind(co2_calib_coefs, ch4_calib_coefs ,  n2o_calib_coefs) %>% as.data.frame()
        
        
        # [Once you have the slope and intercept for each gas for a day save it as a df and then you can just pull when you need and you don't have to re-run the calibration curve every time]
        # [ same for drift! ]
        