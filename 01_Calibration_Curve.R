## CALIBRATION CURVE ----

# 00. Set Up R Environment ----

    # Set working directory 
    # setwd("/Users/altagannon/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/05_Research_Projects/02_GHG_Under_Ice") # Laptop KAG
    setwd("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/05_Research_Projects/02_GHG_Under_Ice")    # Desktop SEEC

    # Load packages and functions 
    source("02_Analysis/GC_Data_Workup/00_libraries.R")
    source("02_Analysis/GC_Data_Workup/00_functions.R")
    
    # Load data 
    calib <- read_excel("01_Data/GC/02_WorkUp/RWorkUp_InputData/20241216_Calibration.xlsx")
    drift <- read_excel("01_Data/GC/02_WorkUp/RWorkUp_InputData/20241216_Drift.xlsx")
    samples <- read_excel("01_Data/GC/02_WorkUp/RWorkUp_InputData/20241216_Samples.xlsx")
    
    # Save Notes date GC run and date calibraiton calculated 
    gc_run_date <- "20241216" %>% as.character()
    calib_calc_date <- "20241219" %>% as.character()
    
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
            geom_point(color= "blue") +
            theme_bw() + 
            labs(x = "ECD Peak Area",
                 y = "Expected N2O (ppm)", 
                 title = "Calibration Curve N2O") + 
            geom_smooth(method = "lm",  #add a linear trend
                        se = TRUE, 
                        color = "blue") + #Include error bars around the trend
            stat_cor(#These next few lines add the model summaries to your graph
              aes(label = paste(..rr.label..,
                                ..p.label..,
                                sep = "~`,`~")),
              label.y = 5, #You may need to adjust this label position
              digits = 2,#How many significant digits
              size=5) +  #specify font size
            stat_regline_equation(label.y = 4.7,#Add the equation to the graph
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
        
# 02. Linear regression for each gas and save slope and intercept  
        
      # CO2
        calib_co2_lm <- lm(expected_co2 ~ TCD_peak, data=calib_co2) # run linear model 
        co2_r2 <- summary(calib_co2_lm)$r.squared %>% as.numeric() %>% round(3)  # save R2
        co2_intercept <- coef(calib_co2_lm)[1] %>% as.numeric() %>% round(3)  #save intercept 
        co2_slope <- coef(calib_co2_lm)[2] %>% as.numeric()  %>% round(3)  # save slope 
        
      # CH4
        calib_ch4_lm <- lm(expected_ch4 ~ FID_peak, data=calib_ch4) # run linear model 
        ch4_r2 <- summary(calib_ch4_lm)$r.squared %>% as.numeric() %>% round(3)  # save R2
        ch4_intercept <- coef(calib_ch4_lm)[1] %>% as.numeric() %>% round(3)  #save intercept 
        ch4_slope <- coef(calib_ch4_lm)[2] %>% as.numeric()  %>% round(3)  # save slope 
        
      # N2O
        calib_n2o_lm <- lm(expected_n2o ~ ECD_peak, data=calib_n2o) # run linear model 
        n2o_r2 <- summary(calib_n2o_lm)$r.squared %>% as.numeric() %>% round(3)  # save R2
        n2o_intercept <- coef(calib_n2o_lm)[1] %>% as.numeric() %>% round(3)  #save intercept 
        n2o_slope <- coef(calib_n2o_lm)[2] %>% as.numeric()  %>% round(3)  # save slope 
        
# 03. Drift Check ----
        
  head(drift)
        
  # Plot FID over the course of the Run (CH4)
       drift_ch4_plot <- drift %>%
          ggplot(aes(
            x = Sample_Location_GC ,
            y = FID_peak)) + 
          geom_point(color= "#FF3D00") +
          theme_bw() + 
          labs(x = "Sample Location on GC",
               y = "FID Peak", 
               title = "Drift Check CH4") + 
         geom_smooth(method = "lm",  #add a linear trend
                     se = TRUE, 
                     color = "#FF3D00") + #Include error bars around the trend
         stat_cor(#These next few lines add the model summaries to your graph
           aes(label = paste(..rr.label..,
                             ..p.label..,
                             sep = "~`,`~")),
           label.y = 11, #You may need to adjust this label position
           digits = 2,#How many significant digits
           size=5) +  #specify font size
         stat_regline_equation(label.y = 10.5,#Add the equation to the graph
                               size=5) 
       drift_ch4_plot
        
  # Plot TCD over the course of the run (CO2)
       drift_co2_plot <- drift %>%
         ggplot(aes(
           x = Sample_Location_GC ,
           y = TCD_peak)) + 
         geom_point(color= "#39B600") +
         theme_bw() + 
         labs(x = "Sample Location on GC",
              y = "TCD Peak", 
              title = "Drift Check CO2") +
         geom_smooth(method = "lm", se = TRUE, color = "#39B600") + #Include error bars around the trend
         stat_cor(#These next few lines add the model summaries to your graph
           aes(label = paste(..rr.label..,
                             ..p.label..,
                             sep = "~`,`~")),
           label.y = 27, #You may need to adjust this label position
           digits = 2,#How many significant digits
           size=5) +  #specify font size
         stat_regline_equation(label.y = 28,#Add the equation to the graph
                               size=5) 
       drift_co2_plot
        
  # Plot ECD over the course of the run 
       drift_n2o_plot <- drift %>%
         ggplot(aes(
           x = Sample_Location_GC ,
           y = ECD_peak)) + 
         geom_point(color= "blue") +
         theme_bw() + 
         labs(x = "Sample Location on GC",
              y = "ECD Peak", 
              title = "Drift Check N2O") + 
         geom_smooth(method = "lm", se = TRUE, color = "blue") + #Include error bars around the trend
         stat_cor(#These next few lines add the model summaries to your graph
           aes(label = paste(..rr.label..,
                             ..p.label..,
                             sep = "~`,`~")),
           label.y = 132, #You may need to adjust this label position
           digits = 2,#How many significant digits
           size=5) +  #specify font size
         stat_regline_equation(label.y = 128,#Add the equation to the graph
                               size=5) 
    # View Drift Plots 
       drift_co2_plot
       drift_ch4_plot
       drift_n2o_plot
       
  ## Add saving R2, then if p value is bellow 0.05 and R2 above 0.65 do drift correction 
       
    # linear model for drift and save p-value   
       drift_ch4_lm <- lm(FID_peak ~ Sample_Location_GC, data=drift) # run linear model 
       drift_ch4_r2 <- summary(drift_ch4_lm)$r.squared %>% as.numeric() %>% round(3)
       drift_ch4_pval <- summary(drift_ch4_lm)$coefficients[2,4] %>% as.numeric() %>% round(3)  
           drift_ch4_intercept <- coef(drift_ch4_lm)[1] %>% as.numeric  %>% round(3)  # Save intercept and slope from Drift linear model and use to correct drift iff necessary? 
           drift_ch4_slope <- coef(drift_ch4_lm)[2] %>% as.numeric%>% round(3)  
       
       drift_co2_lm <- lm(TCD_peak ~ Sample_Location_GC, data=drift) # run linear model
       drift_co2_r2 <- summary(drift_co2_lm)$r.squared %>% as.numeric() %>% round(3)
       drift_co2_pval <- summary(drift_co2_lm)$coefficients[2,4] %>% round(3)  
           drift_co2_intercept <- coef(drift_co2_lm)[1] %>% as.numeric %>% round(3)   # Save intercept and slope from Drift linear model and use to correct drift iff necessary? 
           drift_co2_slope <- coef(drift_co2_lm)[2] %>% as.numeric %>% round(3)  
       
       drift_n2o_lm <- lm(ECD_peak ~ Sample_Location_GC, data=drift) # run linear model 
       drift_n2o_r2 <- summary(drift_n2o_lm)$r.squared %>% as.numeric() %>% round(3)
       drift_n2o_pval <- summary(drift_n2o_lm)$coefficients[2,4] %>% round(3)
           drift_n2o_intercept <- coef(drift_n2o_lm)[1] %>% as.numeric %>% round(3)   # Save intercept and slope from Drift linear model and use to correct drift iff necessary? 
           drift_n2o_slope <- coef(drift_n2o_lm)[2] %>% as.numeric %>% round(3)  
           
     # Add any Notes on calibration or drift 
         notes_co2 <- "NA"
         notes_ch4 <- "NA"
         notes_n2o <- "NA"
           
# 04. Save calib coeficients as a dataframe ----
   co2_calib_coefs <- c(gc_run_date, calib_calc_date, "CO2", 
                        co2_r2, co2_intercept, co2_slope, removed_co2, 
                        drift_co2_pval, drift_co2_r2, drift_co2_intercept, drift_co2_slope, 
                        notes_co2)
   ch4_calib_coefs <- c(gc_run_date, calib_calc_date, "CH4", 
                        ch4_r2, ch4_intercept, ch4_slope, removed_ch4, 
                        drift_ch4_pval, drift_ch4_r2, drift_ch4_intercept, drift_ch4_slope, 
                        notes_ch4)
   n2o_calib_coefs <- c(gc_run_date, calib_calc_date, "N2O", 
                        n2o_r2, n2o_intercept, n2o_slope, removed_n2o, 
                        drift_n2o_pval, drift_n2o_r2, drift_n2o_intercept, drift_n2o_slope, 
                        notes_n2o)
   
   calib_coefs <- rbind(co2_calib_coefs, ch4_calib_coefs ,  n2o_calib_coefs) %>% as.data.frame()
   names(calib_coefs) <- c("GC_Run_Date", "Calc_Date", "Gas",
                           "Calib_R2", "Calib_Intercept", "Calib_Slope", "Calib_Points_Removed", 
                           "Drift_pvalue", "Drift_R2","Drift_Intercept", "Drift_Slope","Notes")
   row.names(calib_coefs) <- seq(1:nrow(calib_coefs))
   
   # Save output 
    # write_xlsx(calib_coefs, "01_Data/GC/03_Calibration_Coef/20241216_calib_coefs.xlsx")
  
## 05. Drift Correct if necessary ----
   # correct if if p value is significant and R2 is greater than 0.65 (Prairie 1996) via Pascal and Nick in Holgerson Lab pipeline for GC drift corrections
   
   names(samples)[names(samples) == "FID_peak"] <- "FID_peak_raw"
   names(samples)[names(samples) == "TCD_peak"] <- "TCD_peak_raw"
   names(samples)[names(samples) == "ECD_peak"] <- "ECD_peak_raw"
   
   samples$drift_ch4_pval <- drift_ch4_pval
   samples$drift_co2_pval <- drift_co2_pval
   samples$drift_n2o_pval <- drift_n2o_pval
   
   samples$drift_ch4_r2 <- drift_ch4_r2
   samples$drift_co2_r2 <- drift_co2_r2
   samples$drift_n2o_r2 <- drift_n2o_r2
   
   samples$FID_peak_corrected <- ifelse(samples$drift_ch4_pval >= 0.05, 
                                        samples$FID_peak_raw,
                                        as.numeric(samples$FID_peak_raw * drift_ch4_slope + drift_ch4_intercept ))
        
   samples$TCD_peak_corrected <- ifelse(samples$drift_co2_pval >= 0.05, 
                                        samples$TCD_peak_raw,
                                        as.numeric(samples$TCD_peak_raw * drift_co2_slope + drift_co2_intercept ))
   
   samples$ECD_peak_corrected <- ifelse(samples$drift_n2o_pval >= 0.05, 
                                        samples$ECD_peak_raw,
                                        as.numeric(samples$ECD_peak_raw * drift_n2o_slope + drift_n2o_intercept ))

## 06. Convert to ppm ----
   
   samples$CH4_ppm <- as.numeric(samples$FID_peak_corrected * ch4_slope + ch4_intercept)
   samples$CO2_ppm <- as.numeric(samples$TCD_peak_corrected * co2_slope + co2_intercept)
   samples$N2O_ppm <- as.numeric(samples$ECD_peak_corrected * n2o_slope + n2o_intercept)
   
## 07. Format and save clean dataframe ---- 
 
   #Subset to only the columns that you want 
   names(samples)
   samples$Calib_Calc_Date <- calib_calc_date
   samples_clean <- subset(samples, select = c("Run_Date", "Calib_Calc_Date", "Sample_ID", "CH4_ppm", "CO2_ppm" , "N2O_ppm" ))
   head(samples_clean)     
   
   # Save output (save by run date)
   write_excel_csv2(samples_clean, "01_Data/GC/04_Cleaned_Data/20241216_samples_clean.xlsx")     
   
  # Merge with sample information   
        
        
        