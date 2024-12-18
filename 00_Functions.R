#Function for calculating water-year DOY. This will help facilitate plotting and analysizing trends in ice-in since they span either side of the winter-year (e.g., 2011-2012). For example, an IceInDayofYear_fed value of 150 means Ice-In occured 150 days after the start of the water-year (Oct1)

hydro.day = function(x, start.month = 10L) {
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}


# Write a Function to Add water year 
# NOTE: this function grabs everything after September 1, generally water year starts Oct 1 but GL4 freezes in early Oct

WaterYear_FUNC <- function(input_df){
  input_df$Month <-lubridate:: month(input_df$timestamp)
  input_df$year <- lubridate::year(input_df$timestamp)
  input_df$year <- as.numeric(input_df$year)
  input_df$water_year <- ifelse(input_df$Month >= 9, input_df$year + 1, 
                                input_df$year)
  input_df$water_year <- as.character(input_df$water_year)
  output <- as.data.frame(input_df)
}

# Katie Function to Calculate Water Year -- Currently broken, runs on things one at a time but will not run over the full column in df 

HydroDay_FUNC <-  function(input_date) {
  
  input_date <- as.POSIXct(input_date, tz = "MST", format = "%Y-%m-%d") # Set as a POSIXct 
  
  water_year_correction <- ifelse(month(input_date) < 10, 1, 0) #if is is later than october 1 you need to start in the october of the previous year
  start_yr <-  year(input_date) - water_year_correction
  
  start_date <- paste(start_yr, "10", "01", sep = "-") #Start date is October 1 
  start_date <- as.POSIXct(start_date, tz = "MST", format = "%Y-%m-%d") # Set as a POSIXct 
  
  hydro_date <- as.integer(input_date - start_date + (24*60*60)) / (24*60*60)
}


