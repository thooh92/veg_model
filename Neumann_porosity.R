## Author: Thomas Ohnemus
## Date: 05/12/2024
## Using Neumann Model for Porosity 

# Prepare Script
rm(list = ls())

library(tidyverse)
library(rdwd)

# Load Test data from DWD in hourly resolution; check dates for bud burst
url <- selectDWD("Wernigerode", res = "hourly", var = "moisture", per = "hist", current = T)
dat <- dataDWD(url, varnames = T, force = F)

# Wrangle data to use in function
dat$MESS_DATUM <- as.POSIXct(dat$MESS_DATUM, format = "%Y-%m-%d %H:%M:%S")
dat$per        <- as.numeric(difftime(dat$MESS_DATUM, lag(dat$MESS_DATUM), units = "hours"))

# Check for only hourly intervals
dat            <- dat[dat$per == 1 & !is.na(dat$MESS_DATUM),]

# Get time & temp
time           <- dat$MESS_DATUM[!is.na(dat$MESS_DATUM)]
temp           <- dat$TT_STD.Lufttemperatur[!is.na(dat$MESS_DATUM)]


# Generalize function
# Neumann's functions

  # For Poro application: function should calculate a % of growth completed based on GDD
neumann_growth <- function(temp, time, base = 4, year){
  # Calculate T exceeding base
  T_base <- temp - base
  
  # Create daily data
  days      <- as.Date(time)
  day_df    <- data.frame(cbind(as.Date(days), T_base))
  day_df    <- day_df %>% group_by(V1) %>% summarize(temp = mean(T_base, na.rm = T))
  
  # Subset to days after bud break
  day_df$V1 <- as.Date(day_df$V1)
  day_df    <- day_df[day_df$V1 > as.Date(paste0(year,"-01-01")) &
                        day_df$V1 <= as.Date(paste0(year,"-01-01")) + days(350),]
  
  # Reset T_base < 0 to 0 for cumulative T
  day_df$temp[day_df$temp < 0] <- 0 
  
  # Cumulate T
  day_df$T_cum  <- cumsum(day_df$temp)
  
  
  # Percentage of completed growth
  # spurs: 500 DD; n = 264; 0.00004 m² * DD
  # long stems: 900 DD; n = 71; 0.00008 m² * DD
  day_df$LeafArea_m2    <- 
    ifelse(day_df$T_cum < 500, day_df$T_cum * 0.00004*264 + day_df$T_cum*0.00008*71,
           ifelse(day_df$T_cum >= 500 & day_df$T_cum < 900, 
                  500 * 0.00004*264 + day_df$T_cum*0.00008*71, 
                  500 * 0.00004*264 + 900*0.00008*71))
  
  day_df$growth_percent <- day_df$LeafArea_m2/max(day_df$LeafArea_m2, na.rm = T)
  

  # Porosity
  day_df$poro   <- 90 - 80 * day_df$growth_percent

  # Calculate Height, Length & Width Extension
  day_df$height_extension <- 0.10 * day_df$growth_percent
  day_df$length_extension <- 2*day_df$height_extension
  day_df$width_extension <- 2*day_df$height_extension

  # Vegetation milestones
  # fruit development: 170 degree days after bud break (DDBB)
  day_df$fruit_development <- F
  day_df$fruit_development[min(which(day_df$T_cum > 170))] <- T
  
  # full bloom:190 DDBB
  day_df$full_bloom <- F
  day_df$full_bloom[min(which(day_df$T_cum > 190))] <- T
  
  # fruit abscission: 720 DDBB (using a fruit abscission curve); then fruit number constant
  day_df$fruit_abscission <- F
  day_df$fruit_abscission[min(which(day_df$T_cum > 720))] <- T
  
  
  # Return data.frame
  return(day_df)
}

# Apply Neumann Poro Growth
output_all <- data.frame()
years      <- c(1980:1990,1992:2000)

for(i in 1:length(years)){
  df <- neumann_growth(temp = temp, time = time, year = years[i])
  
  output_all <- rbind(output_all, df)
}

# Plot results
output_all <- pivot_longer(output_all, cols = 6:9,
                           values_to = "value", names_to = "geometry")

output_all$doy <- as.numeric(format(output_all$V1, format = "%j"))

ggplot(output_all, aes(x = doy, y = value)) +
  geom_point(alpha = 0.1) + theme_bw() + 
  labs(y = "", x = "DOY") +
  facet_wrap(~geometry, scales = "free_y", ncol = 1) +
  xlim(0, 250)
ggsave("C:/Docs/MIRO/vegetation_model/poro_evolution.png", units = "cm",
       dpi = 300, width = 15, height = 20)

