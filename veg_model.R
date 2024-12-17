## Author: Thomas Ohnemus
## Date: 22/10/2024
## Vegetation Submodel

# Prepare Script
rm(list = ls())

library(tidyverse)
library(data.table)
library(rdwd)
library(lubridate)
library(chillR)

# Define functions
  # bud break (Young & Werner 1985)
bud_break_seq <- function(temp, time, base = 4.5){ # Adapting for M9 rootstock; T in hourly frequency
  # Subtract base from temp
  T_base  <- temp - base
  
  # Get year
  year    <- unique(format(time, format = "%Y"))
  
  # empty vector to store date when chill units and growing degree days are exceeded in each year
  CU_date   <- as.POSIXct(c())
  GDH_date  <- as.POSIXct(c())
  
  # Loop through years
  for(i in 1:length(year)){
    print(i)
    
    #### Chill Units
    # Get 1st of September of each year
    start  <- as.POSIXct(paste0(year[i],"-09-01 00:00:00"), 
                         format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    
    # Subset Temperatures to time > start time
    T_sub <- T_base[time >= start & time <= start + years(1)] 
    
    # Subset times
    time_sub <- time[time >= start & time <= start + years(1)] 
    
    # Set values > 0 to 0 and < 0 to 1, since one chill unit is one hour below 4.5 °C
    CU      <- ifelse(T_sub >= 0, 0, 1)
    
    # Accumulate chill units
    CU      <- cumsum(abs(CU))
    
    # Get datetime, when CU demand (= 1190 CU) is first met
    CU_date[i] <- time_sub[min(which(CU >= 1190))]
    
    
    #### Growing Degree Hours
    # Subset temperatures to time > CU_date
    T_sub   <- T_base[time >= CU_date[i] & time <= CU_date[i] + years(1)] 
    
    # Subset times
    time_sub <- time[time >= CU_date[i] & time <= CU_date[i] + years(1)]
    
    # Set values < 0 to 0
    GDH     <- ifelse(T_sub < 0, 0, T_sub)
    
    # Accumulate chill units
    GDH      <- cumsum(abs(GDH))
    
    # Get datetime, when GDH demand (4836 h) is first met
    GDH_date[i] <- time_sub[min(which(GDH >= 4836))]
  }
  
  # Unite CU & GDH dates in list
  ret_list <- list(CU_date, GDH_date)
  
  # Return
  return(ret_list)
}


# Neumann's functions
neumann_growth <- function(temp, time, base = 4, bud_break){
  # Calculate T exceeding base
  T_base <- temp - base
  
  # Get bud break day
  bud_break <- as.Date(bud_break)
  
  # Create daily data
  days      <- as.Date(time)
  day_df    <- data.frame(cbind(as.Date(days), T_base))
  day_df    <- day_df %>% group_by(V1) %>% summarize(temp = mean(T_base, na.rm = T))
  
  # Subset to days after bud break
  day_df$V1 <- as.Date(day_df$V1)
  day_df    <- day_df[as.Date(day_df$V1) > bud_break & 
                        as.Date(day_df$V1) < bud_break + days(350),]
  
  # Reset T_base < 0 to 0 for cumulative T
  day_df$temp[day_df$temp < 0] <- 0 
  
  # Cumulate T
  day_df$T_cum  <- cumsum(day_df$temp)
  
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
  
    # leaf area increment
      # spurs: 500 DD; n = 264; 0.00004 m² * DD
      # long stems: 900 DD; n = 71; 0.00008 m² * DD
  day_df$LeafArea_m2    <- 
    ifelse(day_df$T_cum < 500, day_df$T_cum * 0.00004*264 + day_df$T_cum*0.00008*71,
                  ifelse(day_df$T_cum >= 500 & day_df$T_cum < 900, 
                         500 * 0.00004*264 + day_df$T_cum*0.00008*71, 
                         500 * 0.00004*264 + 900*0.00008*71))
  
  
  # Return data.frame
  return(day_df)
}


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

# Apply function
bud_breaks <- bud_break_seq(temp = temp, time = time)

# Create data.frame for plotting
df        <- data.frame(
  date = as.POSIXct(c(bud_breaks[[1]], bud_breaks[[2]])),
  param = rep(c("Dormancy Requirement", "Bud Burst"), each = length(bud_breaks[[1]]))
)

# add doy
df$doy <- as.numeric(format(df$date, format = "%j"))
df$doy <- ifelse(df$doy > 300, df$doy - 365, df$doy)

# Plot Bud Burst
ggplot(df, aes(x = date, y = doy)) +
  theme_bw() + geom_point() +
  facet_wrap(~param, ncol = 1, scales = "free_y") +
  labs(x = "", y = "Day of Year")

output_all <- c()

# Apply Neumann function
for(k in 1:length(df$date)){
  print(k)
  
  output <- neumann_growth(temp = temp, time = time, base = 4, bud_break = df$date[k])
  output$doy <- as.numeric(format(output$V1, format = "%j"))
  output$LeafArea_m2[output$doy < output$doy[which(output$LeafArea_m2 == 0)]] <- 0
  
  # Combine dfs
  output_all <- rbind(output_all, output)
}

# add year column
output_all$year <- format(output_all$V1, format = "%Y")


ggplot(output_all[output_all$LeafArea_m2 != max(output_all$LeafArea_m2, na.rm = T) &
                    output_all$LeafArea_m2 != 0,], aes(x = doy, y = LeafArea_m2)) +
  geom_point(alpha = 0.07) + theme_bw() +
  xlim(0,230) + labs(x = "Day of Year", y = "Leaf Area [m²]", color = "") +
  geom_point(data = output_all[output_all$fruit_development == T,], aes(color = "Fruit Development"), alpha = 0.3) +
  geom_point(data = output_all[output_all$full_bloom == T,], aes(color = "Full Bloom"), alpha = 0.3) +
  geom_point(data = output_all[output_all$fruit_abscission == T,], aes(color = "Fruit Abscission"), alpha = 0.3)
ggsave("LeafAreaGrowth.png", units = "cm", width = 15, height = 8, dpi = 300)
