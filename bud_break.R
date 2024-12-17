## Author: Thomas Ohnemus
## Date: 25/10/2024
## Bud Break Model & Validation

# Prepare Script
rm(list = ls())

library(tidyverse)
library(data.table)

# Get Function
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
    #print(i)
    
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

# Load Data
setwd("C:/Docs/MIRO/vegetation_model")
  
  # Phenology Observations
obs    <- read.csv("Phenology_Observations.csv")
    # Subset to bud break
obs    <- obs[obs$Phase_id == 3,]

  # Weather Data
setwd("./data/bud_break")

files  <- list.files(pattern = ".csv")

# Empty DF to store data
bud_break_val <- c()

# Calculate bud break
for(i in 1:500){
  print(paste("----", i, "----"))
  
  # Get station_D
  statid  <- unique(obs$Stations_id)[i]
  
  # Subset to ith station 
  obs_sub <- obs[obs$Stations_id == statid,]
  
  # Grab associated weather data
  if(length(files[files %like% statid]) > 0){
    weather <- read.csv(files[files %like% statid])
    
    # Create timestamp column
    weather$MESS_DATUM <- as.POSIXct(weather$MESS_DATUM, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    
    # Calculate Bud Break
    bud_break_calc     <- bud_break_seq(temp = weather$TT_TU.Lufttemperatur, time = weather$MESS_DATUM)
    bud_break_calc     <- data.frame(
      bb_date = bud_break_calc[[2]],
      bb_year = as.numeric(format(bud_break_calc[[2]], format = "%Y")),
      bb_doy  = format(bud_break_calc[[2]], format = "%j"),
      statid  = statid
    )
      
    # Validate: Jultag = DOY?
    date  <- obs_sub$Eintrittsdatum
    date  <- as.POSIXct(as.character(date), format = "%Y%m%d")
    obs_sub$Jultag <- format(date, format = "%j")
    
    # Unite with observation dataframe
    bud_break_calc      <- left_join(bud_break_calc, obs_sub[,c(2,8,12)],
                                    by = c("bb_year" = "Referenzjahr"))
    
    
    # Store looped data
    bud_break_val      <- rbind(bud_break_val, bud_break_calc)
  }
}


# Scatterplot
ggplot(bud_break_val[!is.na(bud_break_val$Jultag),], 
       aes(x = as.numeric(Jultag), y = as.numeric(bb_doy), color = statid)) +
  geom_point(alpha = 0.5) + theme_bw() +
  geom_abline(slope = 1, color = "black", linetype = "dashed") +
  labs(x = "Observed DOY", y = "Modelled DOY", title = "Bud Break", color = "Site") + 
  facet_wrap(~bb_year)
ggsave("C:/Docs/MIRO/vegetation_model/bb_pred.png", width = 20, height = 20,
       units = "cm", dpi = 300)

# Calculate DOY per year
doy_year <- bud_break_val %>%
  group_by(bb_year) %>%
  summarize(doy = mean(as.numeric(Jultag), na.rm = T))

ggplot(doy_year[doy_year$doy != "NaN",], aes(x = bb_year, y = doy)) +
  geom_point() + theme_bw() + labs(x = "Year", y = "Mean DOY")

# Calculate DOY per variety
doy_variety <- bud_break_val %>%
  group_by(SORTE_ID, bb_year) %>%
  summarize(doy = mean(as.numeric(Jultag), na.rm = T),
            doy_sd = sd(as.numeric(Jultag), na.rm = T))

ggplot(doy_variety[doy_variety$doy != "NaN",], aes(x = bb_year, y = doy)) +
  geom_point() + theme_bw() + labs(x = "Year", y = "Mean DOY") +
  geom_errorbar(aes(ymin = doy - doy_sd, ymax = doy + doy_sd), color = "steelblue") +
  facet_wrap(~SORTE_ID)

# Calculate DOY per site
doy_site <- bud_break_val %>%
  group_by(SORTE_ID, bb_year, statid) %>%
  summarize(doy = mean(as.numeric(Jultag), na.rm = T),
            doy_sd = sd(as.numeric(Jultag), na.rm = T))

ggplot(doy_site[doy_site$doy != "NaN",], aes(x = bb_year, y = doy, color = as.factor(statid))) +
  geom_point() + theme_bw() + labs(x = "Variety", y = "Mean DOY") +
  geom_errorbar(aes(ymin = doy - doy_sd, ymax = doy + doy_sd), color = "steelblue") +
  facet_wrap( ~ SORTE_ID) + theme(legend.position = "NONE")





