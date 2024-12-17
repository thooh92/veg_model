## Author: Thomas Ohnemus
## Date: 12/12/2024
## Testing ChillR package

rm(list = ls())

# Prepare Script
library(tidyverse)
library(chillR)
library(gridExtra)


# Load Phenology observations
obs   <- read.csv("C:/Docs/MIRO/vegetation_model/Phenology_Observations.csv")
obs   <- obs[obs$Phase_id == 3 & obs$Referenzjahr > 1995,]

# Create budbreak datetime object
obs$bb_datetime <- 
  as.POSIXct(as.character(obs$Eintrittsdatum), format = "%Y%m%d")


# Connect to weather data folder
setwd("C:/Docs/MIRO/vegetation_model/dwd_csv")

files <- list.files()

for(k in 1:254){
  print(k)
  
  
  # Load exemplary data
  dat   <- read.csv(files[k])
  
  
  # Manipulate datetime format
  dat$time <- as.POSIXct(dat$time, format = "%Y-%m-%d %H:%M:%S")
  dat$year <- as.numeric(format(dat$time, format = "%Y"))
  dat$doy  <- as.numeric(format(dat$time, format = "%j"))
  
  
  
  # Assign "phenological year" info; i.e. divide timeline starting July
  dat$phen_year <- ifelse(dat$doy > 182, dat$year + 1, dat$year)
  dat           <- dat[dat$phen_year != 1995,]
  
  # Remove NAs
  dat   <- dat[!is.na(dat$air_temperature),]
  
  if(nrow(dat) > 0){
  # Apply Models
  dat <- dat %>%
    group_by(phen_year) %>%
    mutate(utah = Utah_Model(air_temperature),
           dyn = Dynamic_Model(air_temperature),
           doy_July = 1:length(air_temperature)/24) %>%
    ungroup
  
  # Plot
  grid.arrange(
    ggplot(dat[!is.na(dat$year),], aes(x = doy_July, y = utah, 
                                     color = as.factor(phen_year))) +
    geom_line() + theme_bw() +
    labs(x = "Day after July 1st", y = "Accumulated Utah Chill Units",
         color = "Phenological Year"),
    ggplot(dat[!is.na(dat$year),], aes(x = doy_July, y = dyn, 
                                       color = as.factor(phen_year))) +
      geom_line() + theme_bw() +
      labs(x = "Day after July 1st", y = "Accumulated Chill Portions",
           color = "Phenological Year"))

  # Extract starting day of chill accumulation according to Utah logic
  Chill_start <- dat[!is.na(dat$phen_year),] %>% 
    group_by(phen_year) %>% 
    summarize(utah_time = time[which.min(utah)],
              dyn_time = time[which.min(dyn)])
  
  
  # Corresponding bud break dates
  dat        <- dat[!is.na(dat$phen_ID),]
  Obs_sub    <- obs[obs$Stations_id == unique(dat$phen_ID),]
  
  
  #  Summarize chill period in one data.frame
  Chill_period    <- full_join(Chill_start, Obs_sub, by = c("phen_year" = "Referenzjahr"))
  Chill_period    <- Chill_period[!is.na(Chill_period$Stations_id),]
  
  
  # Extract Chill Hour Accumulation for each period
  Chill_period$utah_acc <- NA
  Chill_period$dyn_acc  <- NA
  
  for(i in 1:nrow(Chill_period)){
    ## UTAH MODEL
    # Subset to period
    utah_sub <- dat[dat$time >= Chill_period$utah_time[i] &
                      dat$time < Chill_period$bb_datetime[i],]
    
    # Calculate difference between min and max chill accumulation within period
    Chill_period$utah_acc[i] <- max(utah_sub$utah) - min(utah_sub$utah)
    
    ## DYNAMIC MODEL
    # Subset to period
    utah_sub <- dat[dat$time >= Chill_period$dyn_time[i] &
                      dat$time < Chill_period$bb_datetime[i],]
    
    # Calculate difference between min and max chill accumulation within period
    Chill_period$dyn_acc[i] <- max(utah_sub$dyn) - min(utah_sub$dyn)
    
    
    
    # Save Chill_Period Results
    write.csv(Chill_period[c(4,15,14,1:3,16:18)],
              paste0("../results/chill_acc/", files[k]),
              row.names = F)
  }}
}




####### COMPARE NAMES
setwd("../results/chill_acc")
files2 <- list.files()


missing <- files[!files %in% files2]



################################
?PLS_pheno






