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
obs   <- obs[obs$Referenzjahr > 1995 & (obs$Phase_id == 3 |
               obs$Phase_id == 5 | obs$Phase_id == 6),]

# Create budbreak datetime object
obs$bb_datetime <- 
  as.POSIXct(as.character(obs$Eintrittsdatum), format = "%Y%m%d")


# Connect to weather data folder
setwd("C:/Docs/MIRO/vegetation_model/dwd_csv")

files <- list.files()


for(k in 1:length(files)){  
  print(k)

  # Load weather data
  dat   <- read.csv(files[k])
  
   # Load coresponding phenology data
  Obs_sub    <- obs[obs$Stations_id == unique(dat$phen_ID),]
  
  # Manipulate datetime format
  dat$time <- as.POSIXct(dat$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  dat$year <- as.numeric(format(dat$time, format = "%Y"))
  dat$doy  <- as.numeric(format(dat$time, format = "%j"))
  
  # Create month column to assign "phenological year"
  dat$month<- as.numeric(format(dat$time, format = "%m"))
  
  # Assign "phenological year" info; i.e. divide timeline starting July
  dat$phen_year <- ifelse(dat$month >= 7, dat$year + 1, dat$year)
  dat           <- dat[!is.na(dat$phen_year) &
                         dat$phen_year %in% Obs_sub$Referenzjahr,]
  
  
  # Remove NAs
  dat   <- dat[!is.na(dat$air_temperature),-6]
  
  if(nrow(dat) > 0){
  # Apply Models
  dat <- dat %>%
    group_by(phen_year) %>%
    mutate(utah = Utah_Model(air_temperature),
           dyn = Dynamic_Model(air_temperature),
           GDH = GDH(air_temperature),
           doy_July = 1:length(air_temperature)/24) %>%
    ungroup


  # Extract starting day of chill accumulation according to Utah logic
  Chill_start <- dat[!is.na(dat$phen_year),] %>% 
    group_by(phen_year) %>% 
    summarize(start = as.POSIXct(paste0(unique(phen_year)-1,"-09-01"), format = "%Y-%m-%d"))
  
  
  # Corresponding bud break dates
  dat        <- dat[!is.na(dat$phen_ID),]
  
  
  
  #  Summarize chill period in one data.frame
  Chill_period    <- full_join(Chill_start, Obs_sub, by = c("phen_year" = "Referenzjahr"))
  Chill_period    <- Chill_period[!is.na(Chill_period$Stations_id),]
  
  
  # Extract Chill & Heat Accumulation for each period
  Chill_period$utah_acc <- NA
  Chill_period$dyn_acc  <- NA
  Chill_period$GDH      <- NA 
  
  
  for(i in 1:nrow(Chill_period)){
    ## UTAH MODEL
    # Subset to period
    sub <- dat[dat$time >= Chill_period$start[i] &
                      dat$time < Chill_period$bb_datetime[i],]
    
    # Calculate difference between min and max chill accumulation within period
    Chill_period$utah_acc[i]  <- max(sub$utah) - min(sub$utah)
    Chill_period$dyn_acc[i]   <- max(sub$dyn) - min(sub$dyn)
    Chill_period$GDH[i]       <- max(sub$GDH) - min(sub$GDH)

  }
  
    # Save Chill_Period Results
    write.csv(Chill_period[c(6,15,14,1:3,16:18)],
              paste0("../results/chill_acc/", files[k]),
              row.names = F)
  
  }
}

