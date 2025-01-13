## Author: Thomas Ohnemus
## Date: 12/12/2024
## ChillR Processing of Raw Data

rm(list = ls())

# Prepare Script
library(tidyverse)
library(chillR)
library(gridExtra)
library(minio.s3)

# Download processed data from minio
## Credentials
Sys.setenv("AWS_ACCESS_KEY_ID" = 'allaccess',
           "AWS_SECRET_ACCESS_KEY" = 'wDoXiBjUaNQBG2BbGusEbX7E2Zx8LrUQrovR4aLp',
           "AWS_DEFAULT_REGION" = 'test',
           "AWS_S3_ENDPOINT" = 'minio.ufz.de:443')


# List files in bucket/directory
objects <- 
  minio.s3::get_bucket(
    bucket = 'met-ohnemus-miro',
    prefix = "veg_model/dwd_csvs/",
    base_url = 'https://minio.ufz.de:443',
    use_https = TRUE
  )


while(length(objects) < 2300){
  # Get last key as marker
  last_key <- objects[length(objects)]$Contents$Key
  
  # Get subset of keys in minio
  sub <- 
    minio.s3::get_bucket(
    bucket = 'met-ohnemus-miro',
    prefix = "veg_model/dwd_csvs/",
    base_url = 'https://minio.ufz.de:443',
    marker = last_key,
    use_https = TRUE
    )

  # Unite with objects
  objects <- c(objects, sub)
  
}


# Iterate over the objects and download them
for (object in objects) {
  # Extract the object key (path in the bucket)
  file_key <- object$Key
  file_name <- basename(file_key)  # Extract the file name
  
  # Download each file to the local directory
  minio.s3::save_object(
    object = file_key,
    bucket = 'met-ohnemus-miro',
    file = file.path("C:/Docs/MIRO/vegetation_model/dwd_csv", file_name),
    use_https = TRUE
  )
}

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
  
  # Remove -Inf before saving
  Chill_period <- Chill_period[Chill_period$dyn_acc != "-Inf",]
  
    # Save Chill_Period Results
    write.csv(Chill_period[c(6,15,14,1:3,16:18)],
              paste0("../results/chill_acc/", files[k]),
              row.names = F)
  
  }
}

