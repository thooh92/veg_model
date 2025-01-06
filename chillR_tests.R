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


for(k in 1:length(files)){  # 1:839 all files before 10,000 IDs; so remove 839 files at end when processing other data
  print(k)

  # Load weather data
  dat   <- read.csv(files[k])
  
   # Load coresponding phenology data
  Obs_sub    <- obs[obs$Stations_id == unique(dat$phen_ID),]
  
  # Manipulate datetime format
  dat$time <- as.POSIXct(dat$time, format = "%Y-%m-%d %H:%M:%S")
  dat$year <- as.numeric(format(dat$time, format = "%Y"))
  dat$doy  <- as.numeric(format(dat$time, format = "%j"))
  
  
  
  # Assign "phenological year" info; i.e. divide timeline starting July
  dat$phen_year <- ifelse(dat$doy > 182, dat$year + 1, dat$year)
  dat           <- dat[!is.na(dat$phen_year) &
                         dat$phen_year %in% Obs_sub$Referenzjahr,]
  
  
  # Remove NAs
  dat   <- dat[!is.na(dat$air_temperature),]
  
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




####### COMPARE NAMES
setwd("../results/chill_acc")
files2 <- list.files()


missing <- files[!files %in% files2]
missing <- as.numeric(substr(missing, 0, nchar(missing)-4))

# Sites missing cause of NA data
NA_v <- c(7503, 7828, 8499, 10802, 10971, 11055, 11089, 11292, 12294, 
12522, 12739, 12773, 12881, 12989, 14673, 19499, 19549)


# Load Station Data
stats   <- read.table("C:/Docs/MIRO/vegetation_model/PH_Beschreibung_Phaenologie_Stationen_Jahresmelder.txt", 
                      sep = ";", header = T)

stats_sub <- stats[stats$Stations_id %in% NA_v,]


# Map Location of Missing sites
stats_shp <- vect(stats_sub, geom = c("geograph.Laenge", "geograph.Breite"),
                  crs = "EPSG:4326")
plot(stats_shp)

## Check
# NA: 



#### Probably shift to new script
# Read Files for Meta Analysis
dat    <- data.frame()

for(i in 1:length(files2)){
  data <- read.csv(files2[i])
  
  dat  <- rbind(dat, data)
}


ggplot(dat, aes(x = SORTE, y = dyn_acc)) +
  geom_boxplot() + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Min CP per variety
CP <- dat[dat$dyn_acc != "-Inf",] %>% group_by(SORTE) %>%
  summarize(CP_min = min(dyn_acc, na.rm = T),
            CP_mean = mean(dyn_acc),
            CP_max = max(dyn_acc),
            n = n())

CP <- CP[CP$n > 60,]
CP$range <- CP$CP_max - CP$CP_min

ggplot(CP, aes(x = n, y = range)) +
  geom_point() + theme_bw() +
  xlim(0,2500)
################################







