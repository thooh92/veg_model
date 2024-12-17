## Author: Thomas Ohnemus
## Date: 23/10/2024
## Phenology database DWD

# Prepare Script
rm(list = ls())

library(tidyverse)
library(data.table)
library(readxl)

setwd("C:/Docs/MIRO/vegetation_model")

# Load Data
  # Station info
stats   <- read.table("PH_Beschreibung_Phaenologie_Stationen_Jahresmelder.txt", 
                      sep = ";", header = T)

  # Observations
obs     <- read.table("PH_Jahresmelder_Obst_Apfel_fruehe_Reife_1929_2023_hist.txt", 
                      sep = ";", header = T)

obs2    <- read.table("PH_Jahresmelder_Obst_Apfel_spaete_Reife_1925_2023_hist.txt", 
                      sep = ";", header = T)

obs     <- rbind(obs, obs2)
remove(obs2)  
  
  # Phase description; https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/annual_reporters/fruit/recent/PH_Beschreibung_Phasendefinition_Jahresmelder_Obst.txt
desc    <- data.frame(
  Objekt_id = c(rep(310,6), rep(311,6), rep(313,6)),
  Objekt = c(rep("Apfel",6), rep("Apfel, fruehe Reife",6), rep("Apfel, spaete Reife",6)),
  Phasen_id = rep(c(3,5,6,7,29,32),3),
  Phase = rep(c("Bud Break", "Bloom start", "Fullbloom", "Bloom end", "start maturity", "leaf fall"),3)
)


  # Variety information
#var     <- read.table("PH_Jahresmelder_Obst_Spezifizierung.txt", 
 #                     sep = ";", header = T)
#var     <- var[var$OBJEKT_ID %in% desc$Objekt_id,]

    # write as .csv
#write.csv(var, "Apple_variety.csv", row.names = F)

    # Get apple var infos
var       <- read.csv("Apple_variety.csv")

    # Get additional apple variety infos (from 2nd list, Jahresmelder_Obst_Notizen.txt)
var2      <- read_excel("Sorteninfo_Zusatz.xlsx")

    # Update variety information
for(i in 1:nrow(var2)){
  var$SORTE[which(var$STATIONS_ID == var2$STATIONS_ID[i] & var$REFERENZJAHR == var2$REFERENZJAHR[i] &
        var$OBJEKT_ID == var2$OBJEKT_ID[i])] <- var2$SORTE[i]
}
remove(var2)


# Assign phase information to observations
obs   <- left_join(obs, desc[,c(1,3:4)], 
                   by = c("Phase_id" = "Phasen_id", "Objekt_id"))

# Assign variety information
obs   <- left_join(obs, var[,c(1:3,5:6)],
                   by = c("Stations_id" = "STATIONS_ID",
                          "Referenzjahr" = "REFERENZJAHR",
                          "Objekt_id" = "OBJEKT_ID"))


# Update SORTE description
  # Produce "unidentified" category for NA, variety not in variety list and variety unknown
obs$SORTE[is.na(obs$SORTE) |
          obs$SORTE == unique(obs$SORTE)[2] |
          obs$SORTE == unique(obs$SORTE)[3]] <- "unidentified"

  # Remove blanks in other Categories
obs$SORTE[obs$SORTE == unique(obs$SORTE)[2]] <- "Weisser Klarapfel"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[3]] <- "Vista Bella"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[4]] <- "Stark Earliest"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[6]] <- "Discovery"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[7]] <- "Mantet"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[8]] <- "Piros"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[9]] <- "Helios"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[10]] <- "Lodi"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[12]] <- "Astramel"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[21]] <- "Boskoop"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[22]] <- "Jonathan"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[23]] <- "Jonagold"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[24]] <- "Ontario"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[25]] <- "Gloster"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[26]] <- "Berlepsch"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[27]] <- "Golden Delicious"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[28]] <- "Starking"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[29]] <- "Idared"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[30]] <- "Glockenapfel"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[31]] <- "Braeburn"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[33]] <- "Champagner Renette"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[34]] <- "Melrose"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[35]] <- "Winterrambur"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[37]] <- "Bohnapfel"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[40]] <- "Mutsu"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[41]] <- "Pinova"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[44]] <- "Brettacher"
obs$SORTE[obs$SORTE == unique(obs$SORTE)[53]] <- "Conference"


# write obs
write.csv(obs, "Phenology_Observations.csv", row.names = F)


# Subset to budbreak
bb    <- obs[obs$Phase_id == 3,]


### Create Validation Data for Bud Break Submodel
# Subset station info +
stats   <- stats[stats$Stations_id %in% bb$Stations_id,]

# Load data.frame
weather_dat <- read.csv("Station_urls.csv")

l_subs <- c()


# URLs can outdate for historical data; thus search URL using station number
library(RCurl)
ftp_url <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/historical/"

# List files in the FTP directory
files <- getURL(ftp_url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
file_list <- strsplit(files, "\r*\n")[[1]]


# Download data
for(j in 1:length(unique(weather_dat$phen_id))){
  print(j)
  
  # Subset to corresponding weather station IDs
  weather_sub <- weather_dat[weather_dat$phen_id == unique(weather_dat$phen_id)[j],]
  
  # Get elevation of phenology stations
  phen_ele    <- stats$Stationshoehe[stats$Stations_id == unique(weather_dat$phen_id)[j]]
  
  # Remove weather data with > 100 m elevation differences (if applicable)
  weather_sub <- weather_sub[abs(weather_sub$Stationshoehe - phen_ele) < 100,]
  
  # Get URL for data file with lowest distance
  inds    <- which(weather_sub$dist == min(weather_sub$dist))
  nearest <- weather_sub[inds,]
  
  
  # Download data of first station
    # Create correct URL
  statid  <- ifelse(nchar(nearest$Stations_id) == 4, paste0(0,nearest$Stations_id), 
                    ifelse(nchar(nearest$Stations_id) == 3, paste0(0,0,nearest$Stations_id), 
                           ifelse(nchar(nearest$Stations_id) == 2, paste0(0,0,0,nearest$Stations_id),
                           ifelse(nchar(nearest$Stations_id) == 1, paste0(0,0,0,0,nearest$Stations_id),nearest$Stations_id))))
  url     <- file_list[file_list %like% paste0("TU_",statid)]
  
    # Get data
  dat     <- dataDWD(paste0(ftp_url, url), varnames = T)
  
  # Assign relevant info to dat data.frame
  dat$phen_id <- nearest$phen_id
  dat$dist    <- nearest$dist
  
  # Are there any additional dates covered by stations with greater distance?
  subs <- weather_sub[which(weather_sub$von_datum < weather_sub$von_datum[inds] |
                  weather_sub$bis_datum > weather_sub$bis_datum[inds]),]
  
  
  # Loop through remaining indices
  while(nrow(subs) > 0){
    # Get next nearest weather station index
    ind  <- which(subs$dist == min(subs$dist))
    
    # Get next nearest data
    nearest <- subs[ind,]
    
    # Download data of first station
    # Create correct URL
    statid  <- ifelse(nchar(nearest$Stations_id) == 4, paste0(0,nearest$Stations_id), 
                      ifelse(nchar(nearest$Stations_id) == 3, paste0(0,0,nearest$Stations_id), 
                             ifelse(nchar(nearest$Stations_id) == 2, paste0(0,0,0,nearest$Stations_id),
                                    ifelse(nchar(nearest$Stations_id) == 1, paste0(0,0,0,0,nearest$Stations_id),nearest$Stations_id))))
    url     <- file_list[file_list %like% paste0("TU_",statid)]
    
    # Get data
    da     <- dataDWD(paste0(ftp_url, url), varnames = T)
    
    # Assign relevant info to dat data.frame
    da$phen_id <- nearest$phen_id
    da$dist    <- nearest$dist
    
    # Remove data with any already covered date
    da         <- da[!da$MESS_DATUM %in% dat$MESS_DATUM,]
    
    # Unite with preceding data.frame
    dat        <- rbind(dat, da)
    
    # Are there any additional dates covered by stations with greater distance?
    subs <- subs[which(subs$von_datum < subs$von_datum[ind] |
                         subs$bis_datum > subs$bis_datum[ind]),]
  }
  # Change Mess-Datum to character
  dat$MESS_DATUM <- format(dat$MESS_DATUM, format = "%Y-%m-%d %H:%M:%S")
  
  
  # Check for NAs
  dat_NA   <- dat[is.na(dat$TT_TU.Lufttemperatur),]
  
  # Save Data Frame with Phenology ID
  write.csv(dat, paste0("./data/bud_break/", unique(weather_dat$phen_id)[j],".csv"),
            row.names = F)
  
  # Store csv in minio
  
  
  # Delete local csv
  
}

