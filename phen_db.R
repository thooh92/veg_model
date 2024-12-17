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



# Remove data based on Quality Flags
obs <- obs[obs$Qualitaetsniveau == 10,]
obs <- obs[obs$Eintrittsdatum_QB < 5,]

1-593880/597569 # Removed data, 0.62%


# write obs
write.csv(obs, "Phenology_Observations.csv", row.names = F)
