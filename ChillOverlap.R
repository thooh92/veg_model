## Author: Thomas Ohnemus
## Date: 06/01/2025
## Chill Overlap Model

# Prepare Script
rm(list = ls())

library(tidyverse)
library(data.table)
library(chillR)

# Observations
obs   <- read.csv("C:/Docs/MIRO/vegetation_model/Phenology_Observations.csv")
obs   <- obs[obs$Referenzjahr > 1995,]

# Create budbreak datetime object
obs$date <- 
  as.POSIXct(as.character(obs$Eintrittsdatum), format = "%Y%m%d")


# List Weather Data
setwd("C:/Docs/MIRO/vegetation_model/dwd_csv")

files <- list.files()


# Variety infos (including min CP observed before bud break)


# Estimate b2 for each phenological stage
b2 <- tst # highest HA - lowest HA per stage & cultivar



# Initiate CP vector for cultivar
CPs <- seq(30, max(), 1)


# Estimate b1 for each phenological stage
b1 <- tst # Calculate min HR found between day of CR fulfillment and phenological stage investigated




