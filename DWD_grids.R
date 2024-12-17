## Date: 02/12/2024
## Author: Thomas Ohnemus
## Hourly DWD Temperature Grids

# Prepare Script
rm(list = ls())

library(terra)
library(ncdf4)
library(R.utils)
library(RCurl)
library(tidyverse)

# Phenology Station Data
stats   <- read.table("C:/Docs/MIRO/vegetation_model/PH_Beschreibung_Phaenologie_Stationen_Jahresmelder.txt", 
                      sep = ";", header = T)

# Phenology Observations
obs <- read.csv("C:/Docs/MIRO/vegetation_model/Phenology_Observations.csv")
obs <- obs[obs$Phase_id == 3,]


# Plot Histograms
ggplot(obs, aes(x = SORTE)) + geom_histogram(stat = "count") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~Referenzjahr, ncol = 1)

# Subset to stations running in 1996 or more recent
stats$Datum.Stationsaufloesung <- 
  as.POSIXct(stats$Datum.Stationsaufloesung, format = "%d.%m.%Y")
stats$Datum.Stationsaufloesung[is.na(stats$Datum.Stationsaufloesung)] <- as.POSIXct("2024-12-31")
stats  <- stats[stats$Datum.Stationsaufloesung > as.POSIXct("1996-12-31"),]

# Phenology Station Shapefile
stats_shp <- vect(stats, geom = c("geograph.Laenge","geograph.Breite"), crs = "EPSG:4326")
plot(stats_shp)

# Download temperature grids
ftp_url <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/hostrada/air_temperature_mean/"

# List files in the FTP directory
files <- getURL(ftp_url)

# Parse the links for files ending with .gz (or desired extension)
file_links <- regmatches(files, gregexpr("href=\"[^\"]*\\.nc\"", files))[[1]]
file_links <- gsub("href=\"|\"", "", file_links) # Clean up the extracted links

# Create full URLs for the files
file_urls <- paste0(ftp_url, file_links)


# Download
for(url in file_urls){
  destfile <- file.path("C:/Docs/MIRO/vegetation_model/grids", basename(url))
  binary_data <- getBinaryURL(url)
  writeBin(binary_data, destfile)
  
  # Load Raster
  ra <- rast(destfile)
  
  # year
  year      <- as.numeric(substr(destfile, nchar(destfile)-12, nchar(destfile)-9))
  
  # IDs with data measured in respective year
  phen_stat <- unique(obs$Stations_id[obs$Referenzjahr == year])
  
  # Subset Shapefile
  shp_sub <- stats_shp[stats_shp$Stations_id %in% phen_stat,]
  
  # Transform shapefile
  shp_sub <- project(shp_sub, crs(ra))
  plot(ra[[1]]);plot(shp_sub, add = T)
  
  # Extract data for Phenology Locations
  ext_df <- terra::extract(ra, stats_shp)
  
  
  # Delete grid
  
  
  
}

# Load Data
setwd("C:/Docs/MIRO/vegetation_model/grids")

# Load nc
files <- list.files()

dat <- rast(files[1])

dim(dat)
plot(dat[[1]])
names(dat[[5]])
