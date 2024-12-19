## Author: Thomas Ohnemus
## Date: 19/12/2024
## Mean DOY as Modelling Approach for Spring Phenology

# Prepare Script
rm(list = ls())

library(tidyverse)
library(data.table)

# Functions
rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

# Load Data
setwd("C:/Docs/MIRO/vegetation_model")

# Phenology Observations
obs    <- read.csv("Phenology_Observations.csv")
obs    <- obs[(obs$Phase_id == 3 |
                obs$Phase_id == 5 |
                obs$Phase_id == 6) &
                obs$Referenzjahr > 1995,]

# Frequency table
freq    <- obs %>% group_by(SORTE, Phase_id) %>%
  summarize(n = n())

# Filter only cultivars with less than 60 observations (i.e. less than 2 observations per year)
freq    <- freq[freq$n <= 60,]
unindentified <- unique(freq$SORTE)

# Shift these to the "unidentified" category
obs$SORTE[obs$SORTE %in% unindentified] <- "unidentified"


## Subset in calibration & validation dataset



# Calculate mean DOY for each cultivar & phase
doy  <- obs %>% 
  group_by(SORTE, Phase_id) %>%
  summarize(mean_doy = mean(Jultag),
            sd = sd(Jultag))

# Calculate rmse per variety & phase
obs     <- left_join(obs, doy, by = c("SORTE", "Phase_id"))
rmse_df <- obs %>% group_by(SORTE, Phase_id) %>%
  summarize(rmse = rmse(Jultag, mean_doy))


