## Author: Thomas Ohnemus
## Date: 19/12/2024
## Mean DOY as Modelling Approach for Spring Phenology

# Prepare Script
rm(list = ls())

library(tidyverse)
library(data.table)

# Load Data
setwd("C:/Docs/MIRO/vegetation_model")

# Phenology Observations
obs    <- read.csv("Phenology_Observations.csv")

# Frequency table
freq    <- data.frame(table(obs$SORTE))

# Filter only cultivars with less than 60 observations (i.e. less than 2 observations per year)
freq    <- freq[freq$Freq <= 60,]

# Shift these to the "unidentified" category
obs$SORTE[obs$SORTE %in% freq$Var1] <- "unidentified"

# Cultivar-specific analyses
# 1. Variation Bud Break Date within Cultivar
# Calculate mean DOY for each cultivar
doy  <- obs %>% 
  group_by(SORTE) %>%
  summarize(mean_doy = mean(Jultag),
            sd = sd(Jultag))
