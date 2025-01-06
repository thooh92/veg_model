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
obs   <- obs[obs$Referenzjahr > 1995 & (obs$Phase_id == 3 |
                                          obs$Phase_id == 5 | obs$Phase_id == 6),]

# Create budbreak datetime object
obs$date <- 
  as.POSIXct(as.character(obs$Eintrittsdatum), format = "%Y%m%d")


# List Weather Data
setwd("C:/Docs/MIRO/vegetation_model/results/chill_acc")

files <- list.files()

# Read files
dat    <- data.frame()

for(i in 1:length(files)){
  data <- read.csv(files[i])
  
  dat  <- rbind(dat, data)
}

# Which data are flawed?
tst <- dat[dat$dyn_acc == "-Inf" & !is.na(dat$dyn_acc),]


# Min CP per variety
CP <- dat[dat$dyn_acc != "-Inf",] %>% group_by(SORTE) %>%
  summarize(CP_min = min(dyn_acc, na.rm = T),
            CP_mean = mean(dyn_acc),
            CP_max = max(dyn_acc),
            n = n())

CP <- CP[CP$n > 60 & !is.na(CP$SORTE),]

ggplot(CP, aes(x = n, y = CP_min)) +
  geom_point() + theme_bw() + 
  labs(x = "Number of Observations", y = "Minimum Chill Observed [CP]") 
ggsave("../../plots/n_vs_CP.png", units = "cm", dpi = 300,
       width = 10, height = 8)


# R² of logarithmic trend
CP$logn <- log10(CP$n)
summary(lm(logn~CP_min, CP))


# Get lower CP in Phase 5 and 6 than 3 years
sub <- dat[dat$dyn_acc %in% CP$CP_min,]


# Estimate b2 for each phenological stage
HA <- dat[dat$GDH != "-Inf",] %>% group_by(SORTE, Phase_id) %>%
  summarize(HA_min = min(GDH, na.rm = T),
            HA_mean = mean(GDH),
            HA_max = max(GDH),
            n = n())

# Remove pointless data
HA <- HA[HA$n > 60 & !is.na(HA$SORTE),]

# Assign phase names
HA$phase <- ifelse(HA$Phase_id == 3, "bud break",
                   ifelse(HA$Phase_id == 5, "bloom start", "fullbloom"))
HA$phase <- factor(HA$phase, levels = c("bud break", "bloom start", "fullbloom"))

HA$b2 <- HA$HA_max - HA$HA_min # highest HA - lowest HA per stage & cultivar

ggplot(HA[HA$SORTE != "unidentified",], aes(x = n, y = b2)) +
  geom_point() + theme_bw() + 
  labs(x = "Number of Observations", y = expression(beta[2] * " Estimate [GDH]")) +
  facet_wrap(~phase, ncol = 1)
ggsave("../../plots/n_vs_b2.png", units = "cm", dpi = 300,
       width = 10, height = 12)


# Initiate CP vector for cultivar
CPs <- seq(30, CP$CP_min[1], 1)
##########################









################################
# Variety infos (including min CP observed before bud break)








# Estimate b1 for each phenological stage
b1 <- tst # Calculate min HR found between day of CR fulfillment and phenological stage investigated




