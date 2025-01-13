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

CP <- CP[CP$n > 60*3 & !is.na(CP$SORTE),]

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

# Estimate b1 for each phenological stage
## Connect to weather data folder
setwd("C:/Docs/MIRO/vegetation_model/dwd_csv")

files <- list.files()

# Remove all cultivars with insufficient entries, e.g. Melrose
obs   <- obs[obs$SORTE %in% CP$SORTE,]


## Initiate loop
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

# Remove NAs & month column
dat   <- dat[!is.na(dat$air_temperature),-6]

# 2nd Subset: are there data of phenological year in the calendar year before the observation?
dat_check <- dat[dat$year %in% Obs_sub$Referenzjahr,]


if(nrow(dat) > 0){
  # Apply Models
  dat <- dat %>%
    group_by(phen_year) %>%
    mutate(utah = Utah_Model(air_temperature),
           dyn = Dynamic_Model(air_temperature),
           GDH = GDH(air_temperature),
           doy_July = 1:length(air_temperature)/24) %>%
    ungroup

  # Create df to store Heat Accumulation periods
  HA_periods <- data.frame()
  
  for(s in 1:length(unique(Obs_sub$SORTE))){
    # Get cultivar info
    SORTE <- unique(Obs_sub$SORTE)[s]
    
    # Get CR sequence
    CRs <- seq(30, CP$CP_min[CP$SORTE == SORTE], 1)
    
    # Subset data to cultivar
    Obs_sub_sub  <- Obs_sub[Obs_sub$SORTE == SORTE,]
    
  for(y in 1:length(unique(Obs_sub_sub$Referenzjahr))){
    # Extract year
    yr       <- unique(Obs_sub_sub$Referenzjahr)[y]
    
    # Subset data to year
    dat_sub  <- dat[dat$phen_year == yr,]
    
    if(nrow(dat_sub) > 8000 | nrow(dat_sub) == 0){
    
    # Get CP start value (1st of September)
    CP_start <- dat_sub$dyn[dat_sub$time == as.POSIXct(paste0(yr-1,"-09-01 00:00:00"))]
    
    # Loop through all CRs to investigate for cultivar
    for(z in 1:length(CRs)){ 
      # Get CP value where CR is fulfilled
      CP_end   <- CP_start + CRs[z]
      
      # Remove values < 0
      dat_sub_sub <- dat_sub[dat_sub$dyn > CP_end,]
      
      # Get time of that CR fulfillment
      CP_time  <- dat_sub_sub$time[which.min(dat_sub_sub$dyn)]
      
      # Subset to observations of reference year
      Obs_sub3 <- Obs_sub_sub[Obs_sub_sub$Referenzjahr == yr,]
      
      if(nrow(Obs_sub_sub) > 0){
      
      # Attach CP_time
      Obs_sub3$CP_time <- CP_time
      
      # Add CR information
      Obs_sub3$CR      <- CRs[z]
      
      # Calculate heat accumulation for these periods
      for(i in 1:nrow(Obs_sub3)){
        # Subset to period
        sub <- dat[dat$time >= Obs_sub3$CP_time[i] &
                     dat$time < Obs_sub3$date[i],]
        
        # Calculate GDH accumulation within period
        Obs_sub3$GDH[i]       <- max(sub$GDH) - min(sub$GDH)
        
      }
      
      # Store in HA_periods
      HA_periods <- rbind(HA_periods, Obs_sub3)
      }}}
  }}
  
  ## Save files
  write.csv(HA_periods, paste0("../results/b1/",files[k]), row.names = F)
}
}


# Approximate b1 summarized per cultivar
## List Data
setwd("../results/b1")
files <- list.files()

# Read files
dat    <- data.frame()

for(i in 1:length(files)){
  data <- read.csv(files[i])
  
  dat  <- rbind(dat, data)
}

## b1 estimate per cultivar and CR
b1 <- dat[dat$GDH != "-Inf",] %>% group_by(SORTE, Phase, CR) %>%
  summarize(b1_est = min(GDH))

### Create factor with correct levelling
b1$Phase <- factor(b1$Phase, levels = c("Bud Break", "Bloom start", "Fullbloom"))

### Plot
ggplot(b1[b1$SORTE != "unidentified",], aes(x = CR, y = b1_est, color = Phase)) +
  theme_bw() + facet_wrap(~SORTE) +
  geom_point(alpha = 0.3) +
  labs(x = "Chilling Requirement [CP]", y = expression(beta[1] * " Estimate [GDH]"))
ggsave("../../plots/b1_estimate.png", dpi = 300, units = "cm", width = 22, height = 18)


