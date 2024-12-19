## Author: Thomas Ohnemus
## Date: 10/12/2024
## Metadata Analysis of Bud Break Observations since 1995

# Prepare Script
rm(list = ls())

library(tidyverse)
library(terra)
library(sf)
library(Kendall)
library(gridExtra)

# Load Data
setwd("C:/Docs/MIRO/vegetation_model")

# Phenology Observations
obs    <- read.csv("Phenology_Observations.csv")

# Subset to phenology data since 1996
obs    <- obs[(obs$Phase_id == 3 |
                 obs$Phase_id == 5 |
                 obs$Phase_id == 6) &
                obs$Referenzjahr > 1995,]


# Load Station Data
stats   <- read.table("PH_Beschreibung_Phaenologie_Stationen_Jahresmelder.txt", 
                      sep = ";", header = T)

# Join data frames
obs    <- left_join(obs, stats, "Stations_id")

# Factorize Phase
obs$Phase <- factor(obs$Phase, 
                    levels = c("Bud Break", "Bloom start", "Fullbloom"))

# Produce Shapefile for Density Map
obs_shp <- vect(obs, geom = c("geograph.Laenge", "geograph.Breite"),
                crs = "EPSG:4326")


# DE Shapefile
DE      <- vect("DE.shp/DE.shp")
DE      <- st_as_sf(DE)

# Density Map
obs_sf  <- st_as_sf(obs_shp)
points_df <- as.data.frame(st_coordinates(obs_sf))


ggplot() +
  geom_hex(data = points_df, aes(X, Y), binwidth = c(0.5,0.5)) +
  geom_sf(data = DE, fill = NA, color = "black") +  # Add Germany boundary
  scale_fill_viridis_c() +  # Use a nice color scale
  coord_sf() +
  labs(x = "", y = "", fill = "Observation\ncount") +
  theme_bw() +
  geom_sf(data = obs_sf, color = "black", alpha = 0.001) + 
  facet_wrap(~Phase)
ggsave("./plots/DensityMap_All.png", units = "cm", dpi = 300,
       width = 25, height = 15)

# Histogram of observations since 1995
ggplot() +
  geom_histogram(data = obs, aes(x = Referenzjahr), binwidth = 1) +
  theme_bw() + labs(y = "Count of Observations", x = "Year") +
  facet_wrap(~Phase, ncol = 1)
ggsave("./plots/Histogram_All.png", units = "cm", dpi = 300,
       width = 15, height = 20)

# Frequency table
freq    <- obs %>% group_by(SORTE, Phase_id) %>%
  summarize(n = n())

# Filter only cultivars with less than 60 observations (i.e. less than 2 observations per year)
freq    <- freq[freq$n <= 60,]
unindentified <- unique(freq$SORTE)

# Shift these to the "unidentified" category
obs$SORTE[obs$SORTE %in% unindentified] <- "unidentified"


# Cultivar-specific analyses
# 1. Variation Bud Break Date within Cultivar
  # Calculate mean DOY for each cultivar
doy  <- obs %>% 
  group_by(SORTE, Phase) %>%
  summarize(mean_doy = mean(Jultag),
            sd = sd(Jultag))

ggplot() +
  geom_point(data = obs, aes(x = SORTE, y = Jultag, color = geograph.Breite), alpha = 0.2) + 
  theme_bw() +
  labs(x = "Cultivar", y = "DOY", color = "Latitude [°]") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_point(data = doy, aes(x = SORTE, y = mean_doy), 
             shape = "diamond", color = "coral", size = 2.5) +
  facet_wrap(~Phase, ncol = 1)
ggsave("./plots/Variation_withinVarieties.png", units = "cm", dpi = 300,
       width = 20, height = 15)


# 2. Variation around bud break date for cultivars
  # Idea: calculate mean for each plot & cultivar; analyze "anomaly" around mean over 29 year period
# Mean for Plot & Cultivar
doy  <- obs %>% 
  group_by(SORTE, Stations_id, Phase) %>%
  summarize(mean_doy = mean(Jultag),
            sd = sd(Jultag),
            n = n())

# Subset to plots with at least 10 observations
doy <- doy[doy$n >= 10,]
doy_sub <- doy[doy$SORTE == "Starking",]

# Assign Data back to obs df
obs <- left_join(obs, doy, by = c("Stations_id", "SORTE", "Phase"))

# Calculate anomaly around mean
obs$anomaly <- obs$Jultag - obs$mean_doy

# Plotting anomaly
ggplot(obs[obs$SORTE != "unidentified" & obs$anomaly <= 100 & !is.na(obs$anomaly),], 
       aes(x = Referenzjahr, y = anomaly)) +
  geom_point(alpha = 0.15) + theme_bw() +
  labs(x = "Year", y = "DOY anomaly [d]", title = "n >= 10") +
  facet_grid(SORTE~Phase, scales = "free_y") +
  geom_smooth(method = "lm", se = F)
ggsave("./plots/Anomaly_around_meanDOY.png", units = "cm", dpi = 300,
       width = 25, height = 40)


# Extract slope of model & significance of trend
statistics <- obs[obs$SORTE != "unidentified" & obs$anomaly <= 100 & !is.na(obs$anomaly),] %>%
  group_by(SORTE, Phase, Stations_id) %>%
  summarize(slope = lm(anomaly ~ Referenzjahr)$coefficients[2],
            sig = MannKendall(anomaly)[["sl"]])

statistics$sig <- ifelse(statistics$sig >= 0.05, F, T)

ggplot(statistics, aes(x = SORTE, y = slope, color = sig)) +
  #geom_boxplot() + 
  geom_point(alpha = 0.5) + theme_bw() + geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Phase, ncol = 1) + labs(x = "", y = "Slope [d/a]", color = "Significant\nTrend")
ggsave("./plots/Site_slopes.png", units = "cm", dpi = 300,
       width = 15, height = 20)


# Mapping Slopes




  # Cultivar specific density maps
points_df$SORTE <- obs$SORTE


  # Produce new obs_sf
obs_sf_new <- vect(points_df, geom = c("X", "Y"), crs = "EPSG:4326")
obs_sf_new <- st_as_sf(obs_sf_new)

ggplot() +
  geom_sf(data = obs_sf_new[obs_sf_new$SORTE != "unidentified",], 
          color = "black", alpha = 0.2) +
  facet_wrap(~SORTE) + coord_sf() + theme_bw() +
  geom_sf(data = DE, fill = NA, color = "black")




# Plot density maps for specific cultivars
for(i in 1:length(unique(points_df$SORTE))){
  # Loop through cultivars
  brettacher <- points_df[points_df$SORTE == unique(points_df$SORTE)[i],]
  brettacher_sf <- st_as_sf(vect(brettacher, geom = c("X", "Y"), crs = "EPSG:4326"))
  
  ggplot() +
   # stat_density_2d(data = brettacher, aes(X, Y, fill = ..level..), geom = "polygon", 
     #               n = 10, adjust = 5) +
    geom_hex(data = brettacher, aes(X, Y), binwidth = c(0.5,0.5)) +
    #geom_bin2d(data = brettacher, aes(X, Y)) +
    geom_sf(data = brettacher_sf, color = "black", fill = "white", alpha = 0.4, shape = 21) +
    geom_sf(data = DE, fill = NA, color = "black") +  # Add Germany boundary
    scale_fill_viridis_c() +
    coord_sf() +
    labs(title = "",
         fill = "Count",
         x = "Longitude",
         y = "Latitude") +
    theme_bw()
  ggsave(paste0("./plots/DensityMap_",unique(points_df$SORTE)[i],".png"), units = "cm", dpi = 300,
         width = 12, height = 15)
}


# Annual Slope of Temperature (produced on MSG in DWD_trend_analysis.R)
## Load Data
slope <- rast("./results/slope_1995_2024.tif")
slope <- slope*365.25 # Transform daily slope to annual slope
slope <- project(slope, "EPSG:4326")
plot(slope)

## Plot Raster
r_df <- as.data.frame(slope, xy = TRUE) 

ggplot(data = r_df) +
  geom_raster(aes(x = x, y = y, fill = lyr.1)) +
  scale_fill_viridis_c() +  # Optional: Viridis color scale
  labs(title = "", x = "Lon [°]", y = "Lat [°]", fill = "Temperature Change\n07/1995 - 06/2024\n[°C/a]") +
  theme_bw()
ggsave("./plots/Slope_Map.png", dpi = 300, units = "cm",
       width = 15, height = 15)


## Monthly Slopes
setwd("./results")
files <- list.files(pattern = ".tif")
files <- files[!files %like% "1995_2024"]

slope <- rast(files[files %like% "slope"])
slope <- slope*30 # Transform daily slope to "annual" slope
slope <- project(slope, "EPSG:4326")

names(slope) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                  "Aug", "Sep", "Oct", "Nov", "Dec")

## Plot Raster
r_df <- as.data.frame(slope, xy = TRUE) 
r_df <- pivot_longer(r_df, cols = 3:14,
                     names_to = "month", 
                     values_to = "slope")
r_df$month <- factor(r_df$month, levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"))

ggplot(data = r_df) +
  geom_raster(aes(x = x, y = y, fill = slope)) +
  scale_fill_viridis_c() +  # Optional: Viridis color scale
  labs(title = "", x = "Lon [°]", y = "Lat [°]", fill = "Temperature Change\n07/1995 - 06/2024\n[°C/a]") +
  theme_bw() + facet_wrap(~month)
ggsave("../plots/Slope_Map_monthly.png", dpi = 300, units = "cm",
       width = 30, height = 20)


## Monthly Mann Kendall Test Results
MK <- rast(files[files %like% "MK"])
MK <- project(MK, "EPSG:4326")

names(MK) <-  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                "Aug", "Sep", "Oct", "Nov", "Dec")

## Plot Raster
MK_df <- as.data.frame(MK, xy = TRUE) 
MK_df <- pivot_longer(MK_df, cols = 3:14,
                     names_to = "month", 
                     values_to = "MK")
MK_df$month <- factor(MK_df$month, levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"))

ggplot(data = MK_df) +
  geom_raster(aes(x = x, y = y, fill = MK)) +
  scale_fill_viridis_c() +  # Optional: Viridis color scale
  labs(title = "", x = "Lon [°]", y = "Lat [°]", fill = "Mann-Kendall\np-value") +
  theme_bw() + facet_wrap(~month)
ggsave("../plots/MK_Map_monthly.png", dpi = 300, units = "cm",
       width = 30, height = 20)

# Unite to one raster
data <- cbind(r_df, MK_df[,4])

# Create a significance mask
data$sig  <- data$MK > 0.05  # Adjust threshold as needed
insig_sub <- data[data$sig == T,]


# Plot slopes with overlay for insignificant cells
ggplot(data, aes(x = x, y = y)) +
  geom_raster(aes(fill = slope)) +
  scale_fill_viridis_c()  +
  coord_equal() +
  theme_bw() + facet_wrap(~month) +  
  labs(title = "", x = "Lon [°]", y = "Lat [°]", 
       fill = "Temperature Change\n07/1995 - 06/2024\n[°C/a]") +
  geom_tile(data = insig_sub,
              aes(x = x, y = y), color = "grey60", alpha = 0.05)
ggsave("../plots/Slope_Map_monthly_withMK.png", dpi = 300, units = "cm",
       width = 30, height = 20)

  # focus: Boskoop, Weisser Klarapfel, Berlepsch, Golden Delicious, Idared, Jonagold, Ontario