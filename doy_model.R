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
set.seed(123)

obs$group_key <- do.call(paste, c(obs[c("SORTE", "Phase_id")], sep = "_"))

split_results <- 
  lapply(split(obs, obs$group_key), function(group_data) {
  # Randomly shuffle rows
  shuffled_rows <- sample(nrow(group_data))
  split_index <- floor(nrow(group_data) * 0.75)
  
  # Assign rows to validation and calibration sets
  validation <- group_data[shuffled_rows[1:split_index], ]
  calibration <- group_data[shuffled_rows[(split_index + 1):nrow(group_data)], ]
  
  list(validation = validation, calibration = calibration)
})

# Combine validation and calibration results
validation <- do.call(rbind, lapply(split_results, function(x) x$validation))
calibration <- do.call(rbind, lapply(split_results, function(x) x$calibration))


# Calculate mean DOY for each cultivar & phase
doy  <- validation %>% 
  group_by(SORTE, Phase_id) %>%
  summarize(mean_doy = mean(Jultag),
            sd = sd(Jultag))

# Unite cal & val data
validation$type <- "validation"
calibration$type <- "calibration"

obs   <- rbind(validation, calibration)
obs   <- left_join(obs, doy, by = c("SORTE", "Phase_id"))

# Calculate rmse per variety & phase
rmse_df <- obs %>% group_by(SORTE, Phase, type) %>%
  summarize(rmse = rmse(Jultag, mean_doy))

# Calculate rmse per phase for all data
rmse_df2 <- obs %>% group_by(Phase, type) %>%
  summarize(rmse = rmse(Jultag, mean_doy))
rmse_df2$SORTE <- "All Data"

# Plotting rmse
rmse_df$Phase <- factor(rmse_df$Phase, 
                        levels = c("Bud Break", "Bloom start", "Fullbloom"))
rmse_df2$Phase <- factor(rmse_df2$Phase, 
                        levels = c("Bud Break", "Bloom start", "Fullbloom"))

ggplot(rmse_df[rmse_df$SORTE != "unidentified",], aes(x = SORTE, y = rmse, color = type)) +
  theme_bw() + geom_point() + facet_wrap(~Phase, ncol = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "RMSE", color = "Dataset") +
  geom_point(data = rmse_df2)
ggsave("./plots/RMSE_doy_model.png", units = "cm", dpi = 300,
       width = 20, height = 12)


# Save RMSE data frame
rmse_df_all <- rbind(rmse_df, rmse_df2)
write.csv(rmse_df_all, row.names = F, 
          "results/rmse_df_all.csv")
