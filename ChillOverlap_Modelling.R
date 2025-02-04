## Author: Thomas Ohnemus
## Date: 04/02/2025
## Implementation Chill Overlap Model

# Prepare Script
rm(list = ls())

library(tidyverse)

setwd("C:/Docs/MIRO/vegetation_model")

# Set parameter starting values
## b1 and CR
b1_CR   <- read.csv("results/b1_estimate.csv")
b1_CR   <- b1_CR[b1_CR$SORTE != "unidentified",]

## b2
b2      <- read.csv("results/b2_estimate.csv")

## Overlap percentages to be tested
ol      <- seq(10,100,10)

# Correct illogical parameter estimates manually
## b2
b2$b2[b2$SORTE == "Berlepsch"] <- 23457
b2$b2[b2$SORTE == "Brettacher"] <- 22936
b2$b2[b2$SORTE == "Champagner Renette"] <- 19652
b2$b2[b2$SORTE == "Discovery" & b2$phase == "bloom start"] <- 19808
b2$b2[b2$SORTE == "Glockenapfel" & b2$phase == "fullbloom"] <- 24077
b2$b2[b2$SORTE == "Gloster"] <- 21089
b2$b2[b2$SORTE == "Golden Delicious"] <- 25112
b2$b2[b2$SORTE == "Idared" & b2$phase == "bloom start"] <- 22134
b2$b2[b2$SORTE == "Jonathan" & b2$phase == "fullbloom"] <- 20783
b2$b2[b2$SORTE == "Ontario" & b2$phase == "bloom start"] <- 22740
b2$b2[b2$SORTE == "Pinova"] <- 13971
b2$b2[b2$SORTE == "Piros" & b2$phase == "fullbloom"] <- 13599
b2$b2[b2$SORTE == "Starking" & b2$phase == "bloom start"] <- 18926
b2$b2[b2$SORTE == "Vista Bella"] <- 24833
b2$b2[b2$SORTE == "Weisser Klarapfel" & b2$phase == "fullbloom"] <- 27915
b2$b2[b2$SORTE == "Winterrambur"] <- 22707

## b1, Ontario only
b1_CR$b1_est[b1_CR$SORTE == "Ontario" & b1_CR$CR >= 76 &
               b1_CR$Phase == "Bloom start"] <- b1_CR$b1_est[b1_CR$SORTE == "Ontario" & 
                                                               b1_CR$CR >= 76 &
                                                               b1_CR$Phase == "Fullbloom"]


######
# Grab parameter set for specific cultivar
cultivar <- "Boskoop"  # later grab iteratively in loop


# Cultivar subsets
b1_CR_cult <- b1_CR[b1_CR$SORTE == cultivar,]
b2_cult    <- b2[b2$SORTE == cultivar,]

# Phase subset                   ##### CHANGE LATER #####
phase      <- "Bud Break"
b1_CR_sub  <- b1_CR_cult[b1_CR_cult$Phase == "Bud Break",]
b2_sub     <- b2_cult[b2_cult$phase == "bud break",]


# Grab first model parameters to test
## Chill Requirement
CR         <- unique(b1_CR_sub$CR)[1]

## Sequence of B1 values
b1_end     <- b1_CR_sub$b1_est[b1_CR_sub$CR == CR]

### Depending on the following conditions, b1 sequence changes
if(b1_end <= 100 & phase == "Bud Break"){
  b1_seq <- seq(0,b1_end,20)
} 
if(b1_end > 100 & phase == "Bud Break") {
  b1_seq <- c(seq(0,100,20), seq(100,b1_end, 50))
} 
if(phase != "Bud Break"){
  b1_seq <- seq(0,b1_end,50)
}

## Sequence of b2 values
### Starting value min 22500 for BB and min 23000 for rest
b2_start <- ifelse(phase == "Bud Break", 22500, 23000)

### Check if starting value of observations is higher
b2_start <- ifelse(b2_sub$b2 < 22500, b2_start, b2_sub$b2)

### End value for BB max 32,000, for others max 35,000
b2_end   <- ifelse(phase == "Bud Break", 32000, 35000)

### Sequence of b2 values to test
b2_seq   <- seq(b2_start, b2_end, 500)


