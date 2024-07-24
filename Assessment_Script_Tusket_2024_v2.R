#...............................................................................
# This script is designed to run all of the functions needed to calculate
# escapement, run timing, and biological characteristics.
# Each site and will have to be assessed separately:
#...............................................................................

# Libraries
require(ROracle)

# Source necessary functions from the Alosa functions
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()

setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2024")

# Get the accessory data i.e. the proportion of fish that were ID'd as BBs
accessory_data <- read.csv("tusket-2024-vaughan-accessory-data.csv")

# Add rows for days we are missing (did not sample, missing data, weekend etc).
# The proportions of BBs on these days will be estimated using a GLM.
# I did this spacing weird so that it lined up.
extra_days <- data.frame(
  day    = c(25, 26, 1,  2,  8,  9),
  mon    = c(5,  5,  6,  6,  6,  6),
  all    = c(NA, NA, NA, NA, NA, NA),
  BB     = c(NA, NA, NA, NA, NA, NA),
  BBprop = c(NA, NA, NA, NA, NA, NA)
  )

species.split <- rbind(accessory_data, extra_days)
species.split<-species.split[order(species.split$mon,species.split$day),] #orders by date

# This is from Assessment_Script_Tusket_2023.R
# Here we use a GLM to fill in the missing days with BB proportions
species.split$day.int <- 1:nrow(species.split)
dat <- data.frame(x = species.split$day.int, y = species.split$BBprop)
dat <- dat[complete.cases(dat), ]
glmfit1 <- glm(BB ~ day.int, data = species.split, offset = log(all), family = "poisson")
out <- data.frame(day.int = 1:32, all = rep(1,32)) #all set to 1 to give proportions
out.predci <- predict(glmfit1, newdata = out, type = "link", se.fit = T) #list of 3
ginv <- glmfit1$family$linkinv
out.pred <- ginv(out.predci[[1]])
out.predlo <- ginv(out.predci[[1]] - 1.96 * out.predci[[2]])
out.predhi <- ginv(out.predci[[1]] + 1.96 * out.predci[[2]])
species.split1 <- species.split
species.split2 <- species.split
species.split3 <- species.split

for(i in 1:nrow(species.split)) {
  if (is.na(species.split$BBprop[i]) == T)
    
  {
    species.split1$BBprop[i] <- out.pred[i]
    species.split2$BBprop[i] <- out.predlo[i]
    species.split3$BBprop[i] <- out.predhi[i]
  }
}

# Estimate the escapement for Lake Vaughan with species split
daily.count <- twospecies.river.escapement(
  filename = "Vaughan 2024 count data.csv",
  fixtime = T,
  downstream.migration = T,
  database = F,
  year = 2024,
  site = 2,
  channel = channel,
  species.split = species.split1
  )

va <- daily.count[[1]] # estimates for alewives at VD
vb <- daily.count[[2]] # estimates for BBs at VD

va$dayofyear <- as.integer(va$dayofyear)
vb$dayofyear <- as.integer(vb$dayofyear)

# For Powerhouse
# ph_esc <- onespecies.river.escapement(
#   "Powerhouse 2024 count data.csv",
#   fixtime = T,
#   downstream.migration = F,
#   database = F,
#   2024,
#   2,
#   channel
# )
# 
# ph_esc <- round(ph_esc)
# write.csv(ph_esc, file = "ph_in_season_summary.csv", row.names = F)
