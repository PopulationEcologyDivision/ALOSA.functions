# Description: MARFIS Cleaner script
#
# Takes the catch data frame, and manually corrects all errors identified 
# by checking the logbooks.
#
# All errors are wrapped in an if statement, that checks if the pre-existing
# issue is still there. If it has been changed in the data base, the correction
# will not be applied and a warning will be printed so that the correction can 
# be deleted.
#
# Inputs: catch data frame from pulling from the MARFIS database
#
# This function is used by: MARFIS_all in one.R

MARFIS.error.cleaner <- function(catch) {
# LOAD DATA ####################################################################
library(tidyverse)

#load("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/marfis-pull-2024-03-21.Rdata")

# This factor is for converting a count of fish to a mass in kilograms
conv <- 0.240

# 2019 #########################################################################

#120037
catch[catch$LICENCE_ID==120037 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120037 & catch$YEAR==2019, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120037 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120039
catch[catch$LICENCE_ID==120039 & catch$YEAR==2019, "FV_HOURS_FISHED"] <- NA

#120091
catch[catch$LICENCE_ID==120091 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-04-16", "FV_WEIGHT"] <- 816
catch[catch$LICENCE_ID==120091 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-04-18", "FV_WEIGHT"] <- 847

#120100
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-02", "FV_WEIGHT"] <- 150
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-05", "FV_WEIGHT"] <- 250
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-06", "FV_WEIGHT"] <- 900
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-07", "FV_WEIGHT"] <- 350
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-09", "FV_WEIGHT"] <- 300
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-30", "FV_WEIGHT"] <- 250
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-04", "FV_WEIGHT"] <- 165
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-05", "FV_WEIGHT"] <- 150
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-09", "FV_WEIGHT"] <- 324
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-10", "FV_WEIGHT"] <- 800
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-12", "FV_WEIGHT"] <- 66
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-13", "FV_WEIGHT"] <- 50
catch[catch$LICENCE_ID==120100 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120100 & catch$YEAR==2019, "FV_WEIGHT"] * conv

#120046
catch[catch$LICENCE_ID==120046 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120046 & catch$YEAR==2019, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120046 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120050
catch[catch$LICENCE_ID==120050 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120050 & catch$YEAR==2019 & catch$MONTH=="04" & catch$DAY=="27", "DAY"] <- "22"
catch[catch$LICENCE_ID==120050 & catch$YEAR==2019 & catch$MONTH=="04" & catch$DAY=="22", "FV_DATE_FISHED"] <- "2019-04-22"

#120065
catch[catch$LICENCE_ID==120065 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "POUNDS"

#120073
catch[catch$LICENCE_ID==120073 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120085
catch[catch$LICENCE_ID==120085 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120085 & catch$YEAR==2019, "FV_WEIGHT"] * conv

#120088 - how to remove a single row with character criteria
catch <- catch[!(catch$LICENCE_ID==120088 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-04-15" & catch$MEASUREMENT_UNIT=="POUNDS"), ]

#120089
catch[catch$LICENCE_ID==120089 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120103
catch[catch$LICENCE_ID==120103 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="16", "FV_WEIGHT"] <- 108

#120108
catch[catch$LICENCE_ID==120108 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120108 & catch$YEAR==2019, "FV_WEIGHT"] * conv

#120118
catch[catch$LICENCE_ID==120118 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120118 & catch$YEAR==2019, "FV_WEIGHT"] * conv

#120121
catch[catch$LICENCE_ID==120121 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120121 & catch$YEAR==2019, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120121 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="02", "FV_WEIGHT"] <- 650 * conv
catch[catch$LICENCE_ID==120121 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="22", "FV_WEIGHT"] <- 180 * conv

#120124
catch[catch$LICENCE_ID==120124 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-13", "FV_WEIGHT" ] <- 2750
catch[catch$LICENCE_ID==120124 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="06", "FV_HOURS_FISHED" ] <- 5
catch[catch$LICENCE_ID==120124 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-23" & catch$FV_WEIGHT==1500, "FV_DATE_FISHED" ] <- "2019-05-24"
catch[catch$LICENCE_ID==120124 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-24", "DAY"] <- "24"

#120135
catch[catch$LICENCE_ID==120135 & catch$YEAR==2019, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120135 & catch$YEAR==2019, "GEAR_DESCRIPTION"] <- "DIP STAND"
catch[catch$LICENCE_ID==120135 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-13", "FV_WEIGHT" ] <- 2750

#120139
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="14", "DAY"] <- "15"
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="15", "FV_DATE_FISHED"] <- "2019-05-15"
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$MONTH=="06" & catch$DAY=="01", "DAY"] <- "05"
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$MONTH=="06" & catch$DAY=="05", "FV_DATE_FISHED"] <- "2019-06-05"
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-12", "FV_WEIGHT"] <- 25
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-15", "FV_WEIGHT"] <- 63
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-19", "FV_WEIGHT"] <- 47
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-20", "FV_WEIGHT"] <- 50
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-21", "FV_WEIGHT"] <- 68
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-22", "FV_WEIGHT"] <- 57
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-23", "FV_WEIGHT"] <- 82
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-26", "FV_WEIGHT"] <- 79
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-27", "FV_WEIGHT"] <- 62
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-05", "FV_WEIGHT"] <- 74
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-06", "FV_WEIGHT"] <- 120
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-09", "FV_WEIGHT"] <- 82
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-12", "FV_WEIGHT"] <- 77
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-18", "FV_WEIGHT"] <- 120
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-20", "FV_WEIGHT"] <- 35
catch[catch$LICENCE_ID==120139 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120139 & catch$YEAR==2019, "FV_WEIGHT"] * conv

#120144
catch[catch$LICENCE_ID==120144 & catch$YEAR==2019 & catch$MONTH=="01", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120144 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="29", "FV_DATE_FISHED"] <- "2019-05-29"
catch[catch$LICENCE_ID==120144 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="30", "FV_DATE_FISHED"] <- "2019-05-30"

#120146
catch[catch$LICENCE_ID==120146 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="23", "FV_DATE_FISHED"] <- "2019-05-24"
catch[catch$LICENCE_ID==120146 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="23", "DAY"] <- "24"
catch[catch$LICENCE_ID==120146 & catch$YEAR==2019 & catch$MONTH=="01", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120146 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-22 23:00:00", "FV_DATE_FISHED"] <- "2019-05-22"
catch[catch$LICENCE_ID==120146 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-26 23:00:00", "FV_DATE_FISHED"] <- "2019-05-26"
catch[catch$LICENCE_ID==120146 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-29 23:00:00", "FV_DATE_FISHED"] <- "2019-05-29"
catch[catch$LICENCE_ID==120146 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120154
catch[catch$LICENCE_ID==120154 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-05", "FV_DATE_FISHED"] <- "2019-05-20"
catch[catch$LICENCE_ID==120154 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-20", "DAY"] <- "20"
catch[catch$LICENCE_ID==120154 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-21", "FV_HOURS_FISHED"] <- "3"
catch[catch$LICENCE_ID==120154 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-22", "FV_HOURS_FISHED"] <- "2"
catch[catch$LICENCE_ID==120154 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-23", "FV_HOURS_FISHED"] <- "2"

#120196
catch[catch$LICENCE_ID==120196 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-17", "FV_DATE_FISHED"] <- "2019-05-24"
catch[catch$LICENCE_ID==120196 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-24", "DAY"] <- "24"
temp <- data.frame(
  LICENCE_ID = 120196,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 100,
  FV_GEAR_CODE = 41L,
  GEAR_DESCRIPTION = "GILL NET (SET OR FIXED)",
  FV_DATE_FISHED = "2019-05-17",
  FV_WEIGHT = 940 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "17",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SALMON",
  COUNTY = "DIGBY COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120203
# catch[catch$LICENCE_ID==120203 & catch$YEAR==2019, "FV_GEAR_CODE "] <- 41L
# catch[catch$LICENCE_ID==120203 & catch$YEAR==2019, "FV_GEAR_CODE "] <- 42L
# catch[catch$LICENCE_ID==120203 & catch$YEAR==2019, "GEAR_DESCRIPTION "] <- "GILL NET (SET OR FIXED)"
# catch[catch$LICENCE_ID==120203 & catch$YEAR==2019, "GEAR_DESCRIPTION "] <- "GILL NET, DRIFT"

#120216
weights <- c(
  400,800,6950,8100,8500,750,450,500,500,2960,350,100,1650,3500,3500,3600,2900,
  1000,4300,2100,1775,1350,3000,1700,900,500
)

dates <- c(
  "2019-05-09","2019-05-10","2019-05-13","2019-05-14","2019-05-15","2019-05-16",
  "2019-05-17","2019-05-20","2019-05-21","2019-05-22","2019-05-23","2019-05-24",
  "2019-05-27","2019-05-28","2019-05-29","2019-05-30","2019-05-31","2019-06-03",
  "2019-06-04","2019-06-05","2019-06-06","2019-06-07","2019-06-11","2019-06-12",
  "2019-06-13","2019-06-17"
  )

subset_rows <- catch$LICENCE_ID==120216 & catch$YEAR==2019
catch$FV_WEIGHT[subset_rows] <- weights
catch$FV_DATE_FISHED[subset_rows] <- dates

catch[catch$LICENCE_ID==120216 & catch$YEAR==2019, "MONTH"] <- "05"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-09", "DAY"] <- "09"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-10", "DAY"] <- "10"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-13", "DAY"] <- "13"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-14", "DAY"] <- "14"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-15", "DAY"] <- "15"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-16", "DAY"] <- "16"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-17", "DAY"] <- "17"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-20", "DAY"] <- "20"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-21", "DAY"] <- "21"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-22", "DAY"] <- "22"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-23", "DAY"] <- "23"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-24", "DAY"] <- "24"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-27", "DAY"] <- "27"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-28", "DAY"] <- "28"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-29", "DAY"] <- "29"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-30", "DAY"] <- "30"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-31", "DAY"] <- "31"

catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-03", "MONTH"] <- "06"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-03", "DAY"] <- "03"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-04", "MONTH"] <- "06"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-04", "DAY"] <- "04"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-05", "MONTH"] <- "06"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-05", "DAY"] <- "05"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-06", "MONTH"] <- "06"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-06", "DAY"] <- "06"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-07", "MONTH"] <- "06"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-07", "DAY"] <- "07"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-11", "MONTH"] <- "06"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-11", "DAY"] <- "11"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-12", "MONTH"] <- "06"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-12", "DAY"] <- "12"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-13", "MONTH"] <- "06"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-13", "DAY"] <- "13"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-17", "MONTH"] <- "06"
catch[catch$LICENCE_ID==120216 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-17", "DAY"] <- "17"

rm(dates)
rm(weights)
rm(subset_rows)
#120232
catch[catch$LICENCE_ID==120232 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120235
catch[catch$LICENCE_ID==120235 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="08", "FV_WEIGHT"] <- 353
catch[catch$LICENCE_ID==120235 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120235 & catch$YEAR==2019, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120235 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120237
catch[catch$LICENCE_ID==120237 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "POUNDS"

#120257
temp <- data.frame(YEAR=2019, LICENCE_ID=120257, NIL_REPORT_FLAG="Y")
didnotfish <- rbind(temp, didnotfish)

#120260
temp <- catch[catch$LICENCE_ID==120260 & catch$YEAR==2019, ]
temp <- temp[!duplicated(temp[c("MONTH","DAY")]), ]
catch <- catch[!(catch$LICENCE_ID==120260 & catch$YEAR==2019), ]
catch <- rbind(catch, temp)

#120271
catch[catch$LICENCE_ID==120271 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "POUNDS"

#120285
catch[catch$LICENCE_ID==120285 & catch$YEAR==2019 & catch$MONTH=="04" & catch$DAY=="17", "FV_WEIGHT"] <- 915
catch[catch$LICENCE_ID==120285 & catch$YEAR==2019 & catch$MONTH=="04" & catch$DAY=="24", "DAY"] <- "14"
catch[catch$LICENCE_ID==120285 & catch$YEAR==2019 & catch$MONTH=="04" & catch$DAY=="14", "FV_DATE_FISHED"] <- "2019-04-14"
catch[catch$LICENCE_ID==120285 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "POUNDS"

#120311
catch[catch$LICENCE_ID==120311 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120311 & catch$YEAR==2019, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120311 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120312
catch[catch$LICENCE_ID==120312 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120312 & catch$YEAR==2019, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120312 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120326
catch[catch$LICENCE_ID==120326 & catch$YEAR==2019, "FV_GEAR_CODE"] <- NA

#120334
catch[catch$LICENCE_ID==120334 & catch$YEAR==2019 & catch$MONTH=="01" & catch$DAY=="14", "MONTH"] <- "04"
catch[catch$LICENCE_ID==120334 & catch$YEAR==2019 & catch$MONTH=="04" & catch$DAY=="14", "FV_DATE_FISHED"] <- "2019-04-14"

#120344
catch[catch$LICENCE_ID==120344 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120344 & catch$YEAR==2019, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120344 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120348
temp <- data.frame(YEAR=2019, LICENCE_ID=120348, NIL_REPORT_FLAG="Y")
didnotfish <- rbind(temp, didnotfish)

#120353
catch[catch$LICENCE_ID==120353 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120353 & catch$YEAR==2019, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120353 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120353 & catch$YEAR==2019, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120353 & catch$YEAR==2019, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120355
catch[catch$LICENCE_ID==120355 & catch$YEAR==2019 &catch$FV_DATE_FISHED=="2019-04-16", "FV_WEIGHT"] <- 88 * conv

#120359
catch[catch$LICENCE_ID==120359 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120377
temp <- data.frame(
  LICENCE_ID = 120377,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 24,
  FV_GEAR_CODE = 62L,
  GEAR_DESCRIPTION = "TRAP",
  FV_DATE_FISHED = "2019-05-28",
  FV_WEIGHT = 23110,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "28",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "OROMOCTO",
  COUNTY = "SUNBURY COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120378
catch[catch$LICENCE_ID==120378 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="10", "FV_HOURS_FISHED"] <- 48

#120387
temp <- data.frame(YEAR=2019, LICENCE_ID=120387, NIL_REPORT_FLAG="Y")
didnotfish <- rbind(temp, didnotfish)

#120426
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019, "FV_GEAR_CODE"] <- 42L
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019, "GEAR_DESCRIPTION"] <-  "GILL NET, DRIFT" 
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019, "MONTH"] <- "05"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-08 23:00:00", "FV_DATE_FISHED"] <- "2019-05-08"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-08 00:00:00", "DAY"] <- "08"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-09 23:00:00", "FV_DATE_FISHED"] <- "2019-05-10"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-12 23:00:00", "FV_DATE_FISHED"] <- "2019-05-13"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-20 23:00:00", "FV_DATE_FISHED"] <- "2019-05-21"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-21 23:00:00", "FV_DATE_FISHED"] <- "2019-05-22"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-22 23:00:00", "FV_DATE_FISHED"] <- "2019-05-23"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-26 23:00:00", "FV_DATE_FISHED"] <- "2019-05-27"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-27 23:00:00", "FV_DATE_FISHED"] <- "2019-05-28"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-28 23:00:00", "FV_DATE_FISHED"] <- "2019-05-29"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-29 23:00:00", "FV_DATE_FISHED"] <- "2019-05-30"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-29 23:00:00", "FV_DATE_FISHED"] <- "2019-05-30"
catch[catch$LICENCE_ID==120426 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-01-30 23:00:00", "FV_DATE_FISHED"] <- "2019-05-31"

#120440
catch[catch$LICENCE_ID==120440 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120440 & catch$YEAR==2019, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120440 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120440 & catch$YEAR==2019, "GEAR_DESCRIPTION"] <- "DIP NET"
catch[catch$LICENCE_ID==120440 & catch$YEAR==2019, "FV_GEAR_CODE"] <- 70L

#120457
catch[catch$LICENCE_ID==120457 & catch$YEAR==2019, "FV_GEAR_CODE"] <- 70L
catch[catch$LICENCE_ID==120457 & catch$YEAR==2019, "GEAR_DESCRIPTION"] <- "DIP NET"

#120462
catch[catch$LICENCE_ID==120462 & catch$YEAR==2019 & catch$MONTH=="04" & catch$DAY=="13", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120462 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="13", "FV_DATE_FISHED"] <- "2019-05-13"
temp <- data.frame(
  LICENCE_ID = 120462,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-05-07",
  FV_WEIGHT = 10 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "07",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "ANNIS",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120482
catch[catch$LICENCE_ID==120482 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "POUNDS"
catch[catch$LICENCE_ID==120482 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-17" & catch$FV_WEIGHT==343, "FV_WEIGHT"] <- 1064
catch[catch$LICENCE_ID==120482 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-20", "FV_WEIGHT"] <- 343
catch[catch$LICENCE_ID==120482 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-21", "FV_WEIGHT"] <- 107
catch[catch$LICENCE_ID==120482 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-22", "FV_WEIGHT"] <- 451

#120495
catch[catch$LICENCE_ID==120495 & catch$YEAR==2019, "FV_GEAR_CODE"] <- 70L
catch[catch$LICENCE_ID==120495 & catch$YEAR==2019, "GEAR_DESCRIPTION"] <- "DIP NET"

#120514
catch[catch$LICENCE_ID==120514 & catch$YEAR==2019, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120514 & catch$YEAR==2019, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120535
catch[catch$LICENCE_ID==120535 & catch$YEAR==2019, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120535 & catch$YEAR==2019, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120548
catch[catch$LICENCE_ID==120548 & catch$YEAR==2019, "FV_GEAR_CODE"] <- 70L
catch[catch$LICENCE_ID==120548 & catch$YEAR==2019, "GEAR_DESCRIPTION"] <- "DIP NET"

#120561
catch[catch$LICENCE_ID==120561 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120561 & catch$YEAR==2019, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120561 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-07-28", "FV_DATE_FISHED"] <- "2019-04-28"
catch[catch$LICENCE_ID==120561 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-04-28", "MONTH"] <- "04"

#120567
catch[catch$LICENCE_ID==120567 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "POUNDS"

#120573
catch[catch$LICENCE_ID==120573 & catch$YEAR==2019 & catch$MONTH=="10" & catch$DAY=="15", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120573 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="15", "FV_DATE_FISHED"] <- "2019-05-15"

#120577
catch[catch$LICENCE_ID==120577 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120588
catch[catch$LICENCE_ID==120588 & catch$YEAR==2019, "FV_GEAR_CODE"] <- 70L
catch[catch$LICENCE_ID==120588 & catch$YEAR==2019, "GEAR_DESCRIPTION"] <- "DIP NET"

#120590

catch <- subset(catch, !(LICENCE_ID==120590 & YEAR==2019))

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 3,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-03",
  FV_WEIGHT = 150 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "03",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MARTIN'S RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 3,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-04",
  FV_WEIGHT = 50 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "04",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "LAHAVE",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 4,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-04",
  FV_WEIGHT = 400 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "04",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MARTIN'S RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-05",
  FV_WEIGHT = 100 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "05",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MIDDLE RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 6,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-05",
  FV_WEIGHT = 700 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "05",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MARTIN'S RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 6,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-06",
  FV_WEIGHT = 25 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "06",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "LAHAVE",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-06",
  FV_WEIGHT = 45 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "06",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "PETITE RIVIERE",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 7,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-09",
  FV_WEIGHT = 1500 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "09",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MARTIN'S RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-10",
  FV_WEIGHT = 25 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "10",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "INDIAN",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 6,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-10",
  FV_WEIGHT = 1200 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "10",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MARTIN'S RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 1,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-11",
  FV_WEIGHT = 10 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "11",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "LAHAVE",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 4,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-11",
  FV_WEIGHT = 300 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "11",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MARTIN'S RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-13",
  FV_WEIGHT = 300 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "13",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MUSHAMUSH",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 8,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-13",
  FV_WEIGHT = 1650 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "13",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MARTIN'S RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 6,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-16",
  FV_WEIGHT = 1400 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "16",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MARTIN'S RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-17",
  FV_WEIGHT = 150 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "17",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MIDDLE RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 6,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-17",
  FV_WEIGHT = 700 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "17",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MARTIN'S RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 1,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-18",
  FV_WEIGHT = 15 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "18",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "LAHAVE",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 4,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-18",
  FV_WEIGHT = 250 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "18",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MARTIN'S RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 3,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-19",
  FV_WEIGHT = 100 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "19",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MARTIN'S RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-20",
  FV_WEIGHT = 60 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "20",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MARTIN'S RIVER",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120590,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 1,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-06-20",
  FV_WEIGHT = 5 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "20",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MUSHAMUSH",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120619
catch[catch$LICENCE_ID==120619 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120619 & catch$YEAR==2019, "FV_WEIGHT"] * conv

#120626
catch[catch$LICENCE_ID==120626 & catch$YEAR==2019, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120626 & catch$YEAR==2019, "GEAR_DESCRIPTION"] <- "DIP STAND"
catch[catch$LICENCE_ID==120626 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120633
catch[catch$LICENCE_ID==120633 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120633 & catch$YEAR==2019 & catch$MONTH=="01" & catch$DAY=="12", "MONTH"] <- "06"
catch[catch$LICENCE_ID==120633 & catch$YEAR==2019 & catch$MONTH=="06" & catch$DAY=="12", "FV_DATE_FISHED"] <- "2019-06-12"

#120636
catch[catch$LICENCE_ID==120636 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120636 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-22", "FV_WEIGHT"] <- 1200

#120645
temp <- data.frame(YEAR=2019, LICENCE_ID=120645, NIL_REPORT_FLAG="Y")
didnotfish <- rbind(temp, didnotfish)

#120654
catch[catch$LICENCE_ID==120654 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120654 & catch$YEAR==2019, "FV_GEAR_CODE"] <- 70L
catch[catch$LICENCE_ID==120654 & catch$YEAR==2019, "GEAR_DESCRIPTION"] <- "DIP NET"

#120663
#catch[catch$LICENCE_ID==120663 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-11", "RIVERNAME_CLEANED"] <- "LAHAVE"
#catch[catch$LICENCE_ID==120663 & catch$YEAR==2019 & catch$RIVERNAME_CLEANED=="MAHONE BAY", "RIVERNAME_CLEANED"] <- "MARTINS RIVER"

#120665
catch[catch$LICENCE_ID==120665 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-08", "FV_WEIGHT"] <- 600

#120679
catch[catch$LICENCE_ID==120679 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120698
catch[catch$LICENCE_ID==120698 & catch$YEAR==2019 & catch$MONTH=="06" & catch$DAY=="07", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120698 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="07", "FV_DATE_FISHED"] <- "2019-05-07"

#120715
catch[catch$LICENCE_ID==120715 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-08", "FV_DATE_FISHED"] <- "2019-05-09"
catch[catch$LICENCE_ID==120715 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-09", "DAY"] <- "09"
temp <- data.frame(
  LICENCE_ID = 120715,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2019,
  FV_HOURS_FISHED = 3,
  FV_GEAR_CODE = 70,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-05-08",
  FV_WEIGHT = 92 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "08",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "ARGYLE",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120716
catch[catch$LICENCE_ID==120716 & catch$YEAR==2019 & catch$MONTH=="04" & catch$DAY=="30", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120716 & catch$YEAR==2019 & catch$MONTH=="05" & catch$DAY=="30", "FV_DATE_FISHED"] <- "2019-05-30"

#120751
catch[catch$LICENCE_ID==120751 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-20", "FV_WEIGHT"] <- 500
catch[catch$LICENCE_ID==120751 & catch$YEAR==2019, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120751 & catch$YEAR==2019, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120751 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#303395
catch[catch$LICENCE_ID==303395 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-08" & catch$FV_WEIGHT==40.00, "FV_DATE_FISHED"] <- "2019-05-06"
catch[catch$LICENCE_ID==303395 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-05-06", "DAY"] <- "06"
catch[catch$LICENCE_ID==303395 & catch$YEAR==2019, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#303397
catch[catch$LICENCE_ID==303397 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-04-22" & catch$FV_WEIGHT==28.80, "FV_DATE_FISHED"] <- "2019-04-23"
catch[catch$LICENCE_ID==303397 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-04-23", "DAY"] <- "23"

# 2020 #########################################################################

#120039
catch[catch$LICENCE_ID==120039 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120039 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120065
catch[catch$LICENCE_ID==120065 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120072
catch[catch$LICENCE_ID==120072 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120072 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120077
temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-04-27",
  FV_WEIGHT = 4201,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "04",
  DAY = "27",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-04-30",
  FV_WEIGHT = 2457,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "04",
  DAY = "30",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-01",
  FV_WEIGHT = 4540,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "01",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-03",
  FV_WEIGHT = 2731,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "03",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-04",
  FV_WEIGHT = 5290,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "04",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-06",
  FV_WEIGHT = 713,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "06",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-07",
  FV_WEIGHT = 1203,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "07",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-08",
  FV_WEIGHT = 4328,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "08",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-1",
  FV_WEIGHT = 3535,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "11",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-12",
  FV_WEIGHT = 2501,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "12",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-13",
  FV_WEIGHT = 2384,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "13",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-12",
  FV_WEIGHT = 2452,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "12",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-18",
  FV_WEIGHT = 3388,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "18",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-19",
  FV_WEIGHT = 905,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "19",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-20",
  FV_WEIGHT = 1776,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "20",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-22",
  FV_WEIGHT = 1388,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "22",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120077,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = NA,
  GEAR_DESCRIPTION = NA,
  FV_DATE_FISHED = "2019-05-25",
  FV_WEIGHT = 1400,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "25",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120077
temp <- data.frame(
  LICENCE_ID = 120085,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-05-29",
  FV_WEIGHT = 20000 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "29",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "ANNIS",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120085,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-05-30",
  FV_WEIGHT = 35000 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "30",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "ANNIS",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120085,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2019-05-31",
  FV_WEIGHT = 45000 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "31",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "ANNIS",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120088
catch[catch$LICENCE_ID==120088 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120088 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120089
catch[catch$LICENCE_ID==120089 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120112
catch[catch$LICENCE_ID==120112 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-07", "FV_WEIGHT"] <- 35 * conv

#120123
catch[catch$LICENCE_ID==120123 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-01-28 23:00:00", "FV_DATE_FISHED"] <- "2020-05-29 00:00:00"
catch[catch$LICENCE_ID==120123 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-29", "MONTH"] <- "05"

#120124
catch[catch$LICENCE_ID==120124 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-07", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120124 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-07", "FV_DATE_FISHED"] <- "2020-05-07"
catch[catch$LICENCE_ID==120124 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-12", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120124 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-12", "FV_DATE_FISHED"] <- "2020-05-12"
catch[catch$LICENCE_ID==120124 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-14" & catch$FV_WEIGHT==1500, "FV_DATE_FISHED"] <- "2020-05-15"
catch[catch$LICENCE_ID==120124 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-15", "DAY"] <- "15"
catch[catch$LICENCE_ID==120124 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-22" & catch$FV_WEIGHT==500, "FV_DATE_FISHED"] <- "2020-05-25"
catch[catch$LICENCE_ID==120124 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-25", "DAY"] <- "25"

#120135
catch[catch$LICENCE_ID==120135 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120135 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120135 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"
catch[catch$LICENCE_ID==120135 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-24" & catch$FV_WEIGHT==405, "FV_DATE_FISHED"] <- "2020-05-27"
catch[catch$LICENCE_ID==120135 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-27", "DAY"] <- "27"

#120136
catch[catch$LICENCE_ID==120136 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120136 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120139
catch[catch$LICENCE_ID==120139 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-18", "FV_DATE_FISHED"] <- "2020-06-18"
catch[catch$LICENCE_ID==120139 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-18", "MONTH"] <- "06"

#120145
catch[catch$LICENCE_ID==120145 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-12", "FV_WEIGHT"] <- 650 * conv

#120157
catch[catch$LICENCE_ID==120157 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120170
catch[catch$LICENCE_ID==120170 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-04-20", "FV_DATE_FISHED"] <- "2020-05-20"
catch[catch$LICENCE_ID==120170 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-20", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120170 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-11", "FV_WEIGHT"] <- 130.7
catch[catch$LICENCE_ID==120170 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-22", "FV_WEIGHT"] <- 130.7

#120178
catch[catch$LICENCE_ID==120178 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120186
catch[catch$LICENCE_ID==120186 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120200
catch[catch$LICENCE_ID==120200 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-31", "FV_HOURS_FISHED"] <- 0
catch[catch$LICENCE_ID==120200 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-10", "FV_HOURS_FISHED"] <- 48
catch[catch$LICENCE_ID==120200 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-11", "FV_HOURS_FISHED"] <- 48
catch[catch$LICENCE_ID==120200 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-18", "FV_HOURS_FISHED"] <- 48

#120202
catch[catch$LICENCE_ID==120202 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120202 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120215
temp <- data.frame(
  LICENCE_ID = 120215,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = 3,
  FV_GEAR_CODE = 41L,
  GEAR_DESCRIPTION = "GILL NET (SET OR FIXED)",
  FV_DATE_FISHED = "2019-04-19",
  FV_WEIGHT = 14 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "19",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120226
catch[catch$LICENCE_ID==120226 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120226 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120227
catch[catch$LICENCE_ID==120227 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 62L
catch[catch$LICENCE_ID==120227 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "TRAP"

#120232
catch[catch$LICENCE_ID==120232 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
temp <- data.frame(
  LICENCE_ID = 120232,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = 7,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-06-11",
  FV_WEIGHT = 80,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "11",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120235
catch[catch$LICENCE_ID==120235 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120236
catch[catch$LICENCE_ID==120236 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 44L
catch[catch$LICENCE_ID==120236 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "SQUARE NET"

#120238
catch[catch$LICENCE_ID==120238 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-14" & catch$FV_WEIGHT==23, "FV_DATE_FISHED"] <- "2020-04-14"
catch[catch$LICENCE_ID==120238 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-04-14", "MONTH"] <- "04"
catch[catch$LICENCE_ID==120238 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120239
catch[catch$LICENCE_ID==120239 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120239 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120242
catch[catch$LICENCE_ID==120242 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120244
catch[catch$LICENCE_ID==120244 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-01", "FV_DATE_FISHED"] <- "2020-05-05"
catch[catch$LICENCE_ID==120244 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-05", "DAY"] <- "05"

#120249
catch[catch$LICENCE_ID==120249 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-01-20 23:00:00", "FV_DATE_FISHED"] <- "2020-05-21"
catch[catch$LICENCE_ID==120249 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-21", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120249 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120249 & catch$YEAR==2020, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120249 & catch$YEAR==2020, "FV_WEIGHT"] * conv

#120279
catch[catch$LICENCE_ID==120279 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120292
catch[catch$LICENCE_ID==120292 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 45L
catch[catch$LICENCE_ID==120292 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "BOX NET"

#120296
catch[catch$LICENCE_ID==120296 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120296 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-04-24", "FV_WEIGHT"] <- 6636.4
catch[catch$LICENCE_ID==120296 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-06", "FV_DATE_FISHED"] <- "2020-05-06"
catch[catch$LICENCE_ID==120296 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-06", "MONTH"] <- "05"

#120298
catch[catch$LICENCE_ID==120298 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120299
catch[catch$LICENCE_ID==120299 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-17" & catch$FV_WEIGHT==0.24, "FV_DATE_FISHED"] <- "2020-04-17"
catch[catch$LICENCE_ID==120299 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-04-17", "MONTH"] <- "05"

#120306
catch[catch$LICENCE_ID==120306 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-03", "FV_WEIGHT"] <- 200 * conv
catch[catch$LICENCE_ID==120306 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-04", "FV_WEIGHT"] <- 200 * conv

#120314
catch[catch$LICENCE_ID==120314 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120314 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120315
catch[catch$LICENCE_ID==120315 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-01", "FV_DATE_FISHED"] <- "2020-05-07"
catch[catch$LICENCE_ID==120315 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-07", "DAY"] <- "07"

#120317
catch <- subset(catch, !(LICENCE_ID==120317 & YEAR==2020 & FV_DATE_FISHED=="2020-05-27" & RIVERNAME_CLEANED=="THE KEYHOLE"))

#120318
catch[catch$LICENCE_ID==120318 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-31", "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120318 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-31", "GEAR_DESCRIPTION"] <- "DIP STAND"

#120319
catch[catch$LICENCE_ID==120319 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-25", "FV_DATE_FISHED"] <- "2020-04-25"
catch[catch$LICENCE_ID==120319 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-04-25", "MONTH"] <- "04"
catch[catch$LICENCE_ID==120319 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-27", "FV_DATE_FISHED"] <- "2020-04-27"
catch[catch$LICENCE_ID==120319 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-04-27", "MONTH"] <- "04"
catch[catch$LICENCE_ID==120319 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120319 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120322
catch[catch$LICENCE_ID==120322 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-18", "FV_WEIGHT"] <- 12700.58

#120326
catch[catch$LICENCE_ID==120326 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120326 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120326 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120330
catch[catch$LICENCE_ID==120330 & catch$YEAR==2020 & catch$FV_GEAR_CODE==84, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120330 & catch$YEAR==2020 & catch$FV_GEAR_CODE==08, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120353
catch[catch$LICENCE_ID==120353 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120353 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120359
catch[catch$LICENCE_ID==120359 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120359 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120367
catch[catch$LICENCE_ID==120367 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120367 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120372
catch[catch$LICENCE_ID==120372 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120377
temp <- data.frame(
  LICENCE_ID = 120377,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = 24L,
  FV_GEAR_CODE = 62L,
  GEAR_DESCRIPTION = "TRAP",
  FV_DATE_FISHED = "2020-04-29",
  FV_WEIGHT = 6000,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "04",
  DAY = "29",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MAQUAPIT LAKE",
  COUNTY = "MULTI",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120381
catch[catch$LICENCE_ID==120381 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-01-28 23:00:00", "FV_DATE_FISHED"] <- "2020-04-29"
catch[catch$LICENCE_ID==120381 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-04-29", "MONTH"] <- "04"

#120388
catch[catch$LICENCE_ID==120388 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120388 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120388 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120404
catch[catch$LICENCE_ID==120404 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-29", "FV_WEIGHT"] <- 187

#120407
catch[catch$LICENCE_ID==120407 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 70L
catch[catch$LICENCE_ID==120407 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP NET"

#120417
catch[catch$LICENCE_ID==120417 & catch$YEAR==2020, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120417 & catch$YEAR==2020, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120417 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120423
catch <- subset(catch, !(LICENCE_ID==120423 & YEAR==2020 & MEASUREMENT_UNIT=="POUNDS"))

#120426
catch <- subset(catch, !(LICENCE_ID==120426 & YEAR==2020))

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-01",
  FV_WEIGHT = 4 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "01",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "AVON",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-08",
  FV_WEIGHT = 8 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "08",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "AVON",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-15",
  FV_WEIGHT = 470 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "15",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "AVON",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-17",
  FV_WEIGHT = 47 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "17",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "AVON",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-22",
  FV_WEIGHT = 42 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "22",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "AVON",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-24",
  FV_WEIGHT = 4 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "24",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "AVON",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-27",
  FV_WEIGHT = 148 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "27",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "AVON",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-30",
  FV_WEIGHT = 193 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "30",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "AVON",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-05-04",
  FV_WEIGHT = 1478 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "04",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "AVON",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-05-05",
  FV_WEIGHT = 352,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "05",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "AVON",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-06",
  FV_WEIGHT = 3 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "06",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "HALFWAY",
  COUNTY = "MULTI",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-09",
  FV_WEIGHT = 3 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "09",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "HALFWAY",
  COUNTY = "MULTI",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-13",
  FV_WEIGHT = 6 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "13",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "HALFWAY",
  COUNTY = "MULTI",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-16",
  FV_WEIGHT = 32 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "16",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "HALFWAY",
  COUNTY = "MULTI",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-20",
  FV_WEIGHT = 110 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "20",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "HALFWAY",
  COUNTY = "MULTI",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-25",
  FV_WEIGHT = 182 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "25",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "HALFWAY",
  COUNTY = "MULTI",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-29",
  FV_WEIGHT = 16 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "29",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "HALFWAY",
  COUNTY = "MULTI",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-05-01",
  FV_WEIGHT = 82 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "01",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "HALFWAY",
  COUNTY = "MULTI",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-05-05",
  FV_WEIGHT = 650 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "05",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "HALFWAY",
  COUNTY = "MULTI",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-06",
  FV_WEIGHT = 1 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "06",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "COGMAGUN",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-13",
  FV_WEIGHT = 2 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "13",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "COGMAGUN",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-16",
  FV_WEIGHT = 20 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "16",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "COGMAGUN",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-20",
  FV_WEIGHT = 12 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "20",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "COGMAGUN",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-25",
  FV_WEIGHT = 16 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "25",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "COGMAGUN",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-04-29",
  FV_WEIGHT = 5 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "29",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "COGMAGUN",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-05-01",
  FV_WEIGHT = 42 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "01",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "COGMAGUN",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120426,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2020-05-05",
  FV_WEIGHT = 16 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "05",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "COGMAGUN",
  COUNTY = "HANTS COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120438
catch[catch$LICENCE_ID==120438 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120440
catch[catch$LICENCE_ID==120440 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120450
catch[catch$LICENCE_ID==120450 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120450 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120450 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120453
catch[catch$LICENCE_ID==120453 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-02", "FV_DATE_FISHED"] <- "2020-05-22"
catch[catch$LICENCE_ID==120453 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-22", "DAY"] <- "22"
catch[catch$LICENCE_ID==120453 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120453 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120459
temp <- data.frame(
  YEAR = "2020",
  LICENCE_ID = "120459",
  NIL_REPORT_FLAG = "Y"
)
didnotfish <- rbind(didnotfish, temp)
didnotfish <- subset(didnotfish, !(LICENCE_ID=="120459" & YEAR=="2020" & NIL_REPORT_FLAG=="N"))
catch <- subset(catch, !(LICENCE_ID==120459 & YEAR==2020))

#120465
temp <- data.frame(
  YEAR = "2020",
  LICENCE_ID = "120465",
  NIL_REPORT_FLAG = "Y"
)
didnotfish <- rbind(didnotfish, temp)
didnotfish <- subset(didnotfish, !(LICENCE_ID=="120465" & YEAR=="2020" & NIL_REPORT_FLAG=="N"))
catch <- subset(catch, !(LICENCE_ID==120465 & YEAR==2020))

#120472
catch[catch$LICENCE_ID==120472 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-29", "FV_WEIGHT"] <- 1200
catch[catch$LICENCE_ID==120472 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-30", "FV_WEIGHT"] <- 1200
catch[catch$LICENCE_ID==120472 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-31", "FV_WEIGHT"] <- 1200

#120473
catch[catch$LICENCE_ID==120473 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120473 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120474
catch[catch$LICENCE_ID==120474 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120477
catch[catch$LICENCE_ID==120477 & catch$YEAR==2020, "RIVERNAME_CLEANED"] <- "SYDNEY RIVER"

#120479
catch[catch$LICENCE_ID==120479 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120485
temp <- data.frame(
  LICENCE_ID = 120485,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = 2L,
  FV_GEAR_CODE = 62L,
  GEAR_DESCRIPTION = "TRAP",
  FV_DATE_FISHED = "2020-06-07",
  FV_WEIGHT = 20,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "06",
  DAY = "07",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "MELONEY CREEK",
  COUNTY = "CAPE BRETON COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)
catch[catch$LICENCE_ID==120485 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120490
catch[catch$LICENCE_ID==120490 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120490 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120495
catch[catch$LICENCE_ID==120495 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120495 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"
catch[catch$LICENCE_ID==120495 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120514
catch[catch$LICENCE_ID==120514 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120514 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120548
catch[catch$LICENCE_ID==120548 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120548 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120561
catch[catch$LICENCE_ID==120561 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-31", "FV_WEIGHT"] <- 400 / 2.205

#120562
catch[catch$LICENCE_ID==120562 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-29", "FV_WEIGHT"] <- 5478 / 2.205
catch[catch$LICENCE_ID==120562 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-30", "FV_WEIGHT"] <- 2523 / 2.205
catch[catch$LICENCE_ID==120562 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120573
catch[catch$LICENCE_ID==120573 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-01-24 23:00:00", "FV_DATE_FISHED"] <- "2020-05-24"
catch[catch$LICENCE_ID==120573 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-24", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120573 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-24", "FV_WEIGHT"] <- 300 * conv
catch[catch$LICENCE_ID==120573 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-29", "FV_WEIGHT"] <- 300 * conv

#120577
catch[catch$LICENCE_ID==120577 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120590
catch[catch$LICENCE_ID==120590 & catch$YEAR==2020 & catch$RIVERNAME_CLEANED=="MARTIN'S RIVER", "RIVERNAME_CLEANED"] <- "MIDDLE RIVER"

#120592
catch[catch$LICENCE_ID==120592 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120608
catch[catch$LICENCE_ID==120608 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-02", "FV_HOURS_FISHED"] <- 8L

#120613
catch[catch$LICENCE_ID==120613 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120613 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"
catch[catch$LICENCE_ID==120613 & catch$YEAR==2020, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120613 & catch$YEAR==2020, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120613 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120626
catch[catch$LICENCE_ID==120626 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120626 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120633
catch[catch$LICENCE_ID==120633 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-25", "FV_HOURS_FISHED"] <- 4L
catch[catch$LICENCE_ID==120633 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-27", "FV_HOURS_FISHED"] <- 5L
catch[catch$LICENCE_ID==120633 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-01", "FV_HOURS_FISHED"] <- 5L
catch[catch$LICENCE_ID==120633 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-04", "FV_HOURS_FISHED"] <- 5L
catch[catch$LICENCE_ID==120633 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-07", "FV_HOURS_FISHED"] <- 4L
catch[catch$LICENCE_ID==120633 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-08", "FV_HOURS_FISHED"] <- 6L
catch[catch$LICENCE_ID==120633 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-11", "FV_HOURS_FISHED"] <- 7L
catch[catch$LICENCE_ID==120633 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-14", "FV_HOURS_FISHED"] <- 7L
catch[catch$LICENCE_ID==120633 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120635
temp <- data.frame(
  LICENCE_ID = 120635,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2020,
  FV_HOURS_FISHED = 3L,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2020-05-13",
  FV_WEIGHT = 600,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "13",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "DOREY ROAD",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120636
catch[catch$LICENCE_ID==120636 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-21", "FV_HOURS_FISHED"] <- 6
catch[catch$LICENCE_ID==120636 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-05-25", "FV_HOURS_FISHED"] <- 8
catch[catch$LICENCE_ID==120636 & catch$YEAR==2020, "FV_GEAR_CODE"] <- 62L
catch[catch$LICENCE_ID==120636 & catch$YEAR==2020, "GEAR_DESCRIPTION"] <- "TRAP"

#120644
catch <- subset(catch, !(LICENCE_ID==120644 & YEAR==2020 & RIVERNAME_LOGBOOK=="PETITE RIVERIERE"))

#120663
catch[catch$LICENCE_ID==120663 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-02", "RIVERNAME_CLEANED"] <- "LAHAVE"
catch[catch$LICENCE_ID==120663 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-04", "RIVERNAME_CLEANED"] <- "LAHAVE"
catch[catch$LICENCE_ID==120663 & catch$YEAR==2020 & catch$FV_DATE_FISHED=="2020-06-08", "RIVERNAME_CLEANED"] <- "LAHAVE"

#120677
catch[catch$LICENCE_ID==120677 & catch$YEAR==2020, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120677 & catch$YEAR==2020, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120677 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120679
catch[catch$LICENCE_ID==120679 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120680
catch[catch$LICENCE_ID==120680 & catch$YEAR==2020, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120680 & catch$YEAR==2020, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120680 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120707
catch[catch$LICENCE_ID==120707 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120708
catch[catch$LICENCE_ID==120708 & catch$YEAR==2020, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120708 & catch$YEAR==2020, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120708 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120751
catch[catch$LICENCE_ID==120751 & catch$YEAR==2020, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120751 & catch$YEAR==2020, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120751 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120753
catch[catch$LICENCE_ID==120753 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120755
catch[catch$LICENCE_ID==120755 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120760
catch[catch$LICENCE_ID==120760 & catch$YEAR==2020, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

# 2021 #########################################################################

#120028
catch[catch$LICENCE_ID==120028 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120055
catch[catch$LICENCE_ID==120055 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

temp <- data.frame(
  LICENCE_ID = 120055,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2019-04-26",
  FV_WEIGHT = 52,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "26",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120055,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2019-04-28",
  FV_WEIGHT = 97,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "28",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120055,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2019-04-30",
  FV_WEIGHT = 234,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "30",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120055,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2019-05-03",
  FV_WEIGHT = 185,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "03",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120055,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2019-05-04",
  FV_WEIGHT = 92,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "04",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120055,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2019-05-10",
  FV_WEIGHT = 63,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "10",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120055,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2019-05-11",
  FV_WEIGHT = 122,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "11",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120055,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2019-05-12",
  FV_WEIGHT = 327,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "12",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120055,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 42L,
  GEAR_DESCRIPTION = "GILL NET, DRIFT",
  FV_DATE_FISHED = "2019-05-13",
  FV_WEIGHT = 328,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "13",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120065
catch[catch$LICENCE_ID==120065 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120072
catch[catch$LICENCE_ID==120072 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120072 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120088
catch[catch$LICENCE_ID==120088 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120095
catch[catch$LICENCE_ID==120095 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-17", "FV_DATE_FISHED"] <- "2021-05-17"
catch[catch$LICENCE_ID==120095 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-17", "MONTH"] <- "05"

#120112
catch[catch$LICENCE_ID==120112 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120112 & catch$YEAR==2021, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120112 & catch$YEAR==2021, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120112 & catch$YEAR==2021, "FV_HOURS_FISHED"] <- "24"
catch[catch$LICENCE_ID==120112 & catch$YEAR==2021, "RIVERNAME_CLEANED"] <- "CATALONE"

#120124
catch[catch$LICENCE_ID==120124 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120135
catch[catch$LICENCE_ID==120135 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120135 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120136
catch[catch$LICENCE_ID==120136 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120136 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120142
temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 8,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-04-20",
  FV_WEIGHT = 438 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "20",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 9,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-04-21",
  FV_WEIGHT = 656 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "21",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 8,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-04-22",
  FV_WEIGHT = 283 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "22",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 9,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-04-25",
  FV_WEIGHT = 124 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "25",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 7,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-04-26",
  FV_WEIGHT = 264 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "26",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 10,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-04-27",
  FV_WEIGHT = 300 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "27",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 9,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-04-28",
  FV_WEIGHT = 181 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "28",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 10,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-04-29",
  FV_WEIGHT = 352 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "29",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 10,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-05-02",
  FV_WEIGHT = 956 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "02",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 11,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-05-03",
  FV_WEIGHT = 777 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "03",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 10,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-05-04",
  FV_WEIGHT = 850 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "04",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 9,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-05-05",
  FV_WEIGHT = 971 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "05",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120142,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 10,
  FV_GEAR_CODE = 44L,
  GEAR_DESCRIPTION = "SQUARE NET",
  FV_DATE_FISHED = "2021-05-06",
  FV_WEIGHT = 705 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "06",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SHUBENACADIE",
  COUNTY = "COLCHESTER COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120146
catch[catch$LICENCE_ID==120146 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120157
catch[catch$LICENCE_ID==120157 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120159
catch[catch$LICENCE_ID==120159 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120159 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120168
catch[catch$LICENCE_ID==120168 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120168 & catch$YEAR==2021, "RIVERNAME_CLEANED"] <- "CATALONE"

#120180
catch[catch$LICENCE_ID==120180 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120186
catch[catch$LICENCE_ID==120186 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-30", "FV_DATE_FISHED"] <- "2021-05-30"
catch[catch$LICENCE_ID==120186 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-30", "MONTH"] <- "05"

catch[catch$LICENCE_ID==120186 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-03-31", "FV_DATE_FISHED"] <- "2021-04-30"
catch[catch$LICENCE_ID==120186 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-30", "MONTH"] <- "04"
catch[catch$LICENCE_ID==120186 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-30", "DAY"] <- "30"

catch[catch$LICENCE_ID==120186 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120202
catch[catch$LICENCE_ID==120202 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120202 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"
catch[catch$LICENCE_ID==120202 & catch$YEAR==2021, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120202 & catch$YEAR==2021, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120202 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120203
catch[catch$LICENCE_ID==120203 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 42L
catch[catch$LICENCE_ID==120203 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "GILL NET, DRIFT"

#120226
catch[catch$LICENCE_ID==120226 & catch$YEAR==2021, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120226 & catch$YEAR==2021, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120226 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120226 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120233
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-26", "FV_HOURS_FISHED"] <- 10
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-27", "FV_HOURS_FISHED"] <- 8
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-29", "FV_HOURS_FISHED"] <- 10
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-30", "FV_HOURS_FISHED"] <- 12
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-03", "FV_HOURS_FISHED"] <- 14
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-04", "FV_HOURS_FISHED"] <- 14
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-06", "FV_HOURS_FISHED"] <- 14
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-07", "FV_HOURS_FISHED"] <- 12
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-10", "FV_HOURS_FISHED"] <- 14
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-11", "FV_HOURS_FISHED"] <- 14
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-13", "FV_HOURS_FISHED"] <- 14
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-14", "FV_HOURS_FISHED"] <- 14
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-17", "FV_HOURS_FISHED"] <- 12
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-18", "FV_HOURS_FISHED"] <- 10
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-20", "FV_HOURS_FISHED"] <- 10
catch[catch$LICENCE_ID==120233 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-21", "FV_HOURS_FISHED"] <- 12

#120236
catch[catch$LICENCE_ID==120236 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 44L
catch[catch$LICENCE_ID==120236 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "SQUARE NET"

#120237
catch[catch$LICENCE_ID==120237 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-01-26 23:00:00", "FV_DATE_FISHED"] <- "2021-05-27"
catch[catch$LICENCE_ID==120237 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-27", "MONTH"] <- "05"

#120239
catch[catch$LICENCE_ID==120239 & catch$YEAR==2021, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120239 & catch$YEAR==2021, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120239 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120239 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120242
catch[catch$LICENCE_ID==120242 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120246
catch[catch$LICENCE_ID==120246 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"
catch[catch$LICENCE_ID==120246 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 44L
catch[catch$LICENCE_ID==120246 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "SQUARE NET"

#120250
catch[catch$LICENCE_ID==120250 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-01-29 23:00:00", "FV_DATE_FISHED"] <- "2021-05-31"
catch[catch$LICENCE_ID==120250 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-31", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120250 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-31", "DAY"] <- "31"

#120255
catch[catch$LICENCE_ID==120255 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120256
catch[catch$LICENCE_ID==120256 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-23", "FV_DATE_FISHED"] <- "2021-04-03"
catch[catch$LICENCE_ID==120256 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-03", "DAY"] <- "03"

#120263
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021, "MONTH"] <- "05"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-01-31 23:00:00", "FV_DATE_FISHED"] <- "2021-05-01"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-02-01 23:00:00", "FV_DATE_FISHED"] <- "2021-05-02"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-02-02 23:00:00", "FV_DATE_FISHED"] <- "2021-05-03"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-02-07 23:00:00", "FV_DATE_FISHED"] <- "2021-05-08"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-02-08 23:00:00", "FV_DATE_FISHED"] <- "2021-05-09"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-02-09 23:00:00", "FV_DATE_FISHED"] <- "2021-05-10"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-02-13 23:00:00", "FV_DATE_FISHED"] <- "2021-05-14"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-02-14 23:00:00", "FV_DATE_FISHED"] <- "2021-05-15"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-02-17 23:00:00", "FV_DATE_FISHED"] <- "2021-05-18"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-02-20 23:00:00", "FV_DATE_FISHED"] <- "2021-05-21"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-02-22 23:00:00", "FV_DATE_FISHED"] <- "2021-05-22"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-22" & catch$FV_WEIGHT==192, "DAY"] <- "22"
catch[catch$LICENCE_ID==120263 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-22" & catch$FV_WEIGHT==24, "FV_DATE_FISHED"] <- "2021-05-23"

#120279
catch[catch$LICENCE_ID==120279 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120286
catch[catch$LICENCE_ID==120286 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120286 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120287
didnotfish <- subset(didnotfish, !(LICENCE_ID=="120287" & YEAR=="2021" & NIL_REPORT_FLAG=="N"))

#120292
catch[catch$LICENCE_ID==120292 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120304
catch[catch$LICENCE_ID==120304 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-06-07", "FV_WEIGHT"] <- 10800 * conv
catch <- subset(catch, !(LICENCE_ID==120304 & YEAR==2021 & FV_DATE_FISHED=="2021-05-07"))

#120311
catch[catch$LICENCE_ID==120311 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-08", "FV_WEIGHT"] <- 700 * conv

#120314
temp <- catch %>% filter(LICENCE_ID==120314 & YEAR==2021) %>% mutate(FV_GEAR_CODE = if_else(is.na(FV_GEAR_CODE), 08, FV_GEAR_CODE))
catch <- catch %>% anti_join(temp, by = c("LICENCE_ID", "YEAR")) %>%  bind_rows(temp)

temp <- catch %>% filter(LICENCE_ID==120314 & YEAR==2021) %>% mutate(GEAR_DESCRIPTION = if_else(is.na(GEAR_DESCRIPTION), "DIP STAND", GEAR_DESCRIPTION))
catch <- catch %>% anti_join(temp, by = c("LICENCE_ID", "YEAR")) %>%  bind_rows(temp)

catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-01", "FV_DATE_FISHED"] <- "2021-05-01"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-01", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-02", "FV_DATE_FISHED"] <- "2021-05-02"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-02", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-03", "FV_DATE_FISHED"] <- "2021-05-03"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-03", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-07", "FV_DATE_FISHED"] <- "2021-05-07"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-07", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-08", "FV_DATE_FISHED"] <- "2021-05-08"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-08", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-09", "FV_DATE_FISHED"] <- "2021-05-09"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-09", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-10", "FV_DATE_FISHED"] <- "2021-05-10"
catch[catch$LICENCE_ID==120314 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-10", "MONTH"] <- "05"

#120317
catch[catch$LICENCE_ID==120317 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-13" & catch$FV_WEIGHT==100, "FV_WEIGHT"] <- 1000
catch[catch$LICENCE_ID==120317 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-14" & catch$FV_WEIGHT==2600, "FV_WEIGHT"] <- 26000
catch[catch$LICENCE_ID==120317 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-07" & catch$FV_WEIGHT==100, "FV_WEIGHT"] <- 1000
catch[catch$LICENCE_ID==120317 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-11" & catch$FV_WEIGHT==200, "FV_WEIGHT"] <- 2000
catch[catch$LICENCE_ID==120317 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-12" & catch$FV_WEIGHT==9200, "FV_DATE_FISHED"] <- "2021-05-13"
catch[catch$LICENCE_ID==120317 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-13", "DAY"] <- "13"

#120318
catch[catch$LICENCE_ID==120318 & catch$YEAR==2021 & catch$FV_WEIGHT==1200, "FV_WEIGHT"] <- 12000 * conv
catch <- subset(catch, !(LICENCE_ID==120318 & YEAR==2021 & FV_WEIGHT==1680))

#120322
catch[catch$LICENCE_ID==120322 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

temp <- data.frame(
  LICENCE_ID = 120322,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 48,
  FV_GEAR_CODE = 45L,
  GEAR_DESCRIPTION = "BOX NET",
  FV_DATE_FISHED = "2021-05-28",
  FV_WEIGHT = 2600,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "28",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "SAINT JOHN",
  COUNTY = "SAINT JOHN COUNTY",
  PROVINCE = "NB",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120326
catch[catch$LICENCE_ID==120326 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120326 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120334
catch[catch$LICENCE_ID==120334 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120334 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120336
catch <- subset(catch, !(LICENCE_ID==120336 & YEAR==2021 & FV_DATE_FISHED=="2021-05-14"))
catch <- subset(catch, !(LICENCE_ID==120336 & YEAR==2021 & FV_DATE_FISHED=="2021-05-15"))

temp <- data.frame(
  LICENCE_ID = 120336,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 41L,
  GEAR_DESCRIPTION = "GILL NET (SET OR FIXED)",
  FV_DATE_FISHED = "2021-05-14",
  FV_WEIGHT = 12700 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "14",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120336,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 41L,
  GEAR_DESCRIPTION = "GILL NET (SET OR FIXED)",
  FV_DATE_FISHED = "2021-05-15",
  FV_WEIGHT = 10300 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "15",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120381
catch[catch$LICENCE_ID==120381 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-09", "FV_DATE_FISHED"] <- "2021-04-29"
catch[catch$LICENCE_ID==120381 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-29", "DAY"] <- "29"

#120437
catch[catch$LICENCE_ID==120437 & catch$YEAR==2021, "FV_HOURS_FISHED"] <- 24

#120441
catch[catch$LICENCE_ID==120441 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120441 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120449
catch <- subset(catch, !(LICENCE_ID==120449 & YEAR==2021))

temp <- data.frame(
  LICENCE_ID = 120449,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 6,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-04-09",
  FV_WEIGHT = 1300 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "09",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120449,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 4,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-04-11",
  FV_WEIGHT = 1200 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "11",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120449,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 4,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-04-24",
  FV_WEIGHT = 1600 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "24",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120449,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 4,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-04-25",
  FV_WEIGHT = 780 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "04",
  DAY = "25",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120449,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 5,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-05-01",
  FV_WEIGHT = 1300 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "01",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120449,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 3,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-05-02",
  FV_WEIGHT = 800 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "02",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120449,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 4,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-05-08",
  FV_WEIGHT = 1600 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "08",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120449,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 4,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-05-09",
  FV_WEIGHT = 400 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "09",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)


temp <- data.frame(
  LICENCE_ID = 120449,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 2,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-05-22",
  FV_WEIGHT = 900 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "22",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120449,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 6,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-05-23",
  FV_WEIGHT = 1400 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "23",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120453
catch[catch$LICENCE_ID==120453 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120453 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120457
catch[catch$LICENCE_ID==120457 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-01-29 23:00:00", "FV_DATE_FISHED"] <- "2021-05-30"
catch[catch$LICENCE_ID==120457 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-30", "MONTH"] <- "05"

#120473
catch[catch$LICENCE_ID==120473 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120473 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120477
catch[catch$LICENCE_ID==120477 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120477 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120479
catch[catch$LICENCE_ID==120479 & catch$YEAR==2021, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120479 & catch$YEAR==2021, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120479 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120490 GOOD EXAMPLE OF REMOVING DUPLICATES
catch[catch$LICENCE_ID==120490 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120490 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP STAND"
temp <- catch[catch$LICENCE_ID==120490 & catch$YEAR==2021, ]
temp <- temp %>% distinct(FV_DATE_FISHED, .keep_all = TRUE)
catch <- subset(catch, !(LICENCE_ID==120490 & YEAR==2021))
catch <- bind_rows(catch, temp)

#120495
catch[catch$LICENCE_ID==120495 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 70L
catch[catch$LICENCE_ID==120495 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP NET"

#120514
catch[catch$LICENCE_ID==120514 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120514 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120529
catch[catch$LICENCE_ID==120529 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120529 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120532
catch[catch$LICENCE_ID==120532 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 70L
catch[catch$LICENCE_ID==120532 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP NET"

#120534
catch[catch$LICENCE_ID==120534 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120534 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120535
catch[catch$LICENCE_ID==120535 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120535 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120539
catch[catch$LICENCE_ID==120539 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 41L
catch[catch$LICENCE_ID==120539 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "GILL NET (SET OR FIXED)"

#120548
catch[catch$LICENCE_ID==120548 & catch$YEAR==2021 & catch$FV_WEIGHT==24.24, "FV_DATE_FISHED"] <- "2021-04-10"
catch[catch$LICENCE_ID==120548 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-10", "DAY"] <- "10"
catch[catch$LICENCE_ID==120548 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 70L
catch[catch$LICENCE_ID==120548 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP NET"

#120553
catch[catch$LICENCE_ID==120553 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-26", "FV_DATE_FISHED"] <- "2021-06-26"
catch[catch$LICENCE_ID==120553 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-06-26", "MONTH"] <- "06"

#120567
catch[catch$LICENCE_ID==120567 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-26", "FV_DATE_FISHED"] <- "2021-05-27"
catch[catch$LICENCE_ID==120567 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-27", "DAY"] <- "27"

temp <- data.frame(
  LICENCE_ID = 120567,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 12,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-05-26",
  FV_WEIGHT = 5400,
  MEASUREMENT_UNIT = "POUNDS",
  MONTH = "05",
  DAY = "26",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "LAHAVE",
  COUNTY = "LUNENBURG COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120604
catch[catch$LICENCE_ID==120604 & catch$YEAR==2021, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120604 & catch$YEAR==2021, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120604 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120608
temp <- data.frame(
  LICENCE_ID = 120608,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = NA,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-05-07",
  FV_WEIGHT = 24,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "07",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

catch[catch$LICENCE_ID==120608 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-06-09", "FV_DATE_FISHED"] <- "2021-05-09"
catch[catch$LICENCE_ID==120567 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-09", "MONTH"] <- "05"

#120617
catch[catch$LICENCE_ID==120617 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-13", "FV_WEIGHT"] <- 240

temp <- data.frame(
  LICENCE_ID = 120617,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 14,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-05-07",
  FV_WEIGHT = 4400 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "07",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

temp <- data.frame(
  LICENCE_ID = 120617,
  RIVERNAME_LOGBOOK = "",
  YEAR = 2021,
  FV_HOURS_FISHED = 14,
  FV_GEAR_CODE = 70L,
  GEAR_DESCRIPTION = "DIP NET",
  FV_DATE_FISHED = "2021-05-14",
  FV_WEIGHT = 5060 * conv,
  MEASUREMENT_UNIT = "KILOGRAMS",
  MONTH = "05",
  DAY = "14",
  FV_SSF_SPECIES_CODE = 350L,
  LICENCE_TYPE = "NON-VESSEL BASED LIMITED",
  FV_CATCH_USAGE_CODE = NA,
  RIVERNAME_CLEANED = "TUSKET",
  COUNTY = "YARMOUTH COUNTY",
  PROVINCE = "NS",
  NOTES = ""
)
catch <- rbind(catch, temp)

#120618
catch[catch$LICENCE_ID==120618 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-26", "FV_DATE_FISHED"] <- "2021-04-28"
catch[catch$LICENCE_ID==120618 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-28" & catch$DAY=="26", "DAY"] <- "28"
catch[catch$LICENCE_ID==120618 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-28" & catch$FV_WEIGHT==120, "FV_DATE_FISHED"] <- "2021-04-26"
catch[catch$LICENCE_ID==120618 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-26" & catch$DAY=="28", "DAY"] <- "26"

#120626
catch[catch$LICENCE_ID==120626 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120626 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120629
catch[catch$LICENCE_ID==120629 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-04", "FV_WEIGHT"] <- 2400

#120644
catch[catch$LICENCE_ID==120644 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "POUNDS"

#120654
catch[catch$LICENCE_ID==120654 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120667
catch[catch$LICENCE_ID==120667 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-04", "FV_DATE_FISHED"] <- "2021-05-14"
catch[catch$LICENCE_ID==120667 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120669
catch[catch$LICENCE_ID==120669 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-11", "FV_DATE_FISHED"] <- "2021-04-11"
catch[catch$LICENCE_ID==120669 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-11", "MONTH"] <- "05"

#120679
catch[catch$LICENCE_ID==120679 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120680
#05/13 ENTERED TWICE IN MARFIS AND BOTH ARE WRONG - WEIGHT SHOULD BE KGS, LIKE THE OTHER DATES
catch[catch$LICENCE_ID==120680 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-01-24 23:00:00", "FV_DATE_FISHED"] <- "2021-05-25"
catch[catch$LICENCE_ID==120680 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-25", "MONTH"] <- "05"
catch <- subset(catch, !(LICENCE_ID==120680 & YEAR==2021 & FV_DATE_FISHED=="2021-05-13" & FV_WEIGHT==270))
catch[catch$LICENCE_ID==120680 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120688
catch[catch$LICENCE_ID==120688 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-17", "FV_DATE_FISHED"] <- "2021-05-07"
catch[catch$LICENCE_ID==120688 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-07", "DAY"] <- "07"

#120707
catch[catch$LICENCE_ID==120707 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120717
catch[catch$LICENCE_ID==120717 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-14" & catch$FV_WEIGHT==128.88, "FV_DATE_FISHED"] <- "2021-05-15"
catch[catch$LICENCE_ID==120717 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-15", "DAY"] <- "15"

#120721
catch[catch$LICENCE_ID==120721 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-01-13 23:00:00", "FV_DATE_FISHED"] <- "2021-05-14"
catch[catch$LICENCE_ID==120721 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-01-14 23:00:00", "FV_DATE_FISHED"] <- "2021-05-15"
catch[catch$LICENCE_ID==120721 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-14", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120721 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-15", "MONTH"] <- "05"
catch[catch$LICENCE_ID==120721 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 70L
catch[catch$LICENCE_ID==120721 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP NET"

#120734
catch[catch$LICENCE_ID==120734 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-06-09" & catch$FV_WEIGHT==2200, "FV_DATE_FISHED"] <- "2021-06-08"
catch[catch$LICENCE_ID==120734 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-06-08", "DAY"] <- "08"

#120735
catch[catch$LICENCE_ID==120735 & catch$YEAR==2021, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120735 & catch$YEAR==2021, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120735 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120737
catch[catch$LICENCE_ID==120737 & catch$YEAR==2021, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120737 & catch$YEAR==2021, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120737 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120744
catch[catch$LICENCE_ID==120744 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 70L
catch[catch$LICENCE_ID==120744 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP NET"
catch[catch$LICENCE_ID==120744 & catch$YEAR==2021, "FV_WEIGHT"] <- catch[catch$LICENCE_ID==120744 & catch$YEAR==2021, "FV_WEIGHT"] * conv
catch[catch$LICENCE_ID==120744 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120750
catch[catch$LICENCE_ID==120750 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "POUNDS"
catch[catch$LICENCE_ID==120750 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 08L
catch[catch$LICENCE_ID==120750 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP STAND"

#120753
catch[catch$LICENCE_ID==120753 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#120755
catch[catch$LICENCE_ID==120755 & catch$YEAR==2021, "MEASUREMENT_UNIT"] <- "KILOGRAMS"

#322015
catch[catch$LICENCE_ID==322015 & catch$YEAR==2021, "FV_GEAR_CODE"] <- 70L
catch[catch$LICENCE_ID==322015 & catch$YEAR==2021, "GEAR_DESCRIPTION"] <- "DIP NET"
catch[catch$LICENCE_ID==322015 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-04-09" & catch$FV_WEIGHT==216, "FV_DATE_FISHED"] <- "2021-05-09"
catch[catch$LICENCE_ID==322015 & catch$YEAR==2021 & catch$FV_DATE_FISHED=="2021-05-09", "MOTNH"] <- "05"

# 2022 #########################################################################
}