#
# Description: MARFIS Cleaner script
# takes the catch data frame, and manually corrects all errors identified 
# by checking the logbooks
# all errors are wrapped in an if statement, that checks if the pre-existing
# issue is still there. if it has been changed in the data base, the correction
# will not be applied and a warning will be printed so that the correction can 
# be deleted.
# Inputs: catch data frame from pulling from the MARFIS database

#
# This function is used by: MARFIS_all in one.R
# 

# Load in a pulled dataframe
load("C:/Users/graylo/Documents/GitHub/data/marfis-pull-2024-03-21.Rdata")

# Create a temporary data frame of catch just for testing the code so that 
# we don't commit any changes to the actual pulled data frame
#catch <- catch

# conversion factor from count of fish to kilograms
conv <- 0.240

# This functions uses:

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
new_row <- data.frame(
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
catch <- rbind(catch, new_row)

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
dupes <- catch[catch$LICENCE_ID==120260 & catch$YEAR==2019, ]
dupes_removed <- dupes[!duplicated(dupes[c("MONTH","DAY")]), ]
catch <- catch[!(catch$LICENCE_ID==120260 & catch$YEAR==2019), ]
catch <- rbind(catch, dupes_removed)

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
catch[catch$LICENCE_ID==120663 & catch$YEAR==2019 & catch$FV_DATE_FISHED=="2019-06-11", "RIVERNAME_CLEANED"] <- "LAHAVE"
catch[catch$LICENCE_ID==120663 & catch$YEAR==2019 & catch$RIVERNAME_CLEANED=="MAHONE BAY", "RIVERNAME_CLEANED"] <- "MARTINS RIVER"

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
