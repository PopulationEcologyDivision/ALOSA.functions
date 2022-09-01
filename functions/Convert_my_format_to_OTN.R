# #convert tag metadata format to OTN submission format
# require (dplyr)
# require (lubridate)
# mytags <- read.csv('R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2021/Acoustic Tracking/Tusket_2021_acoustic_tag_fish_metadata.csv')
# # otntemp <- read.csv('C:/Users/naug/Documents/Acoustic/OTN_template.csv')
# 
# #rename some of the common columns
# mytags <- mytags %>%
#   rename(
#     TAG_ID_CODE = ID,
#     TAG_SERIAL_NUMBER = SN,
#     COMMON_NAME_E = SP,
#     SEX = SX,
#     LENGTH..m.= FL,
#     WEIGHT..kg.= WT,
#     COMMENTS = NOTES
#   )
# 
# #fill out common name
# mytags$COMMON_NAME_E <- as.character(mytags$COMMON_NAME_E)
# mytags$COMMON_NAME_E[mytags$COMMON_NAME_E == 'A'] <- "Alewife"
# mytags$COMMON_NAME_E[mytags$COMMON_NAME_E == 'BB'] <- "Blueback Herring"
# 
# #fill out scientific name
# mytags$SCIENTIFIC_NAME <- NA
# mytags$SCIENTIFIC_NAME[mytags$COMMON_NAME_E == "Alewife"] <- "Alosa pseudoharengus" 
# mytags$SCIENTIFIC_NAME[mytags$COMMON_NAME_E == "Blueback Herring"] <- "Alosa aestivalis"
# 
# #change weights to kg; lengths to m
# mytags$WEIGHT..kg. <- mytags$WEIGHT..kg/1000
# mytags$LENGTH..m. <- mytags$LENGTH..m./100
# 
# #DATETIME SHIT
# #release
# mytags$UTC_RELEASE_DATE_TIME <- NA
# mytags$dt <- ymd_hms(paste(mytags$YR, '-', mytags$MR, '-', mytags$DR, ' ',mytags$HOURR, ':',
#                   mytags$MINR, ':', '00'), tz = "Canada/Atlantic")
# mytags$dt <- with_tz(mytags$dt, "UTC")
# mytags$UTC_RELEASE_DATE_TIME <- gsub(x=mytags$dt, pattern=" ", replacement="T", fixed=T)
# #surgery
# mytags$DATE_OF_SURGERY <- ymd(paste(mytags$YT, ':',mytags$MT, ':', mytags$DT), tz = "Canada/Atlantic")
# 
# #postop hold period
# mytags$POSTOP_HOLD_PERIOD <- 24
# 
# #include TAG_CODE_SPACE
# mytags$TAG_CODE_SPACE <- paste('A180-1702-', mytags$TAG_ID_CODE)
# 
# #Tag implant type
# mytags$TAG_IMPLANT_TYPE <- "EXTERNAL"
# 
# #Estimated tag life
# mytags$EST_TAG_LIFE <- 96
# 
# #length measurement type
# mytags$LENGTH_TYPE <- 'FORK'
# 
# #AGE
# mytags$AGE <- "Unknown"
# 
# #release location
# mytags$RELEASE_LOCATION <- "Vaughan Ladder"
# mytags$RELEASE_LATITUDE <- 43.886916
# mytags$RELEASE_LONGITUDE <--65.969246
# 
# #life stage
# mytags$LIFE_STAGE <- "Adult"
# 
# ##join my metadata with OTN template
# 
# otntemplate <- names(otntemp)
# mytags_otn <- names(mytags)[names(mytags) %in% otntemplate]
# mytags <- select(mytags, mytags_otn)
# #dummy df to get it going
# otn_extra <- data.frame(col1 = 1)
# otn_extra[, otntemplate[!otntemplate %in% names(mytags)]] <- ""
# otn_extra <- select(otn_extra, -col1)
# 
# mytags <- cbind(mytags, otn_extra)
# otntags <- select(mytags, otntemplate)
# 
# write.csv(otntags, 'C:/Users/naug/Documents/Acoustic/OTN_format_2021_Tusket.csv', row.names = F)
# 
# 
