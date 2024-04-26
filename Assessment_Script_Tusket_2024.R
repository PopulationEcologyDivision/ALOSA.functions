#...............................................................................
# This script is designed to run all of the functions needed to calculate
# escapement, run timing, and biological characteristics.
# Each site and will have to be assessed separately:
#...............................................................................

require(ROracle)

source("~/git/ALOSA.functions/functions/sourcery.R")

sourcery()

#Set account name, password, and server
channel = dbConnect(
  DBI::dbDriver("Oracle"),
  oracle.username.GASP,
  oracle.password.GASP,
  "PTRAN",
  believeNRows = FALSE
  ) 

#i.forgot.the.siteIDs(channel)

# Season setup ####
# Only run at beginning of season!
#blank.datasheets(seed=222,startmonth=3,endmonth=6,startday=15,rivername="Vaughan",
#                 year=2024,recordtime=T,speciesID=T,strata=6,samplesperstrata=4)
#blank.datasheets(seed=223,startmonth=3,endmonth=6,startday=15,rivername="Powerhouse",
#                 year=2024,recordtime=T,speciesID=T,strata=6,samplesperstrata=4)
#make.count.filename.textfile("Powerhouse 2024 count data.csv","Secret_Ladder",2024)
#make.count.filename.textfile("Vaughan 2024 count data.csv","Vaughan",2024)

# In season ####
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2024")

# Make sure to save the xlsx sheet that we all use in the sharepoint to a csv.
# It is located in: OneDrive - DFO-MPO\Alosa\Counts\2024.
# I cannot figure out how to access the file path of stuff on sharepoint, so this
# is currently the easiest way I know to update the data

x <- onespecies.river.escapement(
  "Vaughan 2024 count data.csv",
  fixtime = T,
  downstream.migration = F,
  database = F,
  2024,
  2,
  channel
  )

x <- round(x)
n <- dim(x)[1]

# This takes off the latest day, helpful for when it is incomplete.
x <- x[1:n-1, ]

# x$dayofyear<-as.numeric(as.character(x$dayofyear))

print(paste0("Total escapement as of ", x$mon[n], "-", x$day[n], " is ", sum(x$total), sep = ""))

write.csv(x, file = "inseasonsummary.csv", row.names = F)