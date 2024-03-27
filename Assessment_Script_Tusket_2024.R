#...............................................................................
#...............................................................................
#
# This script is designed to run all of the functions needed to calculate
#
# Escapement, run timing, and biological characteristics.
# 
# Each site and will have to be assessed separately:
#
#...............................................................................
#...............................................................................
require(ROracle)
#-------------------------------------------------------------------------------
#...............................................................................
#...............................................................................
#

source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()

#...............................................................................
#Set account name, password, and server
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 
#...............................................................................
#
#i.forgot.the.siteIDs(channel)
#...............................................................................
#### Season setup ####
#Only run at beginning of season!
blank.datasheets(seed=222,startmonth=3,endmonth=6,startday=15,rivername="Vaughan",
                 year=2024,recordtime=T,speciesID=T,strata=6,samplesperstrata=4)
blank.datasheets(seed=223,startmonth=3,endmonth=6,startday=15,rivername="Powerhouse",
                 year=2024,recordtime=T,speciesID=T,strata=6,samplesperstrata=4)
make.count.filename.textfile("Powerhouse 2024 count data.csv","Secret_Ladder",2024)
make.count.filename.textfile("Vaughan 2024 count data.csv","Vaughan",2024)
#...............................................................................