#===============================================================================
# S. Fulton - January 2022
#
# This script is designed to upload the following data to the GASPEREA database:
#
# 1. Biological data
# 2. Age data
# 3. Count data
#
# Each site will have to be uploaded separately:
#
#===============================================================================
#
# First, define the filenames for each of the three csv and set the WD to 
# where they are all stored.

filename.biodata<-"TUSKET_2022_VAUGHAN_bio.csv"
filename.agedata<-"GASPEREAU_2022_age.csv"
filename.countdata<-"TUSKET_2022_POWERHOUSE_count.csv"

setwd(choose.dir(caption = "Navigate to Desired WORKING DIRECTORY"))
#===============================================================================
#
# Load functions if not already loaded.
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()

#-------------------------------------------------------------------------------
# If needed, add the speciesID only fish to the bio characteristics file:

speciesID.expand(data=read.csv(file.choose(),header=T,stringsAsFactors = F),
                 filename.biodata, "Tusket", 2022)

# Double check for data entry errors in BIO and AGE data files:

# Use checker's function on Biological Data. Fix any errors in the csv 
# directly before moving on.
checkers(filename.biodata)

#Check over age data, make sure it all looks right.
age.check(filename.agedata)


#-------------------------------------------------------------------------------
# Once everything looks good, proceed with uploading to the GASPEREA database
#-------------------------------------------------------------------------------

#Load ROracle
require("ROracle") 

#-------------------------------------------------------------------------------
#Set account name, password, and server
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, 
                  oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 
#-------------------------------------------------------------------------------
# Format count data
oracle.count<-format.COUNTDATA.onesite(filename.countdata)

#oracle.count<-oracle.count[oracle.count$COUNT_ID>1375,]

# If you are happy with it, proceed to uploading it the GASPERA database
dbWriteTable(conn = channel, schema="GASPEREA", 
             value = oracle.count,
             name = "ALOSA_VIDEO_COUNT_DATA", 
             date=TRUE,
             row.names = FALSE, 
             overwrite = FALSE, ## NEVER CHANGE TO TRUE
             append = TRUE)

#-------------------------------------------------------------------------------
# Format biological data
oracle.bio<-format.BIODATA.onesite(filename.biodata)

oracle.bio<-oracle.bio[oracle.bio$FISH_ID>1338,]

# If you are happy with it, proceed to uploading it the GASPERA database
dbWriteTable(conn = channel, schema="GASPEREA", 
             value = oracle.bio,
             name = "ALOSA_FISH_BIO_DATA", 
             date=TRUE,
             row.names = FALSE, 
             overwrite = FALSE, ## NEVER CHANGE TO TRUE
             append = TRUE)

#-------------------------------------------------------------------------------
# Format age data
oracle.age<-format.AGE.onesite(filename.agedata)

# If you are happy with it, proceed to uploading it the GASPERA database
dbWriteTable(conn = channel, schema="GASPEREA", 
             value = oracle.age,
             name = "ALOSA_FISH_AGE_DATA", 
             date=TRUE,
             row.names = FALSE, 
             overwrite = FALSE, ## NEVER CHANGE TO TRUE
             append = TRUE)
#===============================================================================
# Common errors you may encounter:
# ---------------------------------
# ORA-00001: unique constraint (constraint name) violated
# 
# The database was set up to only allow unique combinations of 
# sample number, location , and year (FISH_ID;SITE_ID;YEAR)
# If the data was already up loaded, you will get this response.
#
# Sometimes an upload encounters an error for another reason partway through
# writing the data to Oracle. You have to find out which data was successful
# written to the database in order to remove them from the dataframe you are 
# trying to upload. 

# You can pull all data from the year you are uploading using the code below:
# (Modify table name and year as needed)
#X<-dbGetQuery(channel,"SELECT * FROM GASPEREA.ALOSA_VIDEO_COUNT_DATA 
#               WHERE ALOSA_VIDEO_COUNT_DATA.YEAR = 2022")

# Then compare the data that exits in the database to what you are 
# trying to add.
#----------------------------------
# ORA-02291: integrity constraint (constraint name) violated - description
#
# The table you are trying to write has a column where the data entered is
# dependent on another table and the entry doesn't exist.
#
# For example, say you enter the site number as 30 but site '30' does not 
# exist in the SITE_DESC table. This would cause an integrity error because
# the site MUST be defined before you can add it to another table.
#-----------------------------------
# ORA-00054: resource busy and acquire with NOWAIT specified or timeout expired
# This is an error you would get on SQL developer if you are trying to 
# modify tables while you have an open connection through R.

#dbDisconnect(channel)

# Run the above code to disconnect.
#-----------------------------------
# ORA-01722: invalid number

## Usually a column that should be numeric is saved as a character due
## to a typo somewhere in the csv file.
#-----------------------------------

# Do you wan to add lines or modify entries to an existing table such as 
# SITE_DESC or RIVER_DESC?
#
## X<-dbGetQuery(channel,"SELECT * FROM TABLENAME")
##

#-------------------------------------------------------------------------------