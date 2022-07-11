#===============================================================================
# S. Fulton - February 2022
#
# This script is designed to update tables from the GASPEREA database:
#

#
#===============================================================================
setwd(choose.dir(caption = "Navigate to Desired WORKING DIRECTORY"))
#===============================================================================
#
require(dplyr)
require(ROracle)

##NOTE: the functions script must always be attached after package. If the
##      'myfunctions' object is not in the 2nd position in the search path
##      it won't be able to use the functions from the packages loaded 
##      after it!


#Make sure the 'git' folder is in your home directory. You can find out what
# your home directory is by running:
# path.expand('~')

source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()
#---
#Set account name, password, and server
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, 
                  oracle.password.GASP, "PTRAN", believeNRows=FALSE) 

#---///---///---///---///---///---///---///---///---///---///---///---///---///

#Update an existing cell in an existing table:
#
# The WHERE part of the query is super important. It is where you set the
# conditions on which rows to update.

# Structure of an update query
# "UPDATE  [schemaname.tablename]
#  SET     [schemaname.tablename.columnname = newdata value]
#  WHERE   [schemaname.tablename.columnname = condition 1] AND
#          [schemaname.tablename.columnname = condition 2] AND
#          [schemaname.tablename.columnname = condition 3]"

query="UPDATE  ALOSA_VIDEO_COUNT_DATA
       SET   ALOSA_VIDEO_COUNT_DATA.COUNT_DOWN=0
       WHERE ALOSA_VIDEO_COUNT_DATA.COUNT_ID=747 AND
       ALOSA_VIDEO_COUNT_DATA.SITE_ID=3 AND
       ALOSA_VIDEO_COUNT_DATA.YEAR=2021 AND
       ALOSA_VIDEO_COUNT_DATA.TIME=2020"

query="UPDATE  ALOSA_FISH_AGE_DATA
       SET   ALOSA_FISH_AGE_DATA.AGE_STRUCTURE_SAMPLE='Y'
       WHERE ALOSA_FISH_AGE_DATA.YEAR=2021"
# Execute query
dbExecute(channel,query)
# If all is good, commit the changes to the database to save them
dbCommit(channel)
#If it is not good, you can roll them back
# dbRollback(channel)
#
#---///---///---///---///---///---///---///---///---///---///---///---///---///

#Update multiple cells in an existing table:
#
# The WHERE part of the query is super important. It is where you set the
# conditions on which rows to update.

# Structure of an update query
# "UPDATE  [schemaname.tablename]
#  SET     [schemaname.tablename.columnname = newdata value]
#  WHERE   [schemaname.tablename.columnname = condition 1] AND
#          [schemaname.tablename.columnname = condition 2] AND
#          [schemaname.tablename.columnname = condition 3]"

query="UPDATE  ALOSA_VIDEO_COUNT_DATA
       SET   ALOSA_VIDEO_COUNT_DATA.COUNT_DOWN=0
       WHERE ALOSA_VIDEO_COUNT_DATA.COUNT_ID=747 AND
       ALOSA_VIDEO_COUNT_DATA.SITE_ID=3 AND
       ALOSA_VIDEO_COUNT_DATA.YEAR=2021 AND
       ALOSA_VIDEO_COUNT_DATA.TIME=2020"

# Execute query
dbExecute(channel,query)
# If all is good, commit the changes to the database to save them
dbCommit(channel)
#If it is not good, you can roll them back
# dbRollback(channel)
#

#---///---///---///---///---///---///---///---///---///---///---///---///---///

# Add a row to an existing table:

#---///---///---///---///---///---///---///---///---///---///---///---///---///

#---///---///---///---///---///---///---///---///---///---///---///---///---///

#---///---///---///---///---///---///---///---///---///---///---///---///---///


bio.in.oracle=dbGetQuery(channel, "SELECT 
                                   ALOSA_FISH_AGE_DATA.FISH_ID,
                                   ALOSA_FISH_AGE_DATA.SITE_ID,
                                   ALOSA_FISH_AGE_DATA.YEAR

                                  FROM ALOSA_FISH_AGE_DATA
                                  LEFT JOIN ALOSA_FISH_BIO_DATA 
                                   ON ALOSA_FISH_BIO_DATA.FISH_ID=ALOSA_FISH_AGE_DATA.FISH_ID
                                      AND ALOSA_FISH_BIO_DATA.SITE_ID=ALOSA_FISH_AGE_DATA.SITE_ID
                                      AND ALOSA_FISH_BIO_DATA.YEAR=ALOSA_FISH_AGE_DATA.YEAR
                                  WHERE ALOSA_FISH_BIO_DATA.YEAR=2021")

bio.in.oracle=dbGetQuery(channel, "SELECT * FROM ALOSA_FISH_AGE_DATA")
                                 
newdata=anti_join(oracle.age,bio.in.oracle[,c("FISH_ID","SITE_ID","YEAR")])

oracle.age=newdata

dbExecute(channel,"UPDATE  ALOSA_RIVER_DESC
                     SET   ALOSA_RIVER_DESC.SYSTEM_DESC='HYDRO DEV'
                     WHERE ALOSA_RIVER_DESC.RIVER_ID=31 AND
                           ALOSA_RIVER_DESC.COUNTY_ID=11")

dbExecute(channel,"UPDATE  ALOSA_VIDEO_COUNT_DATA
                     SET   ALOSA_VIDEO_COUNT_DATA.COUNT_DOWN=0
                     WHERE ALOSA_VIDEO_COUNT_DATA.COUNT_ID=747 AND
                           ALOSA_VIDEO_COUNT_DATA.SITE_ID=3 AND
                           ALOSA_VIDEO_COUNT_DATA.YEAR=2021 AND
                           ALOSA_VIDEO_COUNT_DATA.TIME=2020")
dbCommit(channel)

x=dbGetQuery(channel, "SELECT * FROM ALOSA_RIVER_DESC")
