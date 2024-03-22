#
#Description: MARFIS Cleaner script
# takes the catch dataframe, and manually corrects all errors identifed 
# by checking the logbooks
# all errors are wrapped in an if statement, that checks if the pre-existing
# issue is still there. if it has been changed in the data base, the correction
# will not be applied and a warning will be printed so that the correction can 
# be deleted.
# Inputs: catch dataframe from pulling from the MARFIS database

#
# This function is used by: MARFIS_all in one.R
# 

# This functions uses:

####2019####
if(catch$FVWEIGHT[catch$LICENCEID==120000 & catch$YEAR==2019 & catch$RIVERNAME==TUSKET & catch$DAY=5]==100)
{catch$FVWEIGHT[catch$LICENCEID==120000 & catch$YEAR==2019 & catch$RIVERNAME==TUSKET & catch$DAY=5]<-200}  else 
{print("Original error for 120000 in 2019")}

####2020####

