#
#Description:
#
# Inputs:

#
# This function is used by:
# 

# This functions uses:



i.forgot.the.siteIDs<-function(channel){
  X<-dbGetQuery(channel,"
    SELECT
    ALOSA_SITE_DESC.SITE_ID,
    ALOSA_SITE_DESC.DESC_ENG as sitename,
    ALOSA_RIVER_DESC.DESC_ENG as rivername
    
    FROM GASPEREA.ALOSA_SITE_DESC
    
    LEFT JOIN GASPEREA.ALOSA_RIVER_DESC
    ON GASPEREA.ALOSA_RIVER_DESC.RIVER_ID = GASPEREA.ALOSA_SITE_DESC.RIVER_ID")
  return(X)
}