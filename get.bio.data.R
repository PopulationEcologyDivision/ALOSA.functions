#
#Description:
#
# Inputs:

#
# This function is used by:
# 

# This functions uses:


get.bio.data<-function(year, siteID, sppID,channel){
   sql <- "SELECT * FROM ALOSA_FISH_BIO_DATA
                    
                    WHERE ALOSA_FISH_BIO_DATA.SITE_ID=?site AND
                    ALOSA_FISH_BIO_DATA.YEAR=?year AND
                    ALOSA_FISH_BIO_DATA.SPECIES_ID=?spp"

  
  query.statement=sqlInterpolate(ANSI(), sql, year = year, site=siteID, spp = sppID)
  test=dbGetQuery(channel,query.statement)
  return(test)
}