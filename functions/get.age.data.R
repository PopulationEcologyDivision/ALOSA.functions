#
#Description:
#
# Inputs:
#channel needs to be called
#
# This function is used by:
#All river assessment RMD files
# Catch curve function

# This functions uses:

get.age.data<-function(year, siteID, sppID, AgeStructure, channel){
 
  if(missing(AgeStructure)){AgeStructure<-T}
  
  sql <- "SELECT * FROM ALOSA_FISH_AGE_DATA
                   LEFT JOIN ALOSA_FISH_BIO_DATA ON
                      ALOSA_FISH_BIO_DATA.FISH_ID= ALOSA_FISH_AGE_DATA.FISH_ID AND
                      ALOSA_FISH_BIO_DATA.SITE_ID= ALOSA_FISH_AGE_DATA.SITE_ID AND
                      ALOSA_FISH_BIO_DATA.YEAR= ALOSA_FISH_AGE_DATA.YEAR
                   
                   WHERE ALOSA_FISH_BIO_DATA.SITE_ID=?site AND
                         ALOSA_FISH_BIO_DATA.YEAR=?year AND
                         ALOSA_FISH_BIO_DATA.SPECIES_ID=?spp"
  
  agequery=sqlInterpolate(ANSI(), sql, year = year, site=siteID, spp = sppID)
  
  agedata=dbGetQuery(channel, agequery)
  
  if(AgeStructure==T)
  {
    agedata=agedata[agedata$AGE_STRUCTURE_SAMPLE=="Y" |agedata$AGE_STRUCTURE_SAMPLE==1,]
  }

  return(agedata)
  
}

