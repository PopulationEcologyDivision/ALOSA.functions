#
#Description:
#
# Inputs:

#
# This function is used by:
# 

# This functions uses:


get.count.data<-function(year, siteID, channel){
  sql <- "SELECT   
                          ALOSA_VIDEO_COUNT_DATA.YEAR,
                          ALOSA_VIDEO_COUNT_DATA.MON,
                          ALOSA_VIDEO_COUNT_DATA.DAY,
                          ALOSA_VIDEO_COUNT_DATA.STRATA,
                          ALOSA_VIDEO_COUNT_DATA.TIME,
                          ALOSA_VIDEO_COUNT_DATA.COUNT_UP,
                          ALOSA_VIDEO_COUNT_DATA.COUNT_DOWN,
                          ALOSA_VIDEO_COUNT_DATA.MINUTES,
                          ALOSA_VIDEO_COUNT_DATA.SECONDS,
                          ALOSA_SITE_DESC.DESC_ENG as SITE_NAME
                          
                          FROM GASPEREA.ALOSA_VIDEO_COUNT_DATA
                          LEFT JOIN GASPEREA.ALOSA_SITE_DESC 
                          ON GASPEREA.ALOSA_SITE_DESC.SITE_ID=
                             GASPEREA.ALOSA_VIDEO_COUNT_DATA.SITE_ID
                          
                          WHERE ALOSA_VIDEO_COUNT_DATA.SITE_ID = ?site AND
                                ALOSA_VIDEO_COUNT_DATA.YEAR = ?year"
  
  query.statement=sqlInterpolate(ANSI(), sql, year = year, site=siteID)
  test=dbGetQuery(channel,query.statement)
  return(test)
}