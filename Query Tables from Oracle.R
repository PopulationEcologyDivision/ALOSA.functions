#===============================================================================
# S. Fulton - January 2022
#
# This script is designed to pull data from the GASPEREA database:
#
# 
# Each site will have to be uploaded separately:
#
#===============================================================================
setwd(choose.dir(caption = "Navigate to Desired WORKING DIRECTORY"))
#===============================================================================
#
require(ROracle)
## Set up Environment with all the functions in it:
#repeat until 'invalid 'name' argument error
detach(myfunctions)
#Attach environment
env <- attach(NULL, name = "myfunctions")
source("//ent.dfo-mpo.ca/atlshares/Science/Population Ecology Division/DFD/Alosa/functions.R",local=env)
#-------------------------------------------------------------------------------
#Set account name, password, and server
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 
#-------------------------------------------------------------------------------
# Forgot the Site_ID numbers? No worries, make yourself a reference.
siteID_ref=i.forgot.the.siteIDs(channel)

count.data=get.count.data(2021,c(2,3),channel)


siteIDs=2
years=2021

get.count.data<-function(years, siteIDs, channel){
  
test=dbGetQuery(channel,"SELECT   
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
                          
                          WHERE ALOSA_VIDEO_COUNT_DATA.SITE_ID = :d AND
                                ALOSA_VIDEO_COUNT_DATA.YEAR = 2021",
                
                data=c(2,3))
dynamic.count.query="SELECT   
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
                          
                          WHERE ALOSA_VIDEO_COUNT_DATA.SITE_ID = :d AND
                                ALOSA_VIDEO_COUNT_DATA.YEAR = :a" 

dynamic.query.statement <- function(x,statement) {
  dbGetQuery(con, statement, data = x)
}

ldply(yourdatalist, thisfinallyworks)


test=dbGetQuery(channel,query.statement,params=list(sites=siteIDs,years=years))


  return(test)
}



test <- dbSendStatement(channel, "SELECT * FROM ALOSA_FISH_AGE_DATA 
                                  WHERE ALOSA_FISH_AGE_DATA.SITE_ID = :a")

dbBind(test, la=c("2", "3")))
dbGetRowsAffected(test)
dbClearResult(test)


site <-c("2", "3")
placeholders = paste(rep('?a', length(site)), collapse = ', ')


sql <- sqlInterpolate(channel, 
                      paste0("SELECT * FROM ALOSA_FISH_AGE_DATA 
                                  WHERE ALOSA_FISH_AGE_DATA.SITE_ID IN (",placeholders,")"), 
                      a = site
)
sql

sqlInterpolate(channel,)

dbGetQuery(channel,sql)




site <-c("H'); DROP TABLE--;","3")
#site <-"H'); DROP TABLE--;"


placeholder = paste0(rep("?", length(site)), collapse = ', ')
i=1:length(site)
placeholder=paste0("?site",i,collapse = ', ')

sql <- sqlInterpolate(ANSI(), 
                      sprintf("SELECT * FROM ALOSA_FISH_AGE_DATA 
                                  WHERE ALOSA_FISH_AGE_DATA.SITE_ID IN (%s)",placeholder), 
                      site1=site[1],site2=site[2]
)
sql


SQL(site)




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
                                
x=sqlInterpolate(ANSI(), sql, year = year, site=siteID)



siteID=3
year=2021








# 
dbListFields(channel,'ALOSA_RIVER_DESC', "GASPEREA")
dbListTables(channel,"GASPEREA")


get.count.data<-function(years, siteIDs, channel){
  nyears=length(years)
  if(nyears==1){
    years.statement=years
  }
  if(nyears>1){
    temp=paste(years[1],sep="")
    for (i in 2:nyears){
       temp=paste(temp,",",years[i],sep="")
      }
   years.statement=paste("(",temp,")",sep="")
  }
  nsites=length(siteIDs)
  if(nsites==1){
    siteID.statement=siteID
  }
  if(nsites>1){
    temp=paste(siteIDs[1],sep="")
    for (i in 2:nsites){
      temp=paste(temp,",",siteIDs[i],sep="")
    }
    siteID.statement=paste("(",temp,")",sep="")
  }
  query.statement= paste("SELECT   
                          ALOSA_VIDEO_COUNT_DATA.YEAR,
                          ALOSA_VIDEO_COUNT_DATA.MON,
                          ALOSA_VIDEO_COUNT_DATA.DAY,
                          ALOSA_VIDEO_COUNT_DATA.STRATA,
                          ALOSA_VIDEO_COUNT_DATA.TIME,
                          ALOSA_VIDEO_COUNT_DATA.COUNT_UP,
                          ALOSA_VIDEO_COUNT_DATA.COUNT_DOWN,
                          ALOSA_SITE_DESC.DESC_ENG as SITE_NAME
                          
                          FROM GASPEREA.ALOSA_VIDEO_COUNT_DATA
                          LEFT JOIN GASPEREA.ALOSA_SITE_DESC 
                          ON GASPEREA.ALOSA_SITE_DESC.SITE_ID=
                             GASPEREA.ALOSA_VIDEO_COUNT_DATA.SITE_ID
                          
                          WHERE ALOSA_VIDEO_COUNT_DATA.SITE_ID IN", 
                                siteID.statement, "AND
                                ALOSA_VIDEO_COUNT_DATA.YEAR IN", 
                                years.statement,sep=" ")
  test=dbGetQuery(channel,query.statement)
  return(test)
}

siteIDs=c(3,2,1)
years=c(2020,2021,2019)
t=count.data(years,siteIDs,channel)


siteID=
years=c(2021,2020,2019)
years.statement=paste("(",years,")",sep="")
query.statement= paste("SELECT   
ALOSA_VIDEO_COUNT_DATA.YEAR,
ALOSA_VIDEO_COUNT_DATA.MON,
ALOSA_VIDEO_COUNT_DATA.DAY,
ALOSA_VIDEO_COUNT_DATA.STRATA,
ALOSA_VIDEO_COUNT_DATA.TIME,
ALOSA_VIDEO_COUNT_DATA.COUNT_UP,
ALOSA_VIDEO_COUNT_DATA.COUNT_DOWN


FROM GASPEREA.ALOSA_VIDEO_COUNT_DATA

WHERE ALOSA_VIDEO_COUNT_DATA.SITE_ID =", siteID, "AND
      ALOSA_VIDEO_COUNT_DATA.YEAR IN", years.statement,sep=" ")



rs <- dbSendQuery(channel, "select * from ALOSA_COUNTY_DESC")
data <- fetch(rs)


agedata.B=dbGetQuery(channel, "SELECT * FROM ALOSA_FISH_AGE_DATA
                   LEFT JOIN ALOSA_FISH_BIO_DATA ON
                      ALOSA_FISH_BIO_DATA.FISH_ID= ALOSA_FISH_AGE_DATA.FISH_ID AND
                      ALOSA_FISH_BIO_DATA.SITE_ID= ALOSA_FISH_AGE_DATA.SITE_ID AND
                      ALOSA_FISH_BIO_DATA.YEAR= ALOSA_FISH_AGE_DATA.YEAR
                   
                   WHERE ALOSA_FISH_BIO_DATA.SITE_ID=2 AND
                         ALOSA_FISH_BIO_DATA.YEAR=2021 AND
                         ALOSA_FISH_BIO_DATA.SPECIES_ID=3502")


biodata=dbGetQuery(channel, "SELECT * FROM ALOSA_FISH_BIO_DATA
                              LEFT JOIN  ALOSA_FISH_AGE_DATA ON
                                  ALOSA_FISH_AGE_DATA.FISH_ID = ALOSA_FISH_BIO_DATA.FISH_ID  AND
                                  ALOSA_FISH_AGE_DATA.SITE_ID = ALOSA_FISH_BIO_DATA.SITE_ID  AND
                                  ALOSA_FISH_AGE_DATA.YEAR = ALOSA_FISH_BIO_DATA.YEAR 
                   
                             WHERE ALOSA_FISH_BIO_DATA.SITE_ID=2 AND
                                 ALOSA_FISH_BIO_DATA.YEAR=2021")



GASP.age=dbGetQuery(channel, "SELECT 
                        ALOSA_FISH_AGE_DATA.CURRENT_AGE
                        ALOSA_FISH_AGE_DATA.CURRENT_AGE - ALOSA_FISH_AGE_DATA.AGE_AT_FIRST_SPAWN as PREVIOUS_SPAWNS
                        ALOSA_FISH_AGE_DATA.YEAR
                        ALOSA_SEX_DESC.DESC_SHORT as SEX
                   
                   FROM ALOSA_FISH_AGE_DATA
                   LEFT JOIN ALOSA_FISH_BIO_DATA ON
                      ALOSA_FISH_BIO_DATA.FISH_ID= ALOSA_FISH_AGE_DATA.FISH_ID AND
                      ALOSA_FISH_BIO_DATA.SITE_ID= ALOSA_FISH_AGE_DATA.SITE_ID AND
                      ALOSA_FISH_BIO_DATA.YEAR= ALOSA_FISH_AGE_DATA.YEAR
                   
                   WHERE ALOSA_FISH_BIO_DATA.SITE_ID=2 AND
                         ALOSA_FISH_BIO_DATA.YEAR=2021 AND
                         ALOSA_FISH_BIO_DATA.SPECIES_ID=3502")
