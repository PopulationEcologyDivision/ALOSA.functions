#
#Description:
#
# Inputs:

#
# This function is used by:
# 

# This functions uses:



format.AGE.onesite<-function(filename){
  data<-read.csv(filename,header=T,stringsAsFactors = F)
  goodnames<-(c("year","sample","current.age","age.at.first.spawn","notes","age.structure.sample")) 
  missingnames=goodnames[!goodnames%in%(names(data))]
  if (length(missingnames>0)){
    cat("Missing column name(s):","\n", missingnames,"\n")
    stop("Please fix column names before continuing")
  }
  
  siteID=as.numeric(readline(prompt= 
                               "Please enter site number \n If site number is unknown enter 0"))
  if(siteID==0){
    x <- as.numeric(readline('Which River? \n 1. "GASPEREAU" 
                           \n 2. "TUSKET" 
                           \n 3. "MERSEY"
                           \n 4. "MEDWAY"
                           \n 5. "SHUBIE" 
                           \n 0. "NONE OF THESE"\n'))
    if (x==0){
      stop(" Go find site ID from database")
    }
    if (x==1){
      siteID=as.numeric(readline('Which Site? \n 3. "WHITE ROCK" 
                           \n 4. "LANES MILL" \n'))
    }
    if (x==2){
      siteID=as.numeric(readline('Which Site? \n 1. "CARLETON" 
                           \n 2. "VAUGHAN" \n'))
    }
    if (x==3){
      siteID=as.numeric(readline('Which Site? \n 5. "ROLL DAM" 
                           \n 6. "COWIE FALLS" 
                           \n 7. "DEEP BROOK"
                           \n 8. "LOWER GREAT BROOK"
                           \n 9. "BIG FALLS"
                           \n 10. "LOWER LAKE FALLS"
                           \n 11. "UPPER LAKE FALLS" \n'))
    }
    if (x==4){
      siteID=as.numeric(readline('Which Site? \n 12. "SITE 1"\n'))
    }
    if (x==5){
      siteID=as.numeric(readline('Which Site? \n 13. "BASS TRAP" \n'))
    }
  }
  
  #Fix up data type and add missing columns
  #data<-data[!(is.na(data$count.upstream)),]
  data<-data[,names(data)%in%goodnames]
  data$SITE_ID<-siteID
  
  col_order <- c("sample", "SITE_ID", "year",
                 "current.age","age.at.first.spawn","notes","age.structure.sample")
  
  data <- data[, col_order]
  names(data) <- c("FISH_ID", "SITE_ID", "YEAR",
                   "CURRENT_AGE","AGE_AT_FIRST_SPAWN","NOTES","AGE_STRUCTURE_SAMPLE")
  data<- data[complete.cases(data[,c("CURRENT_AGE","AGE_AT_FIRST_SPAWN")]),]
  
  return(data)
}