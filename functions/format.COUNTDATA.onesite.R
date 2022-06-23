#
#Description:
#
# Inputs:

#
# This function is used by:
# 

# This functions uses:



format.COUNTDATA.onesite<-function(filename){
  data<-read.csv(filename,header=T,stringsAsFactors = F)
  goodnames<-(c("year","mon","day","time","strata","count.upstream",
                "count.downstream","camera.desc","notes","minutes","seconds")) 
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
  
  # #data<-data[!(is.na(data$count.upstream)),]
  # if("minutes"%in% names(data)){
  #   goodnames=c(goodnames,"minutes","seconds")
  # }
  data<-data[,names(data)%in%goodnames] 
  
  
  
  data$COUNT_ID<-1:dim(data)[1]
  data$SITE_ID<-siteID
  
  for(i in 1:dim(data)[1]){
    if(is.na(data$count.upstream[i])& !is.na(data$count.downstream[i])){
      data$count.upstream[i]=0
    }
    if(!is.na(data$count.upstream[i])& is.na(data$count.downstream[i])){
      data$count.downstream[i]=0
    }
  }
  
  col_order <- c("COUNT_ID", "SITE_ID", "year", "mon", "day","camera.desc",
                 "strata","time","count.upstream","count.downstream","notes",
                 "minutes","seconds")
  data <- data[, col_order]
  names(data) <- c("COUNT_ID", "SITE_ID", "YEAR", "MON", "DAY","CAMERA_DESC",
                   "STRATA","TIME","COUNT_UP","COUNT_DOWN","NOTES",
                   "MINUTES","SECONDS") 
  
  data<- data[complete.cases(data[,c("COUNT_UP","COUNT_DOWN")]),]
  
  return(data)
}