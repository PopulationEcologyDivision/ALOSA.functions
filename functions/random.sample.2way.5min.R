#
#Description:
#
# Inputs:

#
# This function is used by:
# 

# This functions uses:



random.sample.2way.5min<-function(seed,startmonth,endmonth,startday,filename,
                                  year,recordtime=F,strata,samplesperstrata)
{
  path = getwd()
  set.seed(seed)
  ##2023 Tusket (Vaughan) seed is 112 and start date is March 1st
  ##2022 Gaspereau for WR seed is 741 and start date April. 15th
  ##2022 Gaspereau for LM seed is 789 and start date April. 15th
  ##2022 Tusket (Vaughan) seed is  987 and Start date April.1st
  ##2022 Tusket (Carleton) seed is 147 and Start date April.1st
  ##2022 Tusket (Powerhouse) seed is 417 and start date is April 29th
  ##2021 Gaspereau for WR seed is 543 and start date April. 15th
  ##2021 Gaspereau for LM seed is 163 and start date April. 15th
  ##2021 Tusket (Vaughan) seed is 654 and Start date April.1st
  ##2021 Tusket (Carleton) seed is 253 and Start date April.1st
  ##2020 COVID pandemic    
  ##2019 Tusket seed is 345, Gaspereau is 456
  ##2018 Tusket is 676
  ###2018 seed 123 ##2018 was extended from ending on June 21 to June 25, seed remained the same
  ###2017 seed 357 
  ###2016 seed 999
  ###2013 seed 889
  
  ###Tusket Counts
  ###2018 seed 676, 4 strata
  previousseeds=c(676,889,999,357,123,676,345,456,253,654,163,543)
  if (seed %in% previousseeds){
    stop("Seed has already been used (2013-2020). Pick a new one and try again")
  }
  #number of samples per strata
  num<-samplesperstrata
  
  ##check date inputs are ok
  if(startday<1 | startday>31){stop("startday must be from 1 to 31")}
  if(startmonth<1 | startmonth>12){stop("startmonth must be from 1 to 12")}
  if(endmonth<1 | endmonth>12){stop("endmonth must be from 1 to 12")}
  if(startmonth>=endmonth){stop("startmonth must be less than end month")}
  ##coerce inputs to integers
  startday<-as.integer(startday)
  startmonth<-as.integer(startmonth)
  endmonth<-as.integer(endmonth)
  
  #Time dimensions:
  mm<-startmonth:endmonth
  dd<-1:31
  hh<-0:23
  min5<-seq(0,55,by=5)
  
  #Make vectors for data.frame:
  t1<-rep(min5,length(hh))
  t2<-sort(rep(hh,length(min5)))
  t3<-t2*100+t1
  
  time<-rep(t3,length(dd)*length(mm))
  day<-rep(sort(rep(dd,length(t3))),length(mm))
  mon<-sort(rep(mm,length(dd)*length(t3)))

  #Make data frame and select dates:
  junk<-data.frame(mon,
                   day,
                   time,
                   year=year,
                   count.upstream=NA,
                   count.downstream=NA,
                   number.up=NA,
                   counter=NA,
                   notes=NA,
                   camera.desc=NA)
  
  
  if(strata<5 | strata>6)
  {
    stop("Not setup for this number of strata")
  }
  
  if(strata==5)
  {
    #Set up strata:
    #Strata 1 from 0000h to 0600h 
    #Strata 2 from 0600h to 1200h 
    #Strata 3 from 1200h to 1600h 
    #Strata 4 from 1600h to 2000h 
    #Strata 5 from 2000h to 2400h
    
    junk$strata<-ifelse(junk$time<600,1,
                        ifelse(junk$time>=600&junk$time<1200,2,
                               ifelse(junk$time>=1200&junk$time<1600,3,
                                      ifelse(junk$time>=1600&junk$time<2000,4,5))))
  }
  
  if(strata==6)
  {
    #Set up strata:
    #Strata 1 from 0000h to 0500h 
    #Strata 2 from 0500h to 1045h 
    #Strata 3 from 1045h to 1345h 
    #Strata 4 from 1345h to 1645h 
    #Strata 5 from 1645h to 2100h
    #Strata 6 from 2100h to 2400h
    
    #make new strata 
    junk$strata<-ifelse(junk$time>=0 & junk$time<500, 1,
                        ifelse(junk$time>=500 & junk$time<1045, 2,
                               ifelse(junk$time>=1045 & junk$time<1345, 3,
                                      ifelse(junk$time>=1345 & junk$time<1645, 4,
                                             ifelse(junk$time>=1645 & junk$time<2100, 5,
                                                    ifelse(junk$time>=2100 & junk$time<2400, 6, NA
                                                    ))))))
  }
  
  
  sp <- split(junk, list(junk$strata,junk$day,junk$mon))
  
  samples <- lapply(sp, function(x) x[sample(1:nrow(x), num, FALSE),])
  
  junk1 <- do.call(rbind, samples)
  
  # adjust start time here-------------v
  junk<-junk1[!(junk1$mon==4&junk1$day<startday)&!(junk1$mon==6&junk1$day>30)
              &!(junk1$mon==4&junk1$day==31),]
  
  out<-junk[order(junk$mon,junk$day,junk$time),]
  
  col_order <- c("year", "mon", "day",
                 "time","strata",
                 "count.upstream","count.downstream","number.up",
                 "notes","camera.desc")
  out <- out[, col_order]
  
  if(recordtime==T){
    out$minutes=NA
    out$seconds=NA
  }
  
  write.csv(out,file=filename,na="",row.names = F)
  
  cat("\n","Count file saved to WD")
  
}