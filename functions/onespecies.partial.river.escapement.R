#
# Calculate the daily and total escapement estimate from count files. 
# Can adjust if counts are under 5 min and will estimate missing strata.
# Will not currently run if there are missing days. Needs at least one count per day.
# This should be added in in the future.
#
# Inputs:
#        - filename: filename where escapement counts are
#        - fixtime=F: default is F but use T if the minutes/seconds columns are present
#        - database=T: pull count data from database instead of local file (prefered method)
#        - year: use if database=T
#        - site: use if database=T
#        - channel: use if database=T
#
# This function is used by:
#     - no user defined functions use this function

# This functions uses:
#     - get.count.data
#     - first.last.day
#     - user.SE
#     - user.count
#     - satterthwaite.approx.df
#     - runVAR.term
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2022/Data Sheets/Counts")
filename="Carleton Count Sheet Cleaned 2022.csv"
  
onespecies.partial.river.escapement<-function(filename,
                                      fixtime=F,
                                      database=T,
                                      year,
                                      site,
                                      channel)
{
  if(database==T){
    count.data<-get.count.data(year, site, channel)
    names(count.data)[1]<-"year"
    names(count.data)[2]<-"mon"
    names(count.data)[3]<-"day"
    names(count.data)[4]<-"strata"
    names(count.data)[5]<-"time"
    names(count.data)[6]<-"count.upstream"
    names(count.data)[7]<-"count.downstream"
    names(count.data)[8]<-"minutes"
    names(count.data)[9]<-"seconds"
  }
  if(database==F){
    count.data=read.csv(filename,header=T,
                        stringsAsFactors = F)
    
    if(fixtime==F){
      goodnames=(c("year","mon","day","time","strata","count.upstream",
                   "count.downstream")) 
    }
    if (fixtime==T){
      goodnames=(c("year","mon","day","time","strata","count.upstream",
                   "count.downstream","minutes","seconds"))  
    }
    missingnames=goodnames[!goodnames%in%(names(count.data))]
    if (length(missingnames>0)){
      cat("Missing column name(s) from local datasheet:","\n", missingnames,"\n")
      stop("Please fix column names before continuing")
    }
    
  } 
  
  #Checks if names in dataset match names specified by "goodnames"
  #if not, warning message pop-up before script continues 
  
  
  nyears=length(unique(count.data$year))
  if (nyears>1){
    cat("Warning: This data file contains multiple years")
  }
  #---
  # DATA CLEAN UP/REORGANIZATION
  count.data$total=count.data$count.upstream-count.data$count.downstream
  #date conversion amalgamates month and year columns into one format
  
  if(database==F & fixtime==T){
    count.data$total=round((count.data$total/
                              (count.data$minutes*60+count.data$seconds))*300)
  }
  
  count.data$date=as.Date(paste(count.data$day,count.data$mon,count.data$year,sep="-"),
                          format="%d-%m-%Y")
  
  #dayofyear uses "strftime" to evaluate which day of the year each date aligns with
  count.data$dayofyear=as.numeric(strftime(count.data$date, format="%j"))
  
  
  #Extract DOY for first and last day that counts were conducted
  start.end=first.last.day(count.data,
                           "dayofyear",
                           count.data$year[1],
                           "total")
  
  #Select only data from day of first count onwards
  count.data=count.data[count.data$dayofyear>=start.end[1] &
                          count.data$dayofyear<=start.end[2],]
  #===
  # MEANS, STANDARD DEVIATION, VARIANCE, STANDARD ERROR 
  # AND TIME UNITS SAMPLES PER PERIOD PER DAY (two way stratified)
  
  # 1A: Mean Counts
  strata.means<-aggregate(count.data$total,by=list(count.data$dayofyear,
                                                   count.data$strata),
                          FUN="mean",na.rm=T)
  colnames(strata.means)=c("dayofyear","strata","mean")
  
  # 2A: Standard Deviation
  strata.sd=aggregate(count.data$total,by=list(count.data$dayofyear,
                                               count.data$strata),
                      FUN="sd",na.rm=T)
  
  # 3A: Standard Error
  strata.se=aggregate(count.data$total,by=list(count.data$dayofyear,
                                               count.data$strata),
                      FUN=user.SE)
  
  # 4A: Variance
  strata.var=aggregate(count.data$total,by=list(count.data$dayofyear,
                                                count.data$strata),
                       FUN=function(x){var(x,na.rm=T)})
  
  # 5A: Time units sampled
  strata.n=aggregate(count.data$total,by=list(count.data$dayofyear,
                                              count.data$strata),
                     FUN=user.count)
  
  # 6A: Compile into dataframe 
  junk2<-data.frame(strata=strata.means$strata,
                    dayofyear=strata.means$dayofyear,
                    mean=strata.means$mean,
                    sd=strata.sd$x,
                    se=strata.se$x,
                    sample.var=strata.var$x,
                    n.counts=strata.n$x) 
  #add column containing total number of time units per strata per day
  n.strata=max(count.data$strata,na.rm=T)
  
  if(!(n.strata==5|n.strata==6)){
    stop("Number of Strata must be 5 or 6") }
  
  if(n.strata==5){
    min5.periods<-data.frame(strata=c(1,2,3,4,5),
                             n.periods=c(72,72,48,48,48)) }
  if(n.strata==6){
    min5.periods<-data.frame(strata=c(1,2,3,4,5,6),
                             n.periods=c(60,69,36,36,51,36))}
  #merge and order by strata
  summary.data<-merge(junk2,min5.periods,by="strata") 
  
  #--- UNCOUNTABLE TIME UNITS AND EXTRAPOLATION ---
  
  # Check for missing strata
  #Initialize df with all days/strata:
  alldays=data.frame(Group.1=rep(start.end[1]:start.end[2],each=5),
                     Group.2=rep(1:5,times=length(start.end[1]:start.end[2])))
  
  #merge with n.counts dataframe from previous section.
  alldays=merge(alldays,strata.n,all.x = T)
  names(alldays)=c("dayofyear","strata","n.counts")
  # Check for any NAs or counts less than 2:
  table(alldays$n.counts)
  length(alldays$n.counts[is.na(alldays$n.counts)|alldays$n.counts<=1])
  
  ##butcher data
  ##this splits the data into two, counts before a day was missed,
  ##and counts after the last day is missed.
  test<-aggregate(alldays,by=list(alldays$dayofyear),FUN=sum)
  no.count.days<-test$Group.1[test$n.counts==0]
  
  days<-start.end[1]:start.end[2]
  count.days<-setdiff(days,no.count.days)
  butcher.ls<-list()
  butcher.ls[[1]]<-summary.data[summary.data$dayofyear<min(no.count.days),]
  for(i in 2:length(no.count.days))
  {
    butcher.ls[[i]]<-summary.data[summary.data$dayofyear>no.count.days[i-1] & 
                                    summary.data$dayofyear<no.count.days[i],]
  }
  butcher.ls[[length(no.count.days)+1]]<-summary.data[summary.data$dayofyear>max(no.count.days),]
  
  
  # summary.data1<-summary.data[summary.data$dayofyear<min(no.count.days),]
  # summary.data2<-summary.data[summary.data$dayofyear>max(no.count.days),]
  # start.end1<-c(min(summary.data1$dayofyear),max(summary.data1$dayofyear))
  # start.end2<-c(min(summary.data2$dayofyear),max(summary.data2$dayofyear))
  
  if(length(alldays$n.counts[is.na(alldays$n.counts)|alldays$n.counts<=1])>0){
    cat("Missing Strata:","\n")
    print(alldays[alldays$n.counts<2 | is.na(alldays$n.counts),])
    # fill in missing mean count, n.counts, sample.var, and sd for
    # missing strata.
    filled.butcher.ls<-list()
    for(i in 1:length(butcher.ls))
    {
      if(nrow(butcher.ls[[i]])==0){next()}#skip iterations where dataframes are empty
      start.end<-c(min(butcher.ls[[i]]$dayofyear),max(butcher.ls[[i]]$dayofyear))
      filled.butcher.ls[[i]]=missingstrata(butcher.ls[[i]],start.end,n.strata)
    }
  }
  
  ##put the bits back together
  summary.data<-do.call(rbind,filled.butcher.ls)
  # If number of strata with counts >= 1 is >0 stop and use lm to 
  # extrapolate counts of missing strata. If the entire day is missing,
  # there is no way to fill in missing counts. 
  
  #---
  #mean by strata and day * number of periods
  summary.data$total<-summary.data$mean*summary.data$n.periods 
  
  #Determine the proportion of fish that escaped in each stratum
  print(paste("Strata 1 total:",strata.percent(1,summary.data)))
  print(paste("Strata 2 total:",strata.percent(2,summary.data)))
  print(paste("Strata 3 total:",strata.percent(3,summary.data)))
  print(paste("Strata 4 total:",strata.percent(4,summary.data)))
  print(paste("Strata 5 total:",strata.percent(5,summary.data)))
  
  if(n.strata==6){
    print(paste("Strata 6 total:",strata.percent(6,summary.data)))
  }
  
  #---
  # TOTAL ESCAPEMENT, VARIANCE AND CONFIDENCE INTERVAL(2 way stratified)
  
  # p = number of periods in day (k) e.g. 5 strata (periods)
  # k = each day
  # L = total days in run (e.g. 67)
  # N = number of time units per period (p) e.g. 72 or 48,5-min intervals
  # n = number of time units sampled (e.g. 3)
  
  # 1B: Total Escapement 
  print("Escapement estimate:")
  esc.est<-sum(summary.data$total,na.rm=T)
  print(esc.est)
  
  # 2B: Total Variance 
  print("Variance estimate:")
  esc.var<-sum(runVAR.term(summary.data))
  print(esc.var)
  
  # 3B: Degrees of Freedom and Critical Values (2WS)
  ##satterthwaite approximation of degrees and freedom
  ##formula from nelson 2006
  summary.data.df<-summary.data[summary.data$n.counts>1,] ##this is a work around
  ##for counts strata with only a single count, which prevents the df calculation
  ## and therefore the total CI calculation.
  
  df2<-satterthwaite.approx.df(summary.data)
  alpha<-0.05
  crit<-qt(1-alpha/2, df2)
  
  # 4A: Confidence Interval 
  print("CI around escapement estimate:")
  print(esc.est+c(-crit,crit)*sqrt(esc.var)) 
  
  # DAILY VARIANCE AND STANDARD DEVIATION (1WS) 
  
  # 1C: Daily Variance 
  summary.data$daily.variance.term<-
    runVAR.term(summary.data)
  
  daily.var=aggregate(summary.data$daily.variance.term ,
                      by=list(summary.data$dayofyear),FUN="sum",na.rm=T)
  
  # 2C: Daily Standard Deviation
  daily.sd=sqrt(daily.var$x)
  
  # DAILY ESCAPEMENT TOTALS (1WS) AND BINDING 1WS VALUES ---
  
  #1D: Daily Totals 
  daily.total=aggregate(summary.data$total,
                        by=list(summary.data$dayofyear),FUN="sum",na.rm=T)
  colnames(daily.total)=c("dayofyear","total")
  
  # 2D: Bind all 1WS into dataframe
  daily.summary<-cbind(daily.total,daily.var[,2], daily.sd)
  colnames(daily.summary)=c("dayofyear","total","variance","sd")
  
  
  # DAILY DEGREES OF FREEDOM AND CONFIDENCE INTERVALS (1WS) ---
  
  ##Need tp redo start and end days, they got overwritten when butchering the data
  ##this onyl applies to the partial escapement function
  start.end<-c(min(levels(daily.summary$dayofyear)),max(levels(daily.summary$dayofyear)))
  
  for(i in start.end[1]:start.end[2]){
    daily.summary$df1[daily.summary$dayofyear==i]<-
      satterthwaite.approx.df(summary.data[summary.data$dayofyear==i,])
  }
  
  #Set alpha value and find critical value
  alpha<-0.05
  
  daily.summary$clow<-daily.summary$total-
    qt(1-alpha/2, daily.summary$df1)*sqrt(daily.summary$var) 
  
  daily.summary$chigh<-daily.summary$total+
    qt(1-alpha/2, daily.summary$df1)*sqrt(daily.summary$var) 
  
  #Preventing negative C.I. values 
  daily.summary$clow<-ifelse(daily.summary$clow<0,0,daily.summary$clow) 
  
  # Daily summary table
  daily.summary<-merge(daily.summary,
                       unique(count.data[,c("mon","day","dayofyear")]),
                       by="dayofyear",all.x=T)
  daily.summary<-(daily.summary[,c("mon","day","dayofyear",
                                   "total","sd","clow","chigh")])
  
  
  print(paste("First Fish on:",paste(daily.summary$mon
                                     [which(daily.summary$total > 0)[1]],
                                     daily.summary$day
                                     [which(daily.summary$total > 0)[1]],
                                     sep="-")))
  print(paste("Last Fish on:",paste(daily.summary$mon
                                    [max(which(daily.summary$total > 0))],
                                    daily.summary$day
                                    [max(which(daily.summary$total > 0))],
                                    sep="-")))
  
  print(paste("Peak of Run on:",
              paste(daily.summary$mon
                    [daily.summary$total==max(daily.summary$total)],
                    daily.summary$day
                    [daily.summary$total==max(daily.summary$total)],
                    sep="-"),"with",max(daily.summary$total),"fish"))
  
  
  daily.summary$total<-round(daily.summary$total,digits=1)
  return(daily.summary)
  ##End of function::  
} 