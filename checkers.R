#
#Description:
#
# Inputs:

#
# This function is used by:
# 

# This functions uses:


checkers<-function(filename){
  ###
  ##July/August 2021
  ## Lucinda Laskey
  
  #Preface
  
  #this is a function  that is designed to be run on river herring biological data
  #it is designed to identify outliers and data-entry errors 
  #it will provide summary statistics for numerical values as well as basic plots 
  #the function is depended on certain column titles remaining consistent throughout documents and years
  #column names include "day""month""year""sample""species""sex""fork.length""weight"
  #species must be "A" or "B" NOT "Alewife" and/or "Blueback" 
  #this script ignores age and scale data and focuses only on identifying errors in biological data (weight and length)
  
  #Checkers Layout
  
  #Applies name to .csv
  
  #Column  title check
  
  #Issues of sample number 
  ##plots sample number against index
  ##look for duplicate sample numbers
  ##check the order
  
  #(date conversion)
  
  #Issues with year 
  ##plot 
  ##look for multiple years
  
  #Issues with month 
  ##plot each month
  ##04>month>06
  
  #Issues with day 
  ##plots day vs dayofrun for each month
  ##1<month<30/31 for each month
  ##days with no samples
  
  #Issues with weight
  ##Summary Statistics
  ##Plot day of run vs weight w/line 
  ##100<weight<300 (g) 
  
  #Issues with Fork Length
  ##Summary Statistics
  ##plot day of run vs fork length w/ line
  ##22<fork length<20 (cm)
  
  #Fork Length vs Weight Plot w/ line
  ##calculate residuals and standardize them
  ##show index # with missing residuals
  
  #Plot standardized residuals
  #-4<residual<4
  
  #Plot blueback run 
  ##date of first blueback 
  
  #################
  cat("Running Checkers Function", "\n", "\n")  
  
  #load tidyverse includes ggplot 2 and dyplyr
  library(dplyr,pos=3)
  #Set working directory to location of biological data .csv file
  
  
  #apply a name to the dataset 
  #name should be indicative of dataset-- can include river name and year
  #this name will be continuously called upon throughout the script and is the origin for any extractions
  #using this function and replacing the name will allow it to be used as a generic script
  #applying a name to the dataset will input it into the RStudio "environment" where it can be opened and examined
  #selectdata<-read.csv("filename")
  
  #alldata<-read.csv("Tusket_Biological_2021_Fixed.csv")
  alldata<-read.csv(filename)
  
  ############
  #Checks column names before beginning
  #Specifies what column names have to be for the function to work properly
  names(alldata)
  goodnames=(c("day","mon","year","sample","species","sex","fork.length","weight"))
  
  #Checks if names in dataset match names specified by "goodnames"
  #if not, warning message pop-up before script continues 
  missingnames=goodnames[!goodnames%in%(names(alldata))]
  if (length(missingnames>0)){
    stop(paste("Missing column name(s):", missingnames))
  }
  
  selectdata=alldata%>%
    select(names(alldata)[names(alldata) %in% goodnames])
  
  ###############
  #Looking for errors in Sample Number
  #sample number should not have any duplicates/anomalies unless they are in concordance with  the field books
  
  cat("Checking for issues in sample number...", "\n")
  
  #plot should be a straight diagonal plot with no outliers 
  plot(selectdata$sample,
       pch=20,
       xlab= "Index",
       ylab="Sample Number",
       main="Sample number")
  
  #"duplicated" looks for duplicate sample numbers 
  duplicates=selectdata$sample[which(duplicated(selectdata$sample))]
  
  #"if" compares "duplicated" results to a set parameter (in this case 0) 
  #if there are duplicates it will print all duplicates found in sample column
  if(length(duplicates)>0){
    cat("Duplicated sample numbers found (index and sample number should be the same):", "\n", "\n")
    print(selectdata[selectdata$sample%in%duplicates,])
    cat("\n", "\n")
  }
  
  #Checking if sample numbers are in consecutive order from 1 to dim (dimensions of data set)
  #applies i to the second data entry and all following entries become i-1
  #hopefully there is no error in the first sample number, which should be 1
  selectdata$order=NA
  for (i in (2:dim(selectdata)[1])){
    selectdata$order[i]=selectdata$sample[i]-selectdata$sample[i-1]
  }
  
  #applies #1 to first data entry 
  selectdata$order[1]=selectdata$sample[1]
  
  #checks that all values in "order" column are equal to 1
  #any number not equal to 1 indicates that the sample numbers are not consecutive 
  #will print a warning if order != 1
  sampleorder=selectdata[!selectdata$order==1,]
  if (dim(sampleorder)[1]>0){
    cat("Issues with sample number order to be checked (may be overlap with duplicate errors):", "\n", "\n")
    print(sampleorder)
    cat("\n", "\n")
  }
  
  if((length(duplicates)==0) & (dim(sampleorder)[1]==0)){
    cat("No sample number issues found.", "\n", "\n")
  }
  
  ##################
  #Date conversion 
  
  #date conversion amalgamates month and year columns into one format
  selectdata$date=as.Date(paste(selectdata$day,selectdata$mon,selectdata$year,sep="-"),
                          format="%d-%m-%Y")
  
  #dayofyear uses "strftime" to evaluate which day of the year each date aligns with
  selectdata$dayofyear=as.numeric(strftime(selectdata$date, format="%j"))
  
  #Day of run, first day is evaluated on a yearly basis 
  firstday<-function(data,year){
    z=data$dayofyear[data$year==year]
    min(z, na.rm=TRUE)
  }
  
  #dayofrun is evaluated based on dayofyear
  for(i in 1:dim(selectdata)[1]){
    selectdata$dayofrun[i]<-(selectdata$dayofyear[i]+1)-firstday(selectdata, selectdata$year[i])
  }
  
  
  ###################
  #Looking for errors in Year
  #Year should all be the same 
  cat("\n")
  cat("Checking for issues in Year...", "\n")
  
  #plot should be a straight vertical plot with no outliers 
  plot(selectdata$year, selectdata$sample,
       pch=20,
       xlab= "Year",
       ylab=" ",
       yaxt= 'n',
       main="Year")
  
  #use mode to identify other years present in dataset
  #this is assuming that the majority of the years in the dataset are correct
  calcmode <- function(a) {  
    vector <- na.omit(unique(a))  
    vector[which.max(tabulate(match(a, vector)))]  
  } 
  
  #calculate mode of year 
  yearmode<-calcmode(selectdata$year)
  yearmode
  
  if(length(selectdata$year[selectdata$year>yearmode | selectdata$year<yearmode])>0){
    cat("Multiple years found:","\n")
    print(selectdata%>%
            select(names(selectdata)[names(selectdata) %in% goodnames])%>%
            filter(year!=yearmode)%>%
            arrange(year))
    cat("\n")
  } else{cat("No errors in Year found.", "\n", "\n")
  }
  
  ##############
  #looking for issues in month 
  cat("\n")
  cat("Checking for issues in Month...", "\n")
  
  #filters any samples which do not fall between April and June
  mc2<-selectdata%>%
    select(names(selectdata)[names(selectdata) %in% goodnames])%>%
    filter(mon>6 | mon<4)%>%
    arrange(mon)
  mc2
  
  if(length(selectdata[selectdata$mon[selectdata$mon>6 | selectdata$mon<4]>0])){
    cat("Months exceeding parameters (04-06):", "\n", "\n")
    print(mc2)
    cat("\n")
  } else{cat("No errors in Month found.", "\n", "\n")
  }
  
  ##################
  #looking for errors in day 
  cat("\n")
  cat("Checking for errors in Day...", "\n")
  
  #plots display data of which days samples were taken
  #plots are divided up by month
  plot((selectdata$mon),selectdata$day,pch=20, xlab="Month", ylab="Day",
       xaxt='n')
  axis(1,at=c(4,5,6),labels=c("April","May","June"))
  points((selectdata$mon[(selectdata$day>30|selectdata$day<1)&selectdata$mon==4]),
         (selectdata$day[(selectdata$day>30|selectdata$day<1)&selectdata$mon==4]),col="red",pch=20)
  points((selectdata$mon[(selectdata$day>31|selectdata$day<1)&selectdata$mon==5]),
         (selectdata$day[(selectdata$day>31|selectdata$day<1)&selectdata$mon==5]),col="red",pch=20)
  points((selectdata$mon[(selectdata$day>30|selectdata$day<1)&selectdata$mon==6]),
         (selectdata$day[(selectdata$day>30|selectdata$day<1)&selectdata$mon==6]),col="red",pch=20)
  
  
  #Checking for any days in april that exceed 30 or are less than 1 
  aprildays<- selectdata%>%
    select(names(selectdata)[names(selectdata) %in% goodnames])%>%
    filter(day>30 | day<1)%>%
    filter(mon==4)%>%
    arrange(day)
  
  
  #if days exceed these parameters, then it will print them in the console 
  if(length(aprildays>0)){
    cat("Days in April exceeding appropriate parameters", "\n")
    print(aprildays)
    cat("\n", "\n")
  }else{cat("No day errors in April found.","\n", "\n")
  }
  
  
  #Checking for any days in may that exceed 31 days or are less than 1 
  maydays<-selectdata%>%
    select(names(selectdata)[names(selectdata) %in% goodnames])%>%
    filter(day>31 | day<1)%>%
    filter(mon==5)%>%
    arrange(day)
  
  #if days exceed these parameters, then it will print them in the console 
  if(length(maydays>0)){
    cat("Days in May exceeding appropriate parameters", "\n")
    print(maydays)
    cat("\n", "\n")
  }else{cat("No day errors in May found.", "\n", "\n")
  }
  
  
  #Checking for any days in june that exceed 30 days or are less than 1 
  junedays<- selectdata%>%
    select(names(selectdata)[names(selectdata) %in% goodnames])%>%
    filter(day>30 | day<1)%>%
    filter(mon==6)%>%
    arrange(day)
  
  #if days exceed these parameters, then it will print them in the console 
  if(length(junedays>0)){
    cat("Days in June exceeding appropriate parameters", "\n")
    print(junedays)
    cat("\n", "\n")
  }else{cat("No day errors in June found.", "\n", "\n")
  }
  
  #checking for days in which no sampling was done
  cat("\n")
  cat("Checking for skipped sampling days...", "\n")
  
  #calculates the dayofyear which the first sample was taken
  startofrun=min(selectdata$dayofyear,na.rm=T)
  
  #calculates dayofyear which last sample was taken
  endofrun=max(selectdata$dayofyear,na.rm=T)
  
  #length of the run is categorized as the start to the end
  alldays=startofrun:endofrun
  
  #missing days are any days within alldays(length of the run) which have no reported samples 
  missingdays=alldays[!(alldays%in%selectdata$dayofyear)]
  
  #ensures the dates are printed in date format
  #missingdates=as.numeric(strftime(missingdays,format="%j"))
  missingdates=as.Date(missingdays,
                       origin=paste((unique(calcmode(selectdata$year))-1),12,31,sep="-"))
  
  
  if(length(missingdates>0)){
    cat("Missing days:", "\n")
    cat(as.character(missingdates))
    cat("\n", "\n")
  }else{cat("No missing sampling dates", "\n", "\n")
  }
  
  ##########
  #Printing sex ratio 
  cat("\n")
  cat("Printing Sex Ratio...", "\n")
  print(table(selectdata$sex))
  cat("\n")
  
  #Printing species ratio 
  cat("\n")
  cat("Printing Species Ratio...", "\n")
  print(table(selectdata$species))
  cat("\n", "\n")
  
  ############
  #looking for errors in Weight
  cat("\n")
  cat("Checking for errors in Weight:", "\n")
  
  #Summary Stats for weight
  cat("Summary Statistics for Weight (Note: Summary Stats includes errors)", "\n")
  
  print(summary(selectdata$weight, na.rm=TRUE))
  cat("\n")
  cat("Range:", (range(selectdata$weight, na.rm=TRUE)), "\n")
  cat("Standard Deviation:", sd(selectdata$weight, na.rm=TRUE), "\n")
  cat("Mode:", calcmode(selectdata$weight), "\n", "\n","\n")
  #"na.rm=TRUE" not added to "calcmode" because it is integrated into the function
  
  
  #plot of dayofrun vs weights-- not sex or species specification
  #set line parameters for plot
  wline<- lm(selectdata$weight~selectdata$dayofrun, na.action=na.exclude)
  
  (plot(selectdata$dayofrun, selectdata$weight,
        pch=20,
        main="Day of Run vs Weight of River Herring",
        xlab="Day of Run",
        ylab="Weight(g)"))
  abline(wline,lty=1, lwd=2)
  
  cat("Day of Run vs Weight line equation", "\n")
  print(wline)
  cat("\n")
  
  #Filter Weight ascending/descending for errors
  #extract/filter highest and lowest values from numeric values and proportions
  wc1<- selectdata%>%
    select(names(selectdata)[names(selectdata) %in% goodnames])%>%
    filter(weight>300 | weight<100)%>%
    arrange(weight)
  
  if(length(wc1)>0){
    cat("Samples with Weights over 300g and under 100g", "\n")
    print(wc1)
    cat("\n", "\n")
  } else{cat("No issues in Weights", "\n", "\n")
  }
  
  ##############
  #looking for errors in Fork Length
  cat("\n")
  cat("Checking for errors in Fork Length", "\n", "\n")
  
  #summary stats for fork length
  cat("Summary Statistics for Fork Length (Note: Summary Stats includes errors)", "\n")
  
  print(summary(selectdata$fork.length, na.rm=TRUE))
  cat("\n")
  cat("Range:", range(selectdata$fork.length, na.rm=TRUE), "\n")
  cat("Standard Deviation:", sd(selectdata$fork.length, na.rm=TRUE), "\n")
  cat("Mode:", calcmode(selectdata$fork.length),"\n", "\n","\n")
  
  
  #plot of dayofrun vs fork-- not sex or species specification
  #set line parameters for plot
  flline<- lm(selectdata$fork.length~selectdata$dayofrun, na.action=na.exclude)
  
  (plot(selectdata$dayofrun, selectdata$fork.length,
        pch=20,
        main="Day of Run vs Fork Length of River Herring",
        xlab="Day of Run",
        ylab="Fork Length(cm)"))
  abline(flline,lty=1, lwd=2)
  
  cat("Day of Run vs Fork Length line equation", "\n")
  print(flline)
  cat("\n", "\n")
  
  #extract/filter highest and lowest values from numeric values and proportions
  flc1<- selectdata%>%
    select(names(selectdata)[names(selectdata) %in% goodnames])%>%
    filter(fork.length>28.0 | fork.length<22.0)%>%
    arrange(fork.length)
  
  if(length(flc1)>0){
    cat("Samples with Fork Lengths exceeding 28.0cm and under 22.0cm", "\n")
    print(flc1)
    cat("\n", "\n")
  } else{cat("No issues in Fork Length", "\n", "\n")
  }
  
  #################
  #Fork Length vs Weight Plot 
  cat("Plotting Fork Length vs Weight...", "\n")
  #establish line equation for fork length vs weight before plotting 
  flxw_line<- lm(selectdata$weight~selectdata$fork.length, na.action=na.exclude)
  
  #plot Weight vs Fork length with no sex or species differentiation
  (plot(selectdata$fork.length, selectdata$weight,
        pch=20,
        main="Fork Length vs Weight of River Herring",
        ylab="Weight (g)",
        xlab="Fork Length(cm)")) 
  abline(flxw_line,lty=1, lwd=2)
  
  
  cat("Fork Length vs Weight line equation", "\n")
  print(flxw_line)
  cat("\n")
  
  #residuals 
  cat("\n")
  cat("Calculating Residuals from Fork Length vs Weight Plot", "\n")
  #calculate residuals of from line 
  residuals(flxw_line)
  
  #display summary statistics fro residuals 
  summary(flxw_line)
  
  #insert new column of standardized residuals 
  selectdata$standres<-rstandard(flxw_line)
  selectdata$standres
  
  #function identifies if any NA values are present in the column
  cat("\n")
  cat("Index numbers with NA as residual", "\n")
  cat(which(! complete.cases(selectdata$standres)), "\n", "\n")
  
  #plot standardized residuals  
  plot(selectdata$sample,selectdata$standres,
       pch=20,
       main="Plot of Standardized Residuals of Fork Length vs Weight of River Herring",
       xlab="Sample Number",
       ylab="Standardized Residual Values") 
  
  
  #filter standardized residuals larger than 1 and less than 0 
  anom_res<-selectdata%>%
    select(names(selectdata)[names(selectdata) %in% goodnames],standres)%>%
    filter(standres>4 |standres<(-4))%>%
    arrange(standres)
  
  if(length(anom_res)>0){
    cat("Residual values less than -4 and larger than 4", "\n")
    print(anom_res)
    cat("\n", "\n")
  } else{cat("All residuals within range [-4,4]", "\n", "\n")
  }
  #residuals larger than 4 and -4 should be checked, especially those larger than +5 and -5
  #this function identifies samples in which the weight and lengths do not match
  #these sample numbers should be checked against the field book 
  #negative residuals likely need their weights checked and increased
  #positive residuals likely need weights checked and decreased 
  
  ############
  #checking blueback run
  #plot datofrun vs sample number for bluebacks only
  #this gives insight into whether or not they are running too early 
  #xlim and ylim set to parameters so plot is comprehensive
  cat("\n")
  
  bbrun<-selectdata%>%
    select(names(selectdata)[names(selectdata) %in% goodnames],dayofrun,date)%>%
    filter(species=="B")%>%
    arrange(sample)
  
  
  if(length(bbrun>0)){
    cat("Date of first Blueback sampled (please check field book to ensure this is true)", "\n")
    cat(as.character(unique(bbrun$date[bbrun$dayofrun==min(bbrun$dayofrun, na.rm=TRUE)])),"\n", "\n")
    cat("Plotting Bluback Run...","\n","\n")
    (plot(selectdata$dayofrun[selectdata$dayofrun>0][selectdata$species=="B"],selectdata$sample[selectdata$dayofrun>0][selectdata$species=="B"],
          pch=20,
          main="Blueback Run",
          xlab="Day of Run (day)",
          ylab="Sample Number"))
    cat("\n","End of Checkers Function")
  } else{
    cat("No Bluebacks Sampled", "\n","\n", "End of Checkers Function")
  }
  
  
  ##############
  #End of function bracket:
}