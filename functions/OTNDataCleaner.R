####OTN emailed data import and cleaning####
####NOTES FOR RUNNING FUNCTION####
##good.data is the dataframe of good data, that should be in the environment when running this script

##bad.data is the dataframe of bad data, that should be in the environment when running this script

##data.dir is the fullpath to the folder the data of interest is stored in. data needs to be in csv format
##needs to a file path in quotations

##receiver.meta.path is the fullpath to the recevier metadata csv folder
##needs to be a file path in quotations

##tag.meta.path is the fullpath to the tag metadata csv folder
##needs to be a file path in quotations

##the tag meta data file from OTN most likely will need to be modified, with the top four rows
##of data entry instructions removed. column headers should be left as they are
##This script cannot handle leaving the top four rows like the other three can

##threshold is the minimum total detections of a tag for a tag to be included in the analysis
##tags that are detected fewer times that the threshold will be assigned to .few.detects dataframe
##can be left blank, will default to 10
##needs to a integer

##Meta is either true or false
##TRUE (or T) means the recevier adn tag meta data files will be output as dataframes 
##at the end of the script. FALSE (or F) will prevent them from being produced
##one of the three VR2W, VR100, or HR2 scripts should have this as true

##Years is a sequence, or single value, of the different years of OTN data contained in the data.dir directory


####MAIN FUNCTION####
CleanOTNData<-function(good.data, #the data frame of good data
                       bad.data, #the data frame of good data
                       data.dir, #the fullpath to the folder the csv's are stored in
                       receiver.meta.dir, #the fullpath to the receiver metadata csv
                       tags.meta.dir, #the fullpath to the tag metadata csv
                       threshold, #the number of detections of a tagged required for analysis
                       meta, #True/False value whether to produce meta data dataframes
                       years #a vector or single value of years of OTN data to process (2017,2018,...)
)
  
{  
  
###threshold is the number of total detections a tag had, if not specified it is set at 10
if(missing(threshold))
{threshold<-10}  

  
###meta is a true or false value. if TRUE, meta data data frames will be produced
##if false, they will not be produced
if(missing(meta))
{meta<-TRUE}  
  
  
import.folder<-function(directory,pattern) #I want to read in different sets of files, so I added pattern as an arg  
{
    
  setwd(directory)
    
  list.filenames<-list.files(pattern=pattern) #here the pattern argument is used to get .csv's, or files that end in 2017.csv
    
  if(length(list.filenames)==0)
  {stop("NO CSV's in data.dir folder")}
    
  list.data<-list()
  
  for(i in 1:length(list.filenames))
  {
    list.data[[i]]<-read.csv(list.filenames[i])
  }
  
  names(list.data)<-list.filenames
  
  list.data
  
}

####Read in all meta data files####
##receiver metadata files
rec.meta<-import.folder(receiver.meta.dir,".csv$")
for(i in 1:length(rec.meta))
{rec.meta[[i]]<-rec.meta[[i]][,1:23]} ##this trims extra columns at the end of files
rec.meta<-do.call("rbind",rec.meta)

##tag metadata files
##Note that these files need to be properly formatted, unlike in the VR2W, VR100, HR2 scripts
##which allow the four rows of info at the top to remain
tag.meta<-import.folder(tags.meta.dir,".csv$")
for(i in 1:length(tag.meta))
{tag.meta[[i]]<-tag.meta[[i]][,1:56]} ##this trims extra columns at the end of files
tag.meta<-do.call("rbind",tag.meta)


##A check to make sure the correct files were read in with the correct format
if(names(rec.meta[1])!="OTN_ARRAY")
{warning("Receiver metadata file doesn't match standard format. Check rec.meta.path is to correct file")}

if(names(tag.meta[2])!="TAG_TYPE")
{warning("Tag metadata file doesn't match standard format. Check tag.meta.path is to correct file")}


##fix formatting of certain columns in meta data
##Fix format of UTC_RELEASE_DATE_TIME
tag.meta$UTC_RELEASE_DATE_TIME<-gsub("T"," ",tag.meta$UTC_RELEASE_DATE_TIME)
tag.meta$UTC_RELEASE_DATE_TIME<-as.POSIXct(tag.meta$UTC_RELEASE_DATE_TIME,tz="UTC")


####Process OTN files####
marss.mdood.all<-list()
marss.md.all<-list()
marss.mqd.all<-list()
marss.muqd.all<-list()

##This for loop process the OTN data for each year i, and assigns the processed data to a list (above)  
for(i in 1:length(years))
{

cat("\n Processing data from year ")
cat(years[i]) 

##remove previous objects
try(rm(marss.mdood,
       marss.md,
       marss.mqd,marss.mqd.match,
       marss.muqd,marss.muqd.match),silent=TRUE)


#use the import folder function to assign list of imported csv's (now dataframes) to a list
#the pattern argument allows us to only grab csv's ending in the year of interest
#this way we can loop over all the years of interest, and each iteration of the loop deals with a single year
stew.import<-import.folder(directory=data.dir,pattern=paste(years[i],".csv$",sep=""))

#convert list of data frames into seperate ones based on their type
#the grep function works similar to gsub, see appendix of accompanying document
#essentially we all the end of the csv name which is the year to be any number of any digits,
#so that this code will work for whatever year is specified by the years argument

#ALSO, since some of the four files might not exist for each year, lets wrap them all in a try() function
#We'll have to include all their operations in the try function as well
try(
marss.mdood<-stew.import[[grep("marss_matched_detections_on_other_deployments_[0-9]*.csv",names(stew.import))]]
)
try(
marss.md<-stew.import[[grep("marss_matched_detections_[0-9]*.csv",names(stew.import))]]
)
try(
marss.mqd<-stew.import[[grep("marss_qualified_detections_[0-9]*.csv",names(stew.import))]]
)
try(
marss.muqd<-stew.import[[grep("marss_unqualified_detections_[0-9]*.csv",names(stew.import))]]
)


#Since any of the above four files may not exist for a given year, all operations performed on the files
#will be completed in an if statement, where exists() will be used to check and see if the object of interest exists

####check to see if any of the OTN files match our tags of interest
####the first file, matched_detections_on_other_deployments_2018.csv, will by definition have new tag detects of interest
####the second file, matched_detections, will have our data submitted to OTN, and the first file mentioned above.
###the second file should be checked to see if any new detections are in it, just in case stuff got missed somewhere
#### the third and fourth files, qualifed and unqualified detections, shouldn't have any of our tags.
#### a quick check is included too see if the third/fourth file does have important info


####First of Four files####
if(exists("marss.mdood"))
{
  marss.mdood$tag.id.code<-gsub(".*-([0-9]*)","\\1",marss.mdood$tagname)
  marss.mdood$datecollected<-as.POSIXct(marss.mdood$datecollected,tz="UTC")
  
  marss.mdood.sub<-subset(marss.mdood, select=c(receiver,
                                          datecollected,
                                          station,
                                          tagname,
                                          receiver_group,
                                          tag.id.code,
                                          latitude,
                                          longitude
  ))
  
  #grab the detections that match the tag metadata
  marss.mdood.sub<-marss.mdood.sub[marss.mdood.sub$tag.id.code %in% tag.meta$TAG_ID_CODE,]
  
  marss.mdood.sub$deploy.datetime<-rep(NA,length(marss.mdood.sub$receiver))
  marss.mdood.sub$recover.datetime<-rep(NA,length(marss.mdood.sub$receiver))
  marss.mdood.sub$earlytag<-rep(NA,length(marss.mdood.sub$receiver))
  marss.mdood.sub$latetag<-rep(NA,length(marss.mdood.sub$receiver))
  
  names(marss.mdood.sub)<-c("rec.serial","datetimeUTC","Receiver","Transmitter","Station.Name","tag.id.code",
                      "Lat","Lon","deploy.datetime","recover.datetime","earlytag","latetag")
  
  marss.mdood.all[[i]]<-marss.mdood.sub
  
} #ends data wrangling for marss.mdood objects in year[i]
else{cat("\n No matched_detections_on_other_deployments CSV in year ")
     cat(years[i])}


####Second of Four files####
if(exists("marss.md"))
{
  marss.md$tag.id.code<-gsub(".*-([0-9]*)","\\1",marss.md$tagname)
  marss.md$datecollected<-as.POSIXct(marss.md$datecollected,tz="UTC")
  marss.md<-marss.md[marss.md$tag.id.code %in% tag.meta$TAG_ID_CODE,]
  
  ##Need to do a match and extract with the tag.id.code and tag metadata
  marss.md.sub<-subset(marss.md,select=c(receiver,
                                         datecollected,
                                         station,
                                         tagname,
                                         receiver_group,
                                         tag.id.code,
                                         latitude,
                                         longitude
  ))
  
  marss.md.sub$deploy.datetime<-rep(NA,length(marss.md.sub$receiver))
  marss.md.sub$recover.datetime<-rep(NA,length(marss.md.sub$receiver))
  marss.md.sub$earlytag<-rep(NA,length(marss.md.sub$receiver))
  marss.md.sub$latetag<-rep(NA,length(marss.md.sub$receiver))
  
  names(marss.md.sub)<-c("rec.serial","datetimeUTC","Receiver","Transmitter","Station.Name","tag.id.code",
                      "Lat","Lon","deploy.datetime","recover.datetime","earlytag","latetag")
  
  ##Exclduing parts of the marss.md dataframe that are undesirable
  #remove rows that exist in the original data frame in the environment
  ##when multiple conditons are given with the or"|" in between, the rows that simultaneously matches those
  #conditions will be excluded via the ! operator
  marss.md.sub<-subset(marss.md.sub, !rec.serial %in% good.data$rec.serial |
                                 !tag.id.code %in% good.data$tag.id.code |
                                 !datetimeUTC %in% good.data$datetimeUTC
                   )
  
  #some of the marss.md file is the tag meta data, wheere the receiver is called release. this removes those rows
  marss.md.sub<-subset(marss.md.sub, rec.serial!="release")
  
  #since the marss.mdood objective is a subset of the mars.md object, lets remove that from there
  if(exists("marss.mdood")) # some years dont have the marss.mdood file, need to do this operation in an if statement
  {
    #remove rows that are in the marss.mdood object
    marss.md.sub<-subset(marss.md.sub, !rec.serial %in% marss.mdood.sub$rec.serial |
                       !tag.id.code %in% marss.mdood.sub$tag.id.code |
                       !datetimeUTC %in% marss.mdood.sub$datetimeUTC
    )
  }
  
  ##Remove rows that match those in the "bad.data" dataframe
  marss.md.sub<-subset(marss.md.sub, !rec.serial %in% bad.data$rec.serial |
                         !tag.id.code %in% bad.data$tag.id.code |
                         !datetimeUTC %in% bad.data$datetimeUTC
  )
  
  
  marss.md.all[[i]]<-marss.md.sub
  
} #ends data wrangling for marss.md objects in year[i]
else{cat("\n No matched_detections CSV in year ")
     cat(years[i])}


####Third of Four files####
if(exists("marss.mqd"))
{
  #third and fourth file checks. should return false
  marss.mqd$tag.id.code<-gsub(".*-([0-9]*)","\\1",marss.mqd$fieldnumber)
  marss.mqd$datecollected<-as.POSIXct(marss.mqd$datecollected,tz="UTC")
  
  marss.mqd.match<-marss.mqd[marss.mqd$tag.id.code %in% tag.meta$TAG_ID_CODE,]
  if(dim(marss.mqd.match)[1]==0)
  {
    cat("\n No tags in marss_qualified_detections_.csv match tag metadata for year ")
    cat(years[i])
  }
  else
  {
    cat("Some tags in marss_qualified_detections_.csv match tag metadata \n")
    cat("Those tags assigned to data frame marss.mqd.tagmatch for checking \n")
    marss.mqd.all[[i]]<-marss.mqd.match
  }
  
  
} #ends data wrnagling for marss.mqd objects in year[i]
else{cat("\n No qualified_detections CSV in year ")
     cat(years[i])}


####Fourth of Four files####
if(exists("marss.muqd"))
{
  #third and fourth file checks. should return false
  marss.muqd$tag.id.code<-gsub(".*-([0-9]*)","\\1",marss.muqd$fieldnumber)
  marss.muqd$datecollected<-as.POSIXct(marss.muqd$datecollected,tz="UTC")
  
  marss.muqd.match<-marss.muqd[marss.muqd$tag.id.code %in% tag.meta$TAG_ID_CODE,]
  if(dim(marss.muqd.match)[1]==0)
  {
    cat("\n No tags in marss_unqualified_detections_.csv match tag metadata for year ")
    cat(years[i])
  }
  else
  {
    cat("Some tags in marss_unqualified_detections_.csv match tag metadata \n")
    cat("Those tags assigned to data frame marss.muqd.tagmatch for checking \n")
    marss.muqd.all[[i]]<-marss.muqd.match
  }
  
  
} #ends data wrangling for marss.muqd objects in year[i]
else{cat("\n No unqualified_detections CSV in year ")
     cat(years[i])}



} #close for loop of years of OTN data

####Assign objects to the enivronment####
##objects are in lists, convert them into one large dataframe each
marss.mdood.all<-do.call("rbind",marss.mdood.all)
marss.md.all<-do.call("rbind",marss.md.all)
marss.mqd.all<-do.call("rbind",marss.mqd.all)
marss.muqd.all<-do.call("rbind",marss.muqd.all)

assign("marss.mdood.all",marss.mdood.all,envir=.GlobalEnv)
assign("marss.md.all",marss.md.all,envir=.GlobalEnv)
assign("marss.mqd.all",marss.mqd.all,envir=.GlobalEnv)
assign("marss.muqd.all",marss.muqd.all,envir=.GlobalEnv)

if(meta==T)
{
assign("rec.meta.all",rec.meta,envir=.GlobalEnv)
assign("tag.meta.all",tag.meta,envir=.GlobalEnv)
}


} #closes function


