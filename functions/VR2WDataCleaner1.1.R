####VR2W Receiver import and cleaning####
##30-11-2021 Mark Billard
##Altering script so that it can properly handle moving receivers to different
##locations, and having multiple receivers at a station over the course
##of a season.
####NOTES FOR RUNNING FUNCTION####

##data.dir is the fullpath to the folder the data of interest is stored in. data needs to be in csv format
##needs to a file path in quotations

##receiver.meta.path is the fullpath to the recevier metadata csv file   
##needs to a file path in quotations

##tag.meta.path is the fullpath to the tag metadata csv file
##needs to a file path in quotations

##the tag meta data CSV may contain an extra four rows of general info at the top
##these can be removed or left there, lines 89-97 handle both scenarios

##threshold is the minimum total detections of a tag for a tag to be included in the analysis
##tags that are detected fewer times that the threshold will be assigned to .few.detects dataframe
##can be left blank, will default to 10
##needs to a integer

##the desired prefix for the names of the dataframes produced herein
##needs to a word in quotations, also called a character string
##if the dataset name is specified as: dataset.name="stewiacke" in the function, the dataframes
##produced will be called stewiacke.data, stewiacke.other.tags, etc.

##meta is either true or false
##TRUE (or T) means the recevier and tag meta data files will be output as dataframes 
##at the end of the script. FALSE (or F) will prevent them from being produced
##one of the three VR2W, VR100, or HR2 scripts should have this as true

####MAIN FUNCTION####
CleanVR2WData<-function(data.dir, #the fullpath to the folder the csv's are stored in
                       receiver.meta.path, #the fullpath to the receiver metadata csv
                       tags.meta.path, #the fullpath to the tag metadata csv
                       threshold, #the number of detections of a tagged required for analysis
                       dataset.name, #the prefix you want your dataframes to be called -"character"
                       meta #True/False value whether to produce meta data dataframes
                       )

{  

###threshold is the number of total detections a tag had, if not specified it is set at 10
if(missing(threshold))
{threshold<-10}  

###meta is a true or false value. if TRUE, meta data data frames will be produced
##if false, they will not be produced
if(missing(meta))
{meta<-TRUE}  
  
#function that imports all csv's in a folder (need to specify full path of folder)
import.folder<-function(data.dir)
{

setwd(data.dir)

list.filenames<-list.files(pattern=".csv$")

if(length(list.filenames)==0)
{stop("NO CSV's in data.dir folder")}

list.data<-list()

for(i in 1:length(list.filenames))
{
  
  list.data[[i]]<-read.csv(list.filenames[i])
  # if(length(colnames(list.data[[i]]))!=10)
  # {stop(paste(list.filenames[i],"doesn't have the correct columns",sep=" "))}
}

names(list.data)<-list.filenames

list.data

}


#use the above function to assign list of imported csv's (now dataframes) to a list
stew.import<-import.folder(data.dir)

#convert list of data frames into one big one, this also removes empty dataframes
stew.data<-do.call("rbind",stew.import)


##remove any duplicated rows resulting from reading in duplicate files, or files containg recent and all detections
stew.data<-stew.data[!duplicated(stew.data),]


##Get the time column renamed and working
colnames(stew.data)[1]<-"datetimeUTC" ####!!!!Double check all read in files have datetime as the first one
stew.data$datetimeUTC<-as.POSIXct(stew.data$datetimeUTC,tz="UTC") #make sure timezone is UTC, needs to be specified

###Read in receiver and tag meta data
rec.meta<-read.csv(receiver.meta.path)

##tag meta data can be read in with or without the four rows of unforamtted info at the top
tag.check<-read.csv(tags.meta.path,nrows=4) ##read in first four rows to check formatting
if(names(tag.check)[3]=="TAGGING.METADATA")
{tag.meta<-read.csv(tags.meta.path,skip=4) #if four rows of info present, skip 4 rows
} else {
  if(names(tag.check)[3]=="TAG_MANUFACTURER") 
  {tag.meta<-read.csv(tags.meta.path) #if rows not present, read as is
  } else {
    stop("Tag Metadata not formatted correctly, check CSV")}} #if neither, stop
rm(tag.check)


##A check to make sure the correct files were read in with the correct format
if(names(rec.meta[1])!="OTN_ARRAY")
{warning("Reciever metadata file doesn't match standard format. Check rec.meta.path is to correct file")}

if(names(tag.meta[2])!="TAG_TYPE")
{warning("Tag metadata file doesn't match standard format. Check tag.meta.path is to correct file")}

#incorporate the rec.meta lat/lons into the stew.data file
#first create columns in stew.data of just the receiver serial code, and just the tag id code
#to allow the dataframes to match up by those variables
##initally used substr to grab the last part of the string which is the rec serial,
##but different receviers (VR2W,VR2W180) have different inital length. sub str wont work
##I elected to use gsub, which is much more powerful and confusing.
##refer to the accompanying document for an indepth description of how the below works
stew.data$rec.serial<-gsub(".*-([0-9]*)","\\1",stew.data$Receiver)
stew.data$tag.id.code<-gsub(".*-([0-9]*)","\\1",stew.data$Transmitter)

rec.meta.merge<-subset(rec.meta,select=c(OTN_ARRAY, STATION_NO, INS_SERIAL_NO,
                                         DEPLOY_LAT, DEPLOY_LONG,
                                         DEPLOY_DATE_TIME....yyyy.mm.ddThh.mm.ss.,
                                         RECOVER_DATE_TIME..yyyy.mm.ddThh.mm.ss.,
                                         DOWNLOAD_DATE_TIME..yyyy.mm.ddThh.mm.ss.)) 

#rename columns to match stew.data
names(rec.meta.merge)<-c("OTN.array","Station.Name","rec.serial","Lat","Lon",
                         "deploy.datetime","recover.datetime","download.datetime")

##coerce columns to their proper format (recevier serial number should be a factor, etc.)
rec.meta.merge$OTN.array<-as.factor(rec.meta.merge$OTN.array)
rec.meta.merge$Station.Name<-as.factor(rec.meta.merge$Station.Name)
rec.meta.merge$rec.serial<-as.factor(rec.meta.merge$rec.serial)
rec.meta.merge$deploy.datetime<-as.character(rec.meta.merge$deploy.datetime)
#if there is no date/time of recovery, assign it a NA
rec.meta.merge$recover.datetime[rec.meta.merge$recover.datetime==""]<-NA
rec.meta.merge$recover.datetime<-as.character(rec.meta.merge$recover.datetime)

rec.meta.merge$download.datetime[rec.meta.merge$download.datetime==""]<-NA
rec.meta.merge$download.datetime<-as.character(rec.meta.merge$download.datetime)

##need to remove duplicate rows before converting datetime cols
rec.meta.merge<-rec.meta.merge[!duplicated(rec.meta.merge),] # get rid of duplicate rows, NA rows

###Fix added Nov 2019 Mark B.
##an issue was occuring where receivers that were still deployed but downloaded,
##thus having no recoverydate, were being excluded from rec.meta.merge,
##and therefore the entire analysis.
##I solve this by putting in the download date as the recovery date if the recovery date doesnt exist
rec.meta.merge$recover.datetime<-ifelse(is.na(rec.meta.merge$recover.datetime),rec.meta.merge$download.datetime,rec.meta.merge$recover.datetime)
##This bit is added for a lost receiver, by excluding the row is the recevier was enver downloaded or recovered
rec.meta.merge<-rec.meta.merge[!with(rec.meta.merge,is.na(recover.datetime)
                                     & is.na(download.datetime)),]

##datetime formats can vary, from just date YYYMMDD to date time YYYMMDD HHMMSS.
##I'll use an if statement and nchar to convert the two different types to POSIXct
##BIG ASSUMPTION, if no time given, time is set to end of the day
##so as not to miss tag pings several hours prior to retrieval
##for deploy datetime column

##replace the T seperating the date and time in the date/time col with a space
rec.meta.merge$deploy.datetime<-gsub("T"," ",rec.meta.merge$deploy.datetime)
##the for loop will leave date/time as it is if it has a valid time stamp (HH:MM:SS),
##but will assign a time of 23:59:59 if no time is given, and print a warning
for(i in 1:length(rec.meta.merge$Lat))
{
  if(nchar(rec.meta.merge$deploy.datetime[i])==19) ##for date and time
  {
    rec.meta.merge$deploy.datetime[i]<-rec.meta.merge$deploy.datetime[i]
  }
  else if(nchar(rec.meta.merge$deploy.datetime[i])==11) ##for date only
  {
    rec.meta.merge$deploy.datetime[i]<-paste(rec.meta.merge$deploy.datetime[i],"23:59:59",sep="")
    cat("WARNING No time info for Receiver", as.character(rec.meta.merge$rec.serial[i]),
        ", deployment time set at 23:59:59")
  }
  else {cat("Check deployment date time format")}
}
##this converts the date/time col from a character to a time format (POSIX)
rec.meta.merge$deploy.datetime<-as.POSIXct(rec.meta.merge$deploy.datetime,
                                           tryFormats = c("%Y-%m-%d %H:%M:%OS"),
                                           tz="UTC")


##for recovery datetime column
##replace the T seperating the date and time in the date/time col with a space
rec.meta.merge$recover.datetime<-gsub("T"," ",rec.meta.merge$recover.datetime)
##the for loop will leave date/time as it is if it has a valid time stamp (HH:MM:SS),
##but will assign a time of 23:59:59 if no time is given, and print a warning
for(i in 1:length(rec.meta.merge$Lat))
{
  if(nchar(rec.meta.merge$recover.datetime[i])==19)
  {
    rec.meta.merge$recover.datetime[i]<-rec.meta.merge$recover.datetime[i]
  }
  else if(nchar(rec.meta.merge$recover.datetime[i])==11)
  {
    rec.meta.merge$recover.datetime[i]<-paste(rec.meta.merge$recover.datetime[i],"23:59:59",sep="")
    cat("WARNING No time info for Receiver", as.character(rec.meta.merge$rec.serial[i]),
        "retrieval time set at 23:59:59","\n")
  }
  else {cat("Check recovery date time format")}
}
##this converts the date/time col from a character to a time format (POSIX)
rec.meta.merge$recover.datetime<-as.POSIXct(rec.meta.merge$recover.datetime,
                                            tryFormats = c("%Y-%m-%d %H:%M:%OS"),
                                            tz="UTC")

##check to see if the receiver meta data matches the actual data
##if no receivers the same between the two, causes error
if(length(intersect(rec.meta.merge$rec.serial,stew.data$rec.serial))==0)
{print(unique(rec.meta.merge$rec.serial))
  print(unique(stew.data$rec.serial))
  stop("Receiver serials in metadata don't match those in data. Incorrect file read in?")}

##Updated 11-30-2021 Mark Billard
##need to identify which receivers are deployed multiple times in different spots in a year
##put this check in to skip this section if no multiple deployments exist
##otherwise it makes r a full dataframe of NAs that breaks everything (MB 09 2023)
if(any(table(rec.meta.merge$rec.serial)>1)==TRUE)
{
multi.deployed.recs<-names(which(table(rec.meta.merge$rec.serial)>1))

#sub out data from a rec deployed multiple times
test<-stew.data[stew.data$rec.serial %in% multi.deployed.recs,]

t.ls<-list() #empty list to fill in all the t values
for(i in 1:length(multi.deployed.recs)) 
{
  t<-test[test$rec.serial==multi.deployed.recs[i],]
  r<-rec.meta.merge[rec.meta.merge$rec.serial==multi.deployed.recs[i],]
  if(nrow(r)>2){stop("Receiver deployed in more than two spots - this script not meant to handle this, sorry")}
  ##if only two deployments for the rec, then this will work
  t$Station.Name<-ifelse(t$datetimeUTC>r$deploy.datetime[1] & t$datetimeUTC<r$recover.datetime[1],
                         r$Station.Name[1],r$Station.Name[2])
  t<-merge(t,r,by=c("rec.serial","Station.Name"))
  t.ls[[i]]<-t
} #closes for loop

stew.data2<-do.call("rbind",t.ls) #take the list of dataframes of multideployed receivers and bind into on df

#Sub out the compliment of the above data. This df can be straight merged on rec.serial
'%!in%' <- function(x,y)!('%in%'(x,y)) #cust func from stackoverflow
test1<-stew.data[stew.data$rec.serial %!in% multi.deployed.recs,]
#before merging, need to get rid of the existing empty "Station.Name" column in the data to avoid messines
test1$Station.Name<-NULL
stew.data1<-merge(test1,rec.meta.merge,by="rec.serial")

#combine the multideployed rec df, and the compliment of single deployed recs df
stew.data<-rbind(stew.data1,stew.data2)
} #closes if statement
else
{stew.data<-merge(stew.data,rec.meta.merge,by="rec.serial")}



# #get rid of old lat lon columns that are empty
stew.data$Longitude<-NULL
stew.data$Latitude<-NULL

##remove other cols, may need to change this
stew.data$Transmitter.Name<-NULL
stew.data$Transmitter.Serial<-NULL
stew.data$Sensor.Value<-NULL
stew.data$Sensor.Unit<-NULL
###now we can trim data prior/after deploy/retrieval times

#below creates logical vectors of whether or not the tags are early/late
stew.data$earlytag<-(stew.data$datetimeUTC<stew.data$deploy.datetime)
stew.data$latetag<-(stew.data$datetimeUTC>stew.data$recover.datetime)

stew.late.or.early<-stew.data[stew.data$earlytag==TRUE | stew.data$latetag==TRUE,]
stew.data<-stew.data[stew.data$earlytag==FALSE & stew.data$latetag==FALSE,]

##create a summary data frame that contains the number of detections of each tag by each receiver
receivers<-unique(stew.data$rec.serial)
tags<-unique(stew.data$tag.id.code)
stew.sum<-expand.grid(receivers,tags)
names(stew.sum)<-c("receivers","tags")
for(i in 1:length(stew.sum$receivers))
{
  temp<-stew.data[stew.data$rec.serial==stew.sum$receivers[i] &
                  stew.data$tag.id.code==stew.sum$tags[i],]
                        
  stew.sum$tag.count[i]<-length(temp$rec.serial)
  
}

##check to see if the tag meta data matches the actual data
##if no tags the same between the two, causes error
if(length(intersect(tag.meta$TAG_ID_CODE,stew.data$tag.id.code))==0)
{stop("Tag IDs in metadata don't match those in data. Incorrect file read in?")}

##since a tag might be from a previous year and not in the tag.meta, but is still relevant, 
##filter this way to get the total detections for each tag
tag.detects<-aggregate(stew.sum$tag.count,by=list(tags=stew.sum$tags),FUN=sum)
names(tag.detects)<-c("tag","total.detects")

#create a logical vector for if the tag count is too low
#if fewer detections then the threshold, it gets a TRUE, and can be removed based on that
tag.detects$threshold<-tag.detects$total.detects<threshold
too.few.detects<-tag.detects$tag[tag.detects$threshold==TRUE]
stew.data.few.detects<-stew.data[stew.data$tag.id.code %in% too.few.detects,]


####filtering out data that isn't in the tag meta data file
#this essentially finds the tag id codes that are in the data but not in the meta datafile,
#and assigns those tag detections to another dataframe using the other.tags vector
other.tags<-as.factor(setdiff(stew.data$tag.id.code,tag.meta$TAG_ID_CODE))
stew.data.other.tags<-stew.data[stew.data$tag.id.code %in% other.tags,]


##possible detections for undeployed tags: if no retreival time given, assign any detections in that day to this df
##need to coerce the time difference to a double with units days, if the time diff is less than a day the units
##switch to hours, making the less than 1 argument useless
temp<-stew.data[substr(as.character(stew.data$recover.datetime),12,19)=="23:59:59",]
possible.undeployed.tags<-temp[as.double(temp$recover.datetime-temp$datetimeUTC,units="days")<1,]

####the above few sections creates new dataframes of the "bad" data, 
####but the "bad" data still exist in the main dataframe
####the below code removes them, but is kind of hard to interpret
####essentially, the main df and the bad dfs are bound together, so the bad data is duplicated in it
####then, the duplicated data is removed twice, by removing once top down and once bottom up simultaneously
####so rather than removing the only the duplicate, the duplicate and original is removed
####works with any number of duplicates
####!!!need to have fromLAST=TRUE first, FALSE second
stew.data<-rbind(stew.data,
                 stew.data.other.tags,
                 stew.data.few.detects,
                 possible.undeployed.tags
                 ) ##note: late/early tags removed previously from dataframe
stew.data<-stew.data[!duplicated(stew.data,fromLast=TRUE) & !duplicated(stew.data,fromLast=FALSE),]

###assign data to environment for analysis
#the below code uses assign to assign the objects the global environemnt (specified by envir=.GLoalEnv)
# and also changes the name of the files using the earlier specified dataset.name argument,
# which is a character string of the name EG. stewiacke.adults, gaspereau.smolts, whatever
# the paste command adds a suffix identifier to the dataset.name
assign(paste(dataset.name,".VR.summary",sep=""),stew.sum,envir=.GlobalEnv)
assign(paste(dataset.name,".VR.data",sep=""),stew.data,envir=.GlobalEnv)
assign(paste(dataset.name,".VR.other.tags",sep=""),stew.data.other.tags,envir=.GlobalEnv)
assign(paste(dataset.name,".VR.few.detects",sep=""),stew.data.few.detects,envir=.GlobalEnv)
assign(paste(dataset.name,".VR.possible.undeployed.tags",sep=""),possible.undeployed.tags,envir=.GlobalEnv)
assign(paste(dataset.name,".VR.late.or.early",sep=""),stew.late.or.early,envir=.GlobalEnv)

##since the VR2W, VR100, and HR2 cleaner scripts may be run all together or individually,
##I put in a switch (meta== T or F) to output the meta data files
##this way anty script can porduce the meta data dat frame, but if all three are run
##there wont be three copies with slightly different names of the metadata files
if(meta==T) ##only export meta files if meta==T, specified in function
{
  assign(paste(dataset.name,".VR.tag.meta",sep=""),tag.meta,envir=.GlobalEnv)
  assign(paste(dataset.name,".VR.rec.meta",sep=""),rec.meta,envir=.GlobalEnv)  
}

} #end of function

