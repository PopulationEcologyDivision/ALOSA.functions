####VR100 Receiver import and cleaning####
####NOTES FOR RUNNING FUNCTION####

##data.dir is the fullpath to the folder the data of interest is stored in. data needs to be in csv format
##needs to a file path in quotations

##tag.meta.path is the fullpath to the tag metadata csv file
##needs to a file path in quotations

##the tag meta data CSV may contain an extra four rows of general info at the top
##these can be removed or left there, lines 89-97 handle both scenarios

##the desired prefix for the names of the dataframes produced herein
##needs to a word in quotations, also called a character string
##if the dataset name is specified as: dataset.name="stewiacke" in the function, the dataframes
##produced will be called stewiacke.data, stewiacke.other.tags, etc.

##meta is either true or false
##TRUE (or T) means the recevier adn tag meta data files will be output as dataframes 
##at the end of the script. FALSE (or F) will prevent them from being produced
##one of the three VR2W, VR100, or HR2 scripts should have this as true

####MAIN FUNCTION####
CleanVR100Data<-function(data.dir, #the fullpath to the folder the csv's are stored in
                         tags.meta.path, #the fullpath to the tag metadata csv
                         dataset.name, #the prefix you want your dataframes to be called -"character"
                         meta #True/False value whether to produce meta data dataframes
                         )

{  

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
  if(length(colnames(list.data[[i]]))!=22)
  {stop(paste(list.filenames[i],"doesn't have the correct columns",sep=" "))}
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
stew.data$datetimeUTC<-paste(stew.data$Date,stew.data$Time,sep=" ")
stew.data$datetimeUTC<-as.POSIXct(stew.data$datetimeUTC,tz="UTC") #make sure timezone is UTC, needs to be specified

##Rename ID column
names(stew.data)[colnames(stew.data)=="ID"]<-"tag.id.code"

##Remove the rows that are only GPS
stew.data<-stew.data[stew.data$Type!="GPS",]

#change row names to numbers, otherwise apeear as the csv name, which is annoying
row.names(stew.data)<-seq(1:length(stew.data$tag.id.code))

###Read in tag meta data. No Recevier meta data, not relevant to VR100
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
if(names(tag.meta[2])!="TAG_TYPE")
{warning("Tag metadata file doesn't match standard format. Check tag.meta.path is to correct file")}


##check to see if the tag meta data matches the actual data
##if no tags the same between the two, causes error
if(length(intersect(tag.meta$TAG_ID_CODE,stew.data$tag.id.code))==0)
{stop("Tag IDs in metadata don't match those in data. Incorrect file read in?")}


####filtering out data that isn't in the tag meta data file
#this essentially finds the tag id codes that are in the data but not in the meta datafile,
#and assigns those tag detections to another dataframe using the other.tags vector
other.tags<-as.factor(setdiff(stew.data$tag.id.code,tag.meta$TAG_ID_CODE))
stew.data.other.tags<-stew.data[stew.data$tag.id.code %in% other.tags,]



####the above few sections creates new dataframes of the "bad" data, 
####but the "bad" data still exist in the main dataframe
####the below code removes them, but is kind of hard to interpret
####essentially, the main df and the bad dfs are bound together, so the bad data is duplicated in it
####then, the duplicated data is removed twice, by removing once top down and once bottom up simultaneously
####so rather than removing the only the duplicate, the duplicate and original is removed
####works with any number of duplicates
####!!!need to have fromLAST=TRUE first, FALSE second
stew.data<-rbind(stew.data,
                 stew.data.other.tags
                 ) ##note: late/early tags removed previously from dataframe
stew.data<-stew.data[!duplicated(stew.data,fromLast=TRUE) & !duplicated(stew.data,fromLast=FALSE),]


##fill in the missing GPS info
stew100.data<-stew.data ## assign to new df, so you can have the original with the extra columns,
##and a modified one with matching columns to the VR2W data

stew100.data$Lat<-rep(NA,length(stew100.data$Date))
stew100.data$Lon<-rep(NA,length(stew100.data$Date))
stew100.data$timediff<-rep(NA,length(stew100.data$Date))

for(i in 1:length(stew100.data$Date))
{
  
  if(is.na(stew100.data$Latitude[i])==FALSE)
  {
    stew100.data$Lat[i]<-stew100.data$Latitude[i]
    stew100.data$Lon[i]<-stew100.data$Longitude[i]
  }
  else
  {
    ##This chunk makes a vector of time differences, and removes the time differences for
    ##instances when there is no lat info, making it easy to assign the lat/lon info from the nearest observation (in time)
    ##without assigning NA's when the nearest observation also has no lat/lon info
    ##frequently there are two observations close in time with no lat/lon info
    stew100.data$timediff<-abs(stew100.data$datetimeUTC-stew100.data$datetimeUTC[i])
    ##rmtimediff is a vector of the rows where no lat info exists
    rmtimediff<-which(is.na(stew100.data$Latitude))
    stew100.data$timediff[c(rmtimediff)]<-NA
    
    ##the next two bits grab the lat/lon info from the nearest observation, and assigns it to replace.lat
    ##for whatever reason, the first line of each chunk assigns a vector of NA's with one real value (the desired value)
    ##so the second line is required to remvoe the NAs, before assigning the lat.lon to the new column
    replace.lat<-stew100.data$Latitude[stew100.data$timediff==min(stew100.data$timediff[stew100.data$timediff>0],na.rm=TRUE)]
    replace.lat<-replace.lat[!is.na(replace.lat)]
    stew100.data$Lat[i]<-replace.lat
    
    replace.lon<-stew100.data$Longitude[stew100.data$timediff==min(stew100.data$timediff[stew100.data$timediff>0],na.rm=TRUE)]
    replace.lon<-replace.lon[!is.na(replace.lon)]
    stew100.data$Lon[i]<-replace.lon
    
    cat("Warning: No Lat/Lon info for observation at",
        as.character(stew100.data$datetimeUTC[i]),
        "Lat/Lon info grabbed from observation",
        as.character(round(min(stew100.data$timediff[stew100.data$timediff>0],na.rm=TRUE),0)),
        "seconds away", "\n"
    )
    
    
  }
}


##talk to dave, decide if leaving old columns appropriate, how appropriate is this process
stew100.data$timediff<-NULL



##prepare dataframe for merging to main df, get columns named so they match
stew100.data$Transmitter<-paste(stew100.data$Code.Space,stew100.data$tag.id.code,sep="-")
stew100.data$Station.Name<-rep("VR100",length(stew100.data$Date))
stew100.data$Receiver<-rep("VR100",length(stew100.data$Date))
stew100.data$rec.serial<-rep("VR100",length(stew100.data$Date))
stew100.data$deploy.datetime<-rep(NA,length(stew100.data$Date))
stew100.data$recover.datetime<-rep(NA,length(stew100.data$Date))
stew100.data$download.datetime<-rep(NA,length(stew100.data$Date))
stew100.data$earlytag<-rep(NA,length(stew100.data$Date))
stew100.data$latetag<-rep(NA,length(stew100.data$Date))

##grab only columns of interest
stew100.data<-subset(stew100.data,select=c(rec.serial,
                                         datetimeUTC,
                                         Receiver,
                                         Transmitter,
                                         Station.Name,
                                         tag.id.code,
                                         Lat,
                                         Lon,
                                         deploy.datetime,
                                         recover.datetime,
                                         download.datetime,
                                         earlytag,
                                         latetag
))



###assign data to environment for analysis
#the below code uses assign to assign the objects the global environemnt (specified by envir=.GLoalEnv)
# and also changes the name of the files using the earlier specified dataset.name argument,
# which is a character string of the name EG. stewiacke.adults, gaspereau.smolts, whatever
# the paste command adds a suffix identifier to the dataset.name
assign(paste(dataset.name,".100.data",sep=""),stew.data,envir=.GlobalEnv)
assign(paste(dataset.name,".100.data.merge",sep=""),stew100.data,envir=.GlobalEnv)
assign(paste(dataset.name,".100.other.tags",sep=""),stew.data.other.tags,envir=.GlobalEnv)

##since the VR2W, VR100, and HR2 cleaner scripts may be run all together or individually,
##I put in a switch (meta== T or F) to output the meta data files
##this way anty script can porduce the meta data dat frame, but if all three are run
##there wont be three copies with slightly different names of the metadata files
if(meta==T) ##only export meta files if meta==T, specified in function
{
  assign(paste(dataset.name,".100.tag.meta",sep=""),tag.meta,envir=.GlobalEnv)
}

} #end of function


