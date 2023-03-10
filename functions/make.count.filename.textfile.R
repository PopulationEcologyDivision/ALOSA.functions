#
#Description: Function to convert a count file to a text file of filenames
#to allow the convert3.sh script on the pi to convert video from .h264 to .mp4
#
# Inputs:
#count file
#
# This function is used by:
#Assessment Scripts 
#
# This functions uses:



make.count.filename.textfile<-function(filename,site.name,year)
{

out<-read.csv(filename)

LOC<-site.name

MM<-paste("0",as.character(out$mon[1]),sep="")

DD<-paste("0",as.character(out$day[1]),sep="")

YYYY<-year

TIME<-paste("0",as.character(out$time[1]),sep="")

file.list<-NULL

for(i in 1:nrow(out))
{
  
  MM<-paste("0",as.character(out$mon[i]),sep="")
  
  

  if(nchar(out$day[i])==1)
  {
    DD<-paste("0",as.character(out$day[i]),sep="")
  } else {
    DD<-as.character(out$day[i])
  }
  
  
  if(nchar(out$time[i])==1)
  {
  TIME<-paste("000",as.character(out$time[i]),sep="")
  }else if(nchar(out$time[i])==2)
  {
  TIME<-paste("00",as.character(out$time[i]),sep="")
  }else if(nchar(out$time[i])==3)
  {
    TIME<-paste("0",as.character(out$time[i]),sep="")
  } else {
    TIME<-as.character(out$time[i])
  }
  
  
  filename<-paste(LOC,"-",DD,MM,YYYY,"-",TIME,".h264",sep="")
  
  file.list[i]<-filename
  
}

write.table(file.list, file=paste(y,year,"Count filenames.txt",sep=" "),
            quote=F,row.names=F,col.names=F)

}
