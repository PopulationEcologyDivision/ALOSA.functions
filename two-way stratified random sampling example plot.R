####plotting two-way stratified random sampling####
#read in count data from times and days
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2023/Data Sheets")
count.data<-read.csv("Vaughan 2023 count data.csv")
count.sub<-count.data[count.data$mon==5 & count.data$day<6,]
count.bad<-data.frame(day<-round(runif(120,1,5),0),
                      time<-round(runif(120,0,2400),0))
#Set up strata:
#Strata 1 from 0000h to 0500h 
#Strata 2 from 0500h to 1045h 
#Strata 3 from 1045h to 1345h 
#Strata 4 from 1345h to 1645h 
#Strata 5 from 1645h to 2100h
#Strata 6 from 2100h to 2400h

plot(NULL,xlab="",ylab="",xlim=c(1,5),ylim=c(0,2400))
mtext("Day",1,3)
mtext("Time of Day",2,3)
#day strata
# segments(1.5,-100,1.5,2500)
# segments(2.5,-100,2.5,2500)
# segments(3.5,-100,3.5,2500)
# segments(4.5,-100,4.5,2500)
# #within day strata
# segments(0,500,6,500)
# segments(0,1045,6,1045)
# segments(0,1345,6,1345)
# segments(0,1645,6,1645)
# segments(0,2100,6,2100)

segments(1.5,-100,1.5,2500,lty=3)
segments(2.5,-100,2.5,2500,lty=3)
segments(3.5,-100,3.5,2500,lty=3)
segments(4.5,-100,4.5,2500,lty=3)
#within day strata
segments(0,500,6,500,lty=3)
segments(0,1045,6,1045,lty=3)
segments(0,1345,6,1345,lty=3)
segments(0,1645,6,1645,lty=3)
segments(0,2100,6,2100,lty=3)
# points(count.sub$day,count.sub$time)
points(count.bad$day,count.bad$time)





