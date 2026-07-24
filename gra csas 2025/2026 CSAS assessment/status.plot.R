##status plots as a function

status.plot<-function(USR,
                      LRP,
                      LRR,
                      TRR,
                      year,
                      removals,
                      ssb,
                      RR.label,
                      file.name)
{  
 #inputs:
  yy<-year
  u<-removals
  ssb<-ssb
  
  river<-"Gaspereau River"
  species<-"Alewife"
  
  ##########################
  
  #calcs:
  SSB.prop<-ssb/USR
  # TRR.prop<-TRR/RRL
  TRR.prop<-u/LRR
  png(file.path('figures',paste0(file.name,'.png',sep="")),width=8.5,height=11,units='in',res=200)
  # windows(width=8.5,height=11)
  par(omi=c(2,1,1,1),mfrow=c(1,1),mar=c(1,2,0,2),las=1)
  
  plot(SSB.prop,TRR.prop,type="n",axes=0,xlab="",ylab="",ylim=c(0,2),xlim=c(0,2))
  
  points(SSB.prop,TRR.prop)
  points(SSB.prop[1],TRR.prop[1],pch=19,col="green")
  points(SSB.prop[length(SSB.prop)],TRR.prop[length(SSB.prop)],pch=19,col="red")
  lines(SSB.prop,TRR.prop)
  
  axis(1)
  axis(2)
  box(lwd=2)
  abline(v=c(1,LRP/USR),lty=5)
  abline(h=c(1,TRR/LRR),lty=5)
  
  mtext(expression(paste("SSB/SSB"["USR"])),1,line=2.25,cex=1.25)
  mtext(expression(paste("RR/LRR")),2,line=2.25,cex=1.25,las=0)
  
  mtext("Spawner Biomass",3,line=2.5,cex=1.5)
  mtext("critical",3,line=.5,cex=1,adj=0.2)
  mtext("cautious",3,line=.5,cex=1,adj=0.4)
  mtext("healthy",3,line=.5,cex=1,adj=0.7)
  
  
  mtext(RR.label,4,line=4,cex=1.5,las=0)
  mtext("too high",4,line=.5,cex=1,adj=.75,las=0)
  mtext("",4,line=1.5,cex=1,adj=.8,las=0)
  mtext("acceptable",4,line=.5,cex=1,adj=.42,las=0)
  mtext("range",4,line=1.5,cex=1,adj=.42,las=0)
  mtext("below",4,line=.5,cex=1,adj=.10,las=0)
  mtext("range",4,line=1.5,cex=1,adj=.1,las=0)
  
  
  mtext(river,1,line=5,cex=1.25,las=0,adj=0)
  mtext(species,1,line=6.5,cex=1.25,las=0,adj=0)
  
  
  mtext(paste("LRR = ",LRR),1,line=4,cex=1.1,adj=1,las=0)
  mtext(paste("TRR = ",TRR),1,line=5,cex=1.1,adj=1,las=0)
  mtext(paste("SSB USR = ",round(USR/1000,1)," MT"),1,line=6,cex=1.1,adj=1,las=0)
  mtext(paste("SSB LRP = ",round(LRP/1000,1)," MT"),1,line=7,cex=1.1,adj=1,las=0)
  
  #text(0.07,1.2,"1982-\n84",col="red",cex=0.85)
  #text(0.23,1.63,"1997",pos=4,col="red",cex=0.85)
  #text(0.6,0.62,"2001",pos=1,col="red",cex=0.85)
  #text(1.1,1.23,"2015-16",pos=4,col="red",cex=0.85)
  #text(2.65,0.88,"2018",pos=3,col="red",cex=0.85)
  #text(0.8,1.47,"2013",col="red",cex=0.85)
  #arrows(0.65,1.45,0.43,1.37,length=0.1,col="red",cex=0.85)
  dev.off()
  }