#
#Description:
#
# Inputs:

#
# This function is used by:
# 

# This functions uses:


reference.point.plot<-function(ssnmsy,
                               ssn10,
                               Fupper,
                               Ffully,
                               pointsX,
                               pointsY,
                               label,
                               pospoints=1,
                               rivername,
                               speciesname="Alewife",
                               plotchar=19){
  
  pointsX=pointsX/ssnmsy
  pointsY=pointsY/Fupper
  
  windows(width=8.5,height=9)
  par(omi=c(2,1,1,1),mfrow=c(1,1),mar=c(1,2,0,2),las=1)
  plot(1,1,type="n",axes=0,xlab="",ylab="",ylim=c(0,2),xlim=c(0,max(pointsX,na.rm=T)*1.1))
  axis(1)
  axis(2)
  box(lwd=2)
  crit.caus.boundary=ssn10/ssnmsy
  fully.partial.boundary=Ffully/Fupper
  abline(v=c(1,crit.caus.boundary),lty=5)
  abline(h=c(1,fully.partial.boundary),lty=5)
  
  mtext(expression(paste("Esc/Esc"["USR"])),1,line=2.25,cex=1.25)
  mtext(expression(paste("U/U"["RLL"])),2,line=2.25,cex=1.25,las=0)
  
  mtext("Spawning escapement",3,line=2.5,cex=1.5)
  mtext("critical",3,line=.5,cex=1.15,at=((-0.2+crit.caus.boundary)/2),adj=0.5)
  mtext("cautious",3,line=.5,cex=1.15,at=((1+crit.caus.boundary)/2),adj=0.5)
  mtext("healthy",3,line=.5,cex=1.15,at=(1+max(pointsX,na.rm=T)*1.1)/2,adj=0.5)
  
  
  mtext("Exploitation rate",4,line=4,cex=1.5,las=0)
  mtext("over",4,line=.5,cex=1.25,adj=.75,las=0)
  mtext("exploited",4,line=1.5,cex=1.25,adj=.8,las=0)
  mtext("fully",4,line=.5,cex=1.25,adj=.42,las=0)
  mtext("exploited",4,line=1.5,cex=1.25,adj=.4,las=0)
  mtext("partially",4,line=.5,cex=1.25,adj=.14,las=0)
  mtext("exploited",4,line=1.5,cex=1.25,adj=.13,las=0)
  
  mtext(paste("URR =",Fupper,sep=""),1,line=4,at=(max(pointsX,na.rm=T)*1.1),las=0,adj=1)
  mtext(paste("fully =",Ffully,sep=""),1,line=5,at=(max(pointsX,na.rm=T)*1.1),las=0,adj=1)
  mtext(paste("Esc URP =",ssnmsy,sep=""),1,line=6,at=(max(pointsX,na.rm=T)*1.1),las=0,adj=1)
  mtext(paste("Esc LRP =",ssn10,sep=""),1,line=7,at=(max(pointsX,na.rm=T)*1.1),las=0,adj=1)
  
  mtext(rivername,1,line=5, at=0,las=0,adj=0, cex=2)
  mtext(speciesname,1,line=7, at=0,las=0,adj=0,cex=2)
  
  points(pointsX[!is.na(pointsX)& !is.na(pointsY)],pointsY[!is.na(pointsX)&!is.na(pointsY)], pch=plotchar)
  text(pointsX,pointsY,col="red",pos=pospoints,labels=label)
  
}