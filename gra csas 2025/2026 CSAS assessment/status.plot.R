##status plots as a function

status.plot<-function(USR,
                      LRP,
                      RR,
                      TRR,
                      u,
                      u.plot.lower,
                      u.plot.upper,
                      ssb,
                      ssb.plot.lower,
                      ssb.plot.upper,
                      years)
{  
  if(length(years)!=length(ssb))
  {stop("ssb length doesn't match years")}
  if(length(years)!=length(u))
  {stop("u length doesn't match years")}
 #inputs:
  nyears<-length(years)
  
  river<-"Gaspereau River"
  species<-"Alewife"
  
  #calcs:
  SSB.prop<-ssb/USR
  #standardize errors too
  SSB.LO<-ssb.plot.lower/USR
  SSB.HI<-ssb.plot.upper/USR
  
  RR.prop<-u/RR
  RR.LO<-u.plot.lower/RR
  RR.HI<-u.plot.upper/RR
  
  #cols
  base.cols<-c("black","cyan")
  var.pal<-colorRampPalette(base.cols)
  cols.vec<-rep(var.pal(nyears/5),each=5) #repeat each of the 7 colours 5 times
  cex.vec<-rep(seq(0.2,1.2,length.out=7),each=5)
  
  #plot
  png("status.plot.GRA.png",width=8.5,height=8.5,units='in',res=200)
  
  par(omi=c(2,1,1,1),mfrow=c(1,1),mar=c(1,2,0,1),las=1)
  layout(matrix(c(1, 2), nrow = 1), widths = c(8, 1))
  plot(SSB.prop,RR.prop,type="p",axes=0,xlab="",ylab="",ylim=c(0,2),xlim=c(0,2),cex=cex.vec)
  
  points(SSB.prop,RR.prop,col=cols.vec,pch=16,cex=cex.vec)
  lines(SSB.prop,RR.prop,lty=2)
  points(SSB.prop[nyears],RR.prop[nyears],pch=19,col="red",cex=max(cex.vec))
  #error bars on final 10 point
  for(i in tail(1:nyears,1))
  {
    arrows(SSB.prop[i],
           RR.LO[i],
           SSB.prop[i],
           RR.HI[i],
           length = 0.05, angle = 90, code = 3)
    arrows(SSB.LO[i],
           RR.prop[i],
           SSB.HI[i],
           RR.prop[i],
           length = 0.05, angle = 90, code = 3)
  }
  
  axis(1)
  axis(2)
  box(lwd=2)
  abline(v=c(1,LRP/USR))
  abline(h=c(1,TRR/RR))
  # abline(h=TRR/RR,lty=2)
  text(SSB.prop[nyears],RR.prop[nyears],"2026",adj=c(-0.2,1.2))
  
  mtext(expression(paste("SSB/SSB"["USR"])),1,line=2.25,cex=1.25)
  mtext(expression(paste("µ/µ"["RR"])),2,line=2.25,cex=1.25,las=0)
  
  mtext("Critical",3,line=1.5,cex=1,adj=0.05)
  mtext("  Zone  ",3,line=0.5,cex=1,adj=0.05) #spaces added so the adj will center the word to the above word
  mtext("Cautious",3,line=1.5,cex=1,adj=0.31)
  mtext("  Zone  ",3,line=0.5,cex=1,adj=0.31)
  mtext("Healthy",3,line=1.5,cex=1,adj=0.77)
  mtext(" Zone  ",3,line=0.5,cex=1,adj=0.77)
  
  text(-0.16,0.9,"TRR",xpd=T)
  
  mtext(river,1,line=5,cex=1.25,las=0,adj=0)
  mtext(species,1,line=6.5,cex=1.25,las=0,adj=0)
  
  mtext(paste("RR = ",RR),1,line=4,cex=1.1,adj=1,las=0)
  mtext(paste("TRR = ",TRR),1,line=5,cex=1.1,adj=1,las=0)
  mtext(paste("USR = ",round(USR/1000,1)," t"),1,line=6,cex=1.1,adj=1,las=0)
  mtext(paste("LRP = ",round(LRP/1000,1)," t"),1,line=7,cex=1.1,adj=1,las=0)

  #legend
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, nyears), xaxs = "i", yaxs = "i")
  rect(xleft = 0, ybottom = 2:(nyears+1), xright = 1, ytop = 1:nyears, col = cols.vec, border = NA)
  axis(side = 4, at=seq(1,nyears,by=5), labels=seq(years[1],max(years),by=5), las = 1)
  
  dev.off()
  }