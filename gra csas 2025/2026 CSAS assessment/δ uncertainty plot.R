#this script is called by assessment master to produce a plot showing 
#sensitivity of µ to changing δ (fpe, fish passage efficiency)

#plot of multiple µs
#recalc µ with different δ
calc.u<-function(δ)
{
  u.out<-catch.plot[7:length(catch.plot)]/(catch.plot[7:length(catch.plot)]+ssb.plot/δ)
  return(u.out)
}
u.all<-list()
for(i in 1:100)
{
  u.all[[i]]<-calc.u(i/100)
}
u.1970<-sapply(u.all,'[',1) #1970 a single year, needs to be plotted with point
library(viridis)
cols.vec<-viridis(100)
yrs<-1970:2026

png("δ.uncertainty.GRA.png",width=11,height=8.5,units='in',res=200)
par(omi=c(1,1,0.5,1),mfrow=c(1,1),mar=c(1,2,0,1),las=1)
layout(matrix(c(1, 2), nrow = 1), widths = c(8, 1))
plot(yrs,u.plot,type="l",ylim=c(0,1))
for(i in 1:100){lines(yrs,u.all[[i]],col=cols.vec[i])}
abline(h=0.496,lty=2)
lines(yrs,u.plot,lwd=2)
points(rep(1970,100),u.1970,pch=1,col=cols.vec)
points(1970,u.plot[1],pch=16)
mtext("Year",1,line=3,cex=1.5)
mtext("µ",2,line=3,las=2,cex=1.5)
#legend
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
rect(xleft = 0, ybottom = (2:101)/100, xright = 1, ytop = (1:100)/100, col = cols.vec, border = NA)
axis(side = 4, at=seq(0.1,1,by=0.1), labels=seq(0.1,1,by=0.1), las = 1)
mtext("δ",4,line=3,las=2,cex=1.5)
dev.off()


# act.fpe<-ssb.plot/(catch.plot[7:length(catch.plot)]/0.496-catch.plot[7:length(catch.plot)])
# act.fpe<-ifelse(act.fpe>1,1,act.fpe)
# plot(yrs,act.fpe,type="l")
# abline(h=0.758)