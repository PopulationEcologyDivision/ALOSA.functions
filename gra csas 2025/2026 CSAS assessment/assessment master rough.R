assessment.out<-readRDS("C:/~/git/gra/assessment.out.gra.RData")


####Catch####
#catch is in numbers, but would be better in tons
#catch is provided to us in lbs, just convert back using 0.227kg/fish
catch.old<-c(181,127,109,45,109,100,91,154,127,217,308,387,453,
             426,470,190,89,45,50,31,55,39,209,261,206,280,174
             ) #from Mcintyre et al. 2007, table 1, fishery officer pail count estimates (mt), 1964:1990
catch.new<-assessment.out$dat$abun.df$catch #gives 1991-2024
catch.new<-c(catch.new,660148,432137) #add in 2025, 2026
catch.new<-catch.new*0.227 #convert to kgs
catch.plot<-c(catch.old*1000,catch.new)
#no error term for catch

####SSB####
#get count data where available
gra.count.old<-data.frame(year=1982:1990,
                          esc=c(50400,114800,111100,NA,NA,NA,NA,NA,NA)) #from Jessop and Parker 1988
gra.count<-assessment.out$dat$abun.df[,c(1,4)] #year and count cols
gra.count<-rbind(gra.count.old,gra.count)
gra.count<-rbind(gra.count,c(2025,858917)) #add in 2025
gra.count<-rbind(gra.count,c(2026,716720)) #add in 2025
gra.count$SE<-c(rep(1,3),rep(NA,6), #1982-1984, total counts, and 1985-1990 no counts
                rep(NA,4),1,NA,rep(1,7), #no count or total count 1991-2003
                22226/1.96,59982/1.96,36312/1.96, #2004-2006 divide half the CI by 1.96,
                47000/1.96,NA,39900/1.96, NA, NA,#assume CI +/- 10% 2007, 2009
                31677/1.96,15000/1.96,NA,#2012 2013
                24070/1.96,25221/1.96,40127/1.96,43546/1.96,41881/1.96, NA,#2015-2019
                36504/1.96,36935/1.96,52546/1.96,31210/1.96,39173/1.96,36910/1.96 #2021-2026
                ) 

source("~/git/ALOSA.functions/gra csas 2025/2026 CSAS assessment/GRA.mean.weight.R")
mean.weight.new<-mean(mw.new$weight,na.rm=T)
mean.weight.old<-mean(mw.old$weight)
all.fish.weight<-rbind(mw.old.old,mw.old,mw.new)

gra.count<-merge(gra.count,all.fish.weight[,c(1,2)],all.x=T)
gra.count$weight<-ifelse(is.na(gra.count$weight),
                         ifelse(gra.count$year<=2008,mean.weight.old,mean.weight.new),gra.count$weight)

obs.ssb<-gra.count$esc*gra.count$weight/1000
obs.ssb.SE<-gra.count$SE*gra.count$weight/1000

pred.ssb<-c(rep(NA,9),assessment.out$int_calc_ests$pred_ssb) #NA's added because model predicted 1991-2024, data is 1982-present 
pred.ssb.SE<-c(rep(NA,9),assessment.out$int_calc_stds$pred_ssb)

ssb.plot<-obs.ssb
ssb.plot[is.na(ssb.plot)]<-pred.ssb[which(is.na(ssb.plot))] #fill in missing obs with predictions

ssb.plot.SE<-obs.ssb.SE
ssb.plot.SE[is.na(ssb.plot.SE)]<-pred.ssb.SE[which(is.na(ssb.plot.SE))] #fill in missing obs with predictions

ssb.plot.lower<-ssb.plot-1.96*ssb.plot.SE
ssb.plot.upper<-ssb.plot+1.96*ssb.plot.SE

####exploitation rate####
#done as biomass not count
#include FPE

#error propagation
#step1, use delta method to approximate a normal SE for FPE
FPE.norm.SE<-0.257/abs(1/(1.144*(1.144-1)))

t1<-ssb.plot/0.758 #esc (as SSB)/FPE
t1.SE<-sqrt((ssb.plot.SE/ssb.plot)^2+(FPE.norm.SE/0.758)^2)*t1

u.plot<-catch.plot[19:length(catch.plot)]/(catch.plot[19:length(catch.plot)]+t1) #19:length catch plot gives 1982:2026
#boot strap to get uncertainty in u
u.BS<-list()
for(i in 1:length(t1))
{
  u.BS[[i]]<-catch.plot[i+18]/(catch.plot[i+18]+rnorm(10000,t1[i],t1.SE[i]))
}
u.plot.lower<-sapply(u.BS,quantile,probs=0.025,na.rm=T)
u.plot.upper<-sapply(u.BS,quantile,probs=0.975,na.rm=T)


####Recruits####
#extract data and trim final estimate
log.rec.plot<-assessment.out$log_recs[1:(length(assessment.out$log_recs)-1)]
log.rec.plot.std<-assessment.out$log_recs_std[1:(length(assessment.out$log_recs_std)-1)]
rec.plot<-exp(log.rec.plot) #gives only 1991-2024, remove final bad year
rec.plot.lower<-exp(log.rec.plot-1.96*log.rec.plot.std)
rec.plot.upper<-exp(log.rec.plot+1.96*log.rec.plot.std)

####probability of stock status####
USR<-assessment.out$brps$USR
LRP<-assessment.out$brps$SSBF40*0.5
#2020
pnorm(USR,ssb.plot[30],ssb.plot.SE[30],lower.tail = F)
#2022
pnorm(USR,ssb.plot[32],ssb.plot.SE[32],lower.tail = F)
#all other probs>0.99

#U
TRR<-1-exp(-assessment.out$brps$F40)
RR<-assessment.out$brps$Umsy
prob.df<-data.frame(year=1982:2026,
                    prob.RR=NA,
                    prob.TRR=NA,
                    prob.USR=NA,
                    prob.LRP=NA)
for(i in 1:length(u.BS))
{
  prob.df$prob.RR[i]<-mean(u.BS[[i]]>RR)
  prob.df$prob.TRR[i]<-mean(u.BS[[i]]>TRR)
  prob.df$prob.USR[i]<-pnorm(USR,ssb.plot[i],ssb.plot.SE[i],lower.tail = F)
  prob.df$prob.LRP[i]<-pnorm(LRP,ssb.plot[i],ssb.plot.SE[i],lower.tail = F)
}

source("~/git/ALOSA.functions/gra csas 2025/2026 CSAS assessment/status.plot.R")
status.plot(USR=USR,
            LRP=LRP,
            RR=RR,
            TRR=TRR,
            u=u.plot[10:45],
            u.plot.lower=u.plot.lower[10:45],
            u.plot.upper=u.plot.upper[10:45],
            ssb=ssb.plot[10:45],
            ssb.plot.lower=ssb.plot.lower[10:45],
            ssb.plot.upper=ssb.plot.upper[10:45],
            years=1991:2026)

