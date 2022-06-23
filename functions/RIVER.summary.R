#
#Description: Creates a summary of MARFIS data for a specific river. Use any river
# name in the cleaned rivernames csv. Outputs dfs with summary tables and a figure
#  of total catch (KGS) by year 
# 
#
# Inputs: rivername:
#         plot=T
#         writeplot=F:
#         CATCHdata:
#         DNFdata:
#         RENEWdata:
#         plotwidth=7:
#         plotheight=5:
#         plotres=200:

#
# This function is used by:
#            - no user defined functions use this function

# This functions uses:
#         reporting.summary
#         convert.KGS
#         reporting.count


RIVER.summary<-function(rivername,plot=T,writeplot=F,
                        CATCHdata,
                        DNFdata,
                        RENEWdata,
                        plotwidth=7,
                        plotheight=5,
                        plotres=200){
  
  river.allyears=CATCHdata[CATCHdata$RIVERNAME_CLEANED==rivername & 
                             !is.na(CATCHdata$RIVERNAME_CLEANED),]
  #Check that there are no BAIT licences mixed in by accident (i.e.
  # fisher reported on wrong form and it got entered into MARFIS)
  table(river.allyears$LICENCE_TYPE)
  #---
  river.licences=unique(river.allyears$LICENCE_ID)
  
  river.reports<-reporting.summary(CATCHdata,river.allyears,river.licences,
                                   RENEWdata,DNFdata,rivername)
  
  river.allyears=convert.KGS(river.allyears)
  
  annualcatch=aggregate(river.allyears$KGS,by=list(YEAR=river.allyears$YEAR),
                        FUN=sum)
  names(annualcatch)[2]<-"ANNUAL_CATCH_KGS"
  
  reportcounts=reporting.count(river.reports)
  
  annualcatch=merge(annualcatch,
                    reportcounts[,c("YEAR","REPORTING_RATE")],all.x=T)
  
  if(plot==T){
    bp<-barplot(annualcatch$ANNUAL_CATCH~annualcatch$YEAR,
                xlab="YEAR", ylab="Catch (KGS)", 
                ylim=c(0, 1.2*max(annualcatch$ANNUAL_CATCH)))
    text(bp, 0, annualcatch$REPORTING_RATE,cex=0.75,pos=3) 
    mtext(rivername)
    if(writeplot==T){
      jpeg(width=plotwidth,height=plotheight, file=paste("Annual catch -", rivername,".jpeg", sep="")
           ,units='in',res=plotres)
      bp<-barplot(annualcatch$ANNUAL_CATCH~annualcatch$YEAR,
                  xlab="YEAR", ylab="Catch (KGS)", 
                  ylim=c(0, 1.2*max(annualcatch$ANNUAL_CATCH)))
      text(bp, 0, annualcatch$REPORTING_RATE,cex=0.75,pos=3) 
      mtext(rivername)
      dev.off()
    } 
  }
  assign(paste("annualcatch",substr(rivername,1,3),sep="."),annualcatch,envir = .GlobalEnv)
  assign(paste("licencereportsummary",substr(rivername,1,3),sep="."),river.reports,envir = .GlobalEnv)
  assign(paste("reportcounts",substr(rivername,1,3),sep="."),reportcounts,envir = .GlobalEnv)
} 