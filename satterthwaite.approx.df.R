#
# Calculates the satterthwaite approximation for degrees of freedom
# when estimating the missing stata so we can also estimate the confidence interval
#
# Inputs:
#        - data set with columns named 'n.periods','n.counts', and sample.var
#
# This function is used by:
#     - onespecies.river.escapement

# This functions uses:
#     - no other user defined functions used.

satterthwaite.approx.df<-function(data){
  missingdata=length(data$n.counts[data$n.counts>1 
                                   & !is.na(data$n.counts)])
  alldata=length(data$n.counts)
  if(!(alldata-missingdata==0)){
    data=data[data$n.counts>1 & !is.na(data$n.counts),]
    warning(paste(alldata-missingdata,"strata were removed due to number of counts less than two",
                  "\n","If missing strata is less than five, impact to CI is likely negligable"))
  }
  N=data$n.periods
  n=data$n.counts
  s2=data$sample.var
  a=((N*(N-n))/n)
  (sum(a*s2)^2)/(sum(((a*s2)^2)/(n-1)))
}