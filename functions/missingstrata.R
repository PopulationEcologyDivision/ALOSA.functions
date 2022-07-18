#
# Estimates and fills in the missing strata count, n.counts, sample variance,
#  standard error and standard deviation. Returns a data set with no missing counts
#
# Inputs:
#        - data set with columns named 'dayofyear','strata', 'mean', "sample.var' 
#         'sd', 'n.counts', 'se'
#        - start.end is the output from the first.last.day function, or
#          a vector with the start and end dates as DOY. Length = 2
#
# This function is used by:
#     - onespecies.river.escapement

# This functions uses:
#     - no other user defined functions used.

missingstrata<-function(data,start.end,n.strata){
  
  #dataframe showing day numbers and strata 
  junk99<-data.frame(dayofyear=factor(start.end[1]:start.end[2],
                                      levels=c(as.character(start.end[1]:start.end[2]))),
                    strata=factor(rep(1:n.strata,each=length(start.end[1]:start.end[2])),
                                   levels=c(as.character(1:n.strata)))
                     
                     ) 
  
  ##data from the database has rows with no counts removed. previously this script
  ##did not fill in the missing times. merging the junk99 object with the count
  ##object creates blank rows for any missing counts, allowing the next section
  ##to fill in the missing times with the model results
  data<-merge(junk99,data,by=c("strata","dayofyear"),all.x=T)
  
  glm.result<-glm(mean ~ 
                    as.factor(dayofyear)+
                    as.factor(strata), 
                  family="poisson",
                  data=data[!data$mean<0,])

  #GLM used to predict mean counts for strata with no counts, 
  # predicting counts for uncountable time units
  junk99.prediction<-predict(glm.result, newdata=junk99, type="response") 
  var.slope=summary(lm(data$sample.var[data$mean>0] ~ 
                         data$mean[data$mean>0]))$coefficients[2,1] #~ = is modeled by, lm = linear model, giving summary of relationship
  sd.slope=summary(lm(data$sd[data$mean>0] ~ 
                        data$mean[data$mean>0]))$coefficients[2,1] # ~ = is modeled by, lm = linear model, giving summary of relationship
  
  #for each strata in a day with missing mean count,use junk99 prediction
  #for each strata in a day with missing variance counts, 
  #    use the slope of the variance vs mean linear model to 
  #    predict variance
  #for each strata in a day with missing standard deviation counts, 
  #    use the slope of the standard deviation vs mean linear model to 
  #    predict standard deviation
  # fill in missing n.counts as 3
  for(i in 1:length(data$mean))
  {
    if(is.na(data$mean[i]))
    {
      data$mean[i]=junk99.prediction[i]
      data$n.counts[i]=3
    }
    
    if(is.na(data$sample.var[i]))
    {
      data$sample.var[i]=(junk99.prediction[i]*var.slope)
    }
    
    if(is.na(data$sd[i]))
    {
      data$sd[i]=(junk99.prediction[i]*sd.slope)
    }
    if(is.na(data$se[i]))
    {
      data$se[i]=data$sd[i]/sqrt(data$n.counts[i])
    }
  }  
  return(data)
}