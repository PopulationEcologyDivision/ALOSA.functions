#
# Calculates the variance term for the entire run
#
# Inputs:
#        - data set with columns named 'n.periods' and 'n.counts'
#
# This function is used by:
#     - onespecies.river.escapement

# This functions uses:
#     - no other user defined functions used.

runVAR.term=function(data){
  data$n.periods*
    (data$n.periods-data$n.counts)*
    data$sample.var/data$n.counts
} 