#
# User defined standard error
#
# This function is used by:
#     - onespecies.river.escapement
# This functions uses:
#     - no other user defined functions used.

user.SE=function(x){sd(x,na.rm=T)/sqrt(sum(!is.na(x)))}