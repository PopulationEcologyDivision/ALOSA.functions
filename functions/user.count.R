#
# User defined standard row counter
#
# This function is used by:
#     - onespecies.river.escapement
#     - reporting.count

# This functions uses:
#     - no other user defined functions used.

user.count=function(x){length(x[!is.na(x)])}