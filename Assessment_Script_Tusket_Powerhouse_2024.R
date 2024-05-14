# In season ####
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2024")

# Make sure to save the xlsx sheet that we all use in the sharepoint to a csv.
# It is located in: OneDrive - DFO-MPO\Alosa\Counts\2024.
# I cannot figure out how to access the file path of stuff on sharepoint, so this
# is currently the easiest way I know to update the data

y <- onespecies.river.escapement(
  "Powerhouse 2024 count data.csv",
  fixtime = T,
  downstream.migration = F,
  database = F,
  2024,
  2,
  channel
)

y <- round(y)
n <- dim(y)[1]

# This takes off the latest day, helpful for when it is incomplete.
y <- y[1:n-1, ]

#y$dayofyear<-as.numeric(as.character(y$dayofyear))

print(paste0("Total escapement as of ", y$mon[n], "-", y$day[n], " is ", sum(y$total), sep = ""))

write.csv(y, file = "powerhouse_inseasonsummary.csv", row.names = F)