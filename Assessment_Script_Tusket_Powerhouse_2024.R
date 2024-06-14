# In season ####
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2024")

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
#y <- y[1:n-1, ]

print(paste0("Total escapement as of ", y$mon[n], "-", y$day[n], " is ", sum(y$total), sep = ""))

write.csv(y, file = "powerhouse_inseasonsummary.csv", row.names = F)