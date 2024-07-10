#...............................................................................
# This script is designed to run all of the functions needed to calculate
# escapement, run timing, and biological characteristics.
# Each site and will have to be assessed separately:
#...............................................................................

# libraries
library(tidyverse)
library(scales)
require(ROracle)
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()

#Set account name, password, and server
channel = dbConnect(
  DBI::dbDriver("Oracle"),
  oracle.username.GASP,
  oracle.password.GASP,
  "PTRAN",
  believeNRows = FALSE
  ) 

#i.forgot.the.siteIDs(channel)

# Season setup ####
# Only run at beginning of season!
#blank.datasheets(seed=222,startmonth=3,endmonth=6,startday=15,rivername="Vaughan",
#                 year=2024,recordtime=T,speciesID=T,strata=6,samplesperstrata=4)
#blank.datasheets(seed=223,startmonth=3,endmonth=6,startday=15,rivername="Powerhouse",
#                 year=2024,recordtime=T,speciesID=T,strata=6,samplesperstrata=4)
#make.count.filename.textfile("Powerhouse 2024 count data.csv","Secret_Ladder",2024)
#make.count.filename.textfile("Vaughan 2024 count data.csv","Vaughan",2024)

setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2024")

# Make sure to save the xlsx sheet that we all use in the sharepoint to a csv.
# It is located in: OneDrive - DFO-MPO\Alosa\Counts\2024.
# I cannot figure out how to access the file path of stuff on sharepoint, so this
# is currently the easiest way I know to update the data

# LAKE VAUGHAN ####
x <- onespecies.river.escapement(
  "Vaughan 2024 count data.csv",
  fixtime = T,
  downstream.migration = F,
  database = F,
  2024,
  2,
  channel
  )

x <- round(x)
n <- dim(x)[1]

# This takes off the latest day, helpful for when it is incomplete.
#x <- x[1:n-1, ]
# x$dayofyear<-as.numeric(as.character(x$dayofyear))

print(paste0("Total escapement as of ", x$mon[n], "-", x$day[n], " is ", sum(x$total), sep = ""))

write.csv(x, file = "inseasonsummary.csv", row.names = F)

write_csv(x, "R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2024/2024_counts.csv")

# We want to annotate the plot with the total escapement and I use commas for easier reading
total_count <- signif(sum(x$total), digits = 3)
total_count_comma <- label_comma()(total_count)

# Load old count data from Lake Vaughan for comparison with this year's and then
# add the data from this year to this data set
vaughan_wide <- read_csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/count_data_tusket_multi_year_wide.csv")
count_2024 <- read_csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2024/2024_counts.csv")
count_2024 <- count_2024 %>% select(mon, day, total) %>% rename(month = mon)
vaughan_wide <- left_join(vaughan_wide, count_2024, by = c("month", "day"))
vaughan_wide <- vaughan_wide %>% rename("2024" = total)

# Convert this wide data set to a long format for plotting
vaughan_long <- vaughan_wide %>%
  pivot_longer(
    cols = "2014":"2024",
    names_to = "year",
    values_to = "count"
  )

# Make the dates for all observations, even those prior to 2024, as 2024.
# This is on purpose to make them overlap on the x-axis. The actual year
# of the observation is retained in the "year" column
vaughan_long$date <- ymd(paste("2024", vaughan_long$month, vaughan_long$day, sep = "-"))

# Add location to the data-frame so we can compare to the Powerhouse site
vaughan_long$location <- "Lake Vaughan"

# Extract the last full day for which data are available
vaughan_last_date <- make_datetime(year = 2024, month = tail(x$mon, 1), day = tail(x$day, 1))

# POWERHOUSE ####
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

# Add estimates from 2024 to a csv from the y variable created above; 
# I couldn't figure out a more effcient way (don't @ me)
write_csv(y, "R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2024/2024_counts_powerhouse.csv")

# Load in data from Powerhouse previous years
powerhouse <- read_csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2024/powerhouse_counts_pre_2024.csv")

# Get total count for annotating the plot
total_count <- signif(sum(y$total), digits = 3)
total_count_comma <- label_comma()(total_count)

# Add column for year
y$year <- 2024

# Add the estimates for this year to the data from previous years
powerhouse <- bind_rows(powerhouse, y)
powerhouse <- select(powerhouse, -sd, -clow, -chigh)

# This makes the dates for all observations, even those prior to 2024, as 2024.
# As above, this is on purpose to make them overlap when they plot
powerhouse$date <- make_date(year = 2024, month = powerhouse$mon, day = powerhouse$day)
powerhouse$year <- as.character(powerhouse$year)

# Add location to the dataframe so we can compare it to the Lake Vaughan data
powerhouse$location <- "Powerhouse"

# Get the last date for annotating the plot
pwr_last_date <-  make_datetime(year = 2024, month = tail(powerhouse$mon, 1), day = tail(powerhouse$day, 1))

# Get total count
pwr_total_count <- signif(sum(y$total), digits = 3)
pwr_total_count_comma <- label_comma()(pwr_total_count)

# Combine data-frames to show difference between the sites. We need to rename
# some of the columns because they are labelled differently and drop a column
# that is no longer useful
powerhouse <- powerhouse %>% rename(month = mon, count = total)
powerhouse <- powerhouse %>% select(-dayofyear)
combined <- bind_rows(vaughan_long, powerhouse, .id = "source")

# We only plot the observations since 2019 so as not to overwhelm the viewer
combined %>%
  filter(year >= 2019) %>%
  mutate(count = if_else(is.na(count) & date < "2024-04-20", 0, count)) %>% 
  group_by(year) %>% 
  ggplot(aes(date, count)) +
  geom_path(data = . %>% filter(year != 2024), aes(colour = year), alpha = 0.25, linewidth = 1) +
  geom_path(data = . %>% filter(year == 2024), aes(colour = year, linetype = location), alpha = 0.9, linewidth = 2) +
  annotate(
    "text",
    x = as.Date("2024-04-23"),
    y = 1.33e5,
    label = paste("Total for Lake Vaughan (solid) as of", vaughan_last_date, "is", total_count_comma),
    size = 4.5
  ) +
  annotate(
    "text",
    x = as.Date("2024-04-23"),
    y = 1.27e5,
    label = paste("Total for Powerhouse (dashed) as of", pwr_last_date, "is", pwr_total_count_comma),
    size = 4.5
  ) +
  theme_bw() +
  labs(
    title = "Estimated escapement for gaspereau on the Tusket River",
    x = "Date",
    y = "gaspereau / day",
    colour = "Year",
    linetype = "Location"
  ) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  scale_y_continuous(
    labels = scales::comma,
    n.breaks = 8) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.title.x = element_blank()
  )

combined %>%
  filter(year == 2024) %>%
  group_by(date) %>%
  filter(all(c("Lake Vaughan", "Powerhouse") %in% location)) %>%
  mutate(ratio = count[location == "Powerhouse"] / count[location == "Lake Vaughan"]) %>%
  ggplot(aes(date, ratio)) +
  geom_path(alpha = 0.75, linewidth = 1)+
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week")
