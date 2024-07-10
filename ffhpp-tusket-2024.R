library(tidyverse)
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()

# Load in count data from Lake Vaughan and Powerhouse from 2022 - 2024
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River")

ph_2022 <- read_csv("Tusket 2022/Data Sheets/Counts/Powerhouse Count Sheet Cleaned 2022.csv")
ph_2023 <- read_csv("Tusket 2023/Data Sheets/Powerhouse 2023 count data1.csv")
ph_2024 <- read_csv("Tusket 2024/Powerhouse 2024 count data.csv")
ph <- rbind(ph_2022, ph_2023, ph_2024)
ph$location <- "Powerhouse"

vd_2022 <- read_csv("Tusket 2022/Data Sheets/Counts/Vaughan 2022 Count Data - Sheet1.csv")
vd_2022 <- vd_2022 %>% select(-c("...13", "...14")) # remove these weird dangler columns
vd_2023 <- read_csv("Tusket 2023/Data Sheets/Vaughan 2023 count data.csv")
vd_2024 <- read_csv("Tusket 2024/Vaughan 2024 count data.csv")
vd <- rbind(vd_2022, vd_2023, vd_2024)
vd$location <- "Lake Vaughan"

# Join the dataframes
counts <- rbind(vd, ph)

# Do the assessments for each site for each year
write_csv(counts %>% filter(location == "Powerhouse" & year == 2022), file = "Tusket 2024/FFHPP-report/ph_2022.csv")
write_csv(counts %>% filter(location == "Powerhouse" & year == 2023), file = "Tusket 2024/FFHPP-report/ph_2023.csv")
write_csv(counts %>% filter(location == "Powerhouse" & year == 2024), file = "Tusket 2024/FFHPP-report/ph_2024.csv")
write_csv(counts %>% filter(location == "Lake Vaughan" & year == 2022), file = "Tusket 2024/FFHPP-report/vd_2022.csv")
write_csv(counts %>% filter(location == "Lake Vaughan" & year == 2023), file = "Tusket 2024/FFHPP-report/vd_2023.csv")
write_csv(counts %>% filter(location == "Lake Vaughan" & year == 2024), file = "Tusket 2024/FFHPP-report/vd_2024.csv")

# Run the escapement estimation function - need to use partial function for ph_2022 and ph_2023
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2024/FFHPP-report")
escape_vd_2022 <- onespecies.river.escapement("vd_2022.csv", fixtime = T, downstream.migration = F, database = F, 2022, 2, channel)
escape_vd_2023 <- onespecies.river.escapement("vd_2023.csv", fixtime = T, downstream.migration = F, database = F, 2023, 2, channel)
escape_vd_2024 <- onespecies.river.escapement("vd_2024.csv", fixtime = T, downstream.migration = F, database = F, 2024, 2, channel)
escape_ph_2022 <- onespecies.partial.river.escapement("ph_2022.csv", fixtime = F, database = F, 2022, 14, channel)
escape_ph_2023 <- onespecies.river.escapement("ph_2023.csv", fixtime = F, downstream.migration = F, database = F, 2023, 14, channel)
escape_ph_2024 <- onespecies.river.escapement("ph_2024.csv", fixtime = T, downstream.migration = F, database = F, 2024, 14, channel)

escape_vd_2022$year <- 2022
escape_vd_2023$year <- 2023
escape_vd_2024$year <- 2024
escape_ph_2022$year <- 2022
escape_ph_2023$year <- 2023
escape_ph_2024$year <- 2024

escape_vd_2022$location <- "Lake Vaughan"
escape_vd_2023$location <- "Lake Vaughan"
escape_vd_2024$location <- "Lake Vaughan"
escape_ph_2022$location <- "Powerhouse"
escape_ph_2023$location <- "Powerhouse"
escape_ph_2024$location <- "Powerhouse"

# merge the dataframes into a single frame
escapement <- rbind(
  escape_vd_2022,
  escape_vd_2023,
  escape_vd_2024,
  escape_ph_2022,
  escape_ph_2023,
  escape_ph_2024
)

# make all dates have 2024 in them so they plot nicely
escapement$date <- ymd(paste("2024", escapement$mon, escapement$day, sep = "-"))

escapement$year <- as.character(escapement$year)

# Remove the NAs and NaNs from clow and chigh so the plots show the confidence intervals for all points
escapement[is.na(escapement)] <- 0

# split the years into a panel for each

# in the future six panels, with year on x, site y, with
# the ratio used between the sites on the third plot
a <-
escapement %>%
  group_by(year) %>%
  #mutate(total = if_else(is.na(total), 0, total)) %>% 
  ggplot(aes(date, total))+
  facet_wrap(~year, scales = "fixed", ncol = 1)+
  geom_path(
    data = . %>% filter(location == "Lake Vaughan"),
    aes(colour = location),
    alpha = 0.9,
    linewidth = 1.25
    )+
  geom_ribbon(
    data = . %>% filter(location == "Lake Vaughan"),
    aes(ymin = clow, ymax = chigh, fill = location),
    alpha = 0.2
    )+
  geom_path(
    data = . %>% filter(location == "Powerhouse"),
    aes(colour = location),
    alpha = 0.9,
    linewidth = 1.25
  )+
  geom_ribbon(
    data = . %>% filter(location == "Powerhouse"),
    aes(ymin = clow, ymax = chigh, fill = location),
    alpha = 0.5
  )+
  theme_bw() +
  labs(
    #title = "Daily escapement estimates for gaspereau on the Tusket River",
    x = "Date",
    y = "fish / day",
    colour = "Location",
    fill = "Location"
  )+
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week")+
  scale_y_continuous(
    limits = c(0, max(escapement$chigh)),
    breaks = seq(0, max(escapement$chigh), by = 20000)
  )+
  theme(
    legend.position = c(0.9, 0.925),
    legend.background = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_blank()
    )

# Have a plot here where the ratio of counts at PH / VD are shown in the
# same manner as the previous plot
b <-
escapement %>%
  #replace_na(0) %>% 
  #filter(year == 2024) %>% 
  group_by(date, year) %>%
  filter(all(c("Lake Vaughan", "Powerhouse") %in% location)) %>%
  mutate(ratio = total[location == "Powerhouse"] / total[location == "Lake Vaughan"]) %>%
  ggplot(aes(date, ratio * 100)) +
  facet_wrap(~year, scales = "fixed", ncol = 1)+
  geom_line(alpha = 0.75, linewidth = 1)+
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week")+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_blank()
  )+
  labs(
    #title = "Daily escapement rates at Powerhouse as a percentage of those at Lake Vaughan",
    y = "Percent (%)"
  )

library(ggpubr)
ggarrange(a, b, ncol = 2)

# Ratios of escapements
filtered_df <- escapement %>% filter(location %in% c("Powerhouse", "Lake Vaughan"))

result <- filtered_df %>% 
  group_by(year) %>% 
  summarise(
    total_PH = sum(total[location == "Powerhouse"]),
    total_VD = sum(total[location == "Lake Vaughan"])
  )

result <- result %>% mutate(percentage = (total_PH / total_VD) * 100)
result <- result %>% mutate(
  percentage = signif(percentage, 1),
  total_PH = signif(total_PH, 3),
  total_VD = signif(total_VD, 3)
  )

result <- result %>% rename("Year" = year, "Powerhouse" = total_PH, "Lake Vaughan" = total_VD, "%" = percentage)

library(scales)
result$Powerhouse <- comma_format()(result$Powerhouse)
result$`Lake Vaughan` <- comma_format()(result$`Lake Vaughan`)

# summary stats
summary <- 
escapement %>% 
  group_by(location, year) %>% 
  summarise(max = max(total), mean = mean(total)) %>% 
  rename("Year" = year, "Location" = location, "Maximum" = max, "Average" = mean)

summary <- summary %>% mutate(
  Maximum = signif(Maximum, 2),
  Average = signif(Average, 2))

summary$Maximum <- comma_format()(summary$Maximum)
summary$Average <- comma_format()(summary$Average)
