library(tidyverse)

# plots ########################################################################
# convert weights to kgs
source("C:/Users/graylo/Documents/git/ALOSA.functions/functions/convert.KGS.R")

catch <- convert.KGS(catch)

catch.nb <- catch[catch$PROVINCE == "NB" & catch$YEAR >= 2019, ]

catch.nb <- catch.nb %>% 
  filter(!is.na(YEAR)) %>% 
  group_by(LICENCE_ID, GEAR_DESCRIPTION)

nb.sum <- catch.nb %>% 
  group_by(YEAR, GEAR_DESCRIPTION) %>% 
  summarise(
    catch_sum_kgs = sum(KGS)
  )

nb.sum$GEAR_DESCRIPTION <- reorder(nb.sum$GEAR_DESCRIPTION, -nb.sum$catch_sum_kgs)

# nb.sum %>% 
#   ggplot(aes(x = YEAR, catch_sum_kgs, y = catch_sum_kgs/1000))+
#   geom_bar(
#     stat = "identity",
#     aes(fill = GEAR_DESCRIPTION),
#     position = position_dodge(width = 0.9)
#     )+
#   theme_bw()+
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#     )+
#   labs(
#     title = "Total landed mass for Southwest NB by gear type",
#     x = "Year",
#     y = "Landings (mt)",
#     fill = "Gear"
#   )

# with some gear types removed
nb.sum %>% 
  filter(!grepl("UNKNOWN", GEAR_DESCRIPTION)) %>% 
  filter(!grepl("SQUARE NET", GEAR_DESCRIPTION)) %>% 
  na.omit() %>% 
  ggplot(aes(x = YEAR, catch_sum_kgs, y = catch_sum_kgs/1000))+
  geom_bar(
    stat = "identity",
    aes(fill = GEAR_DESCRIPTION),
    colour = "black",
    position = position_dodge(width = 0.9)
  )+
  theme_bw()+
  scale_fill_viridis_d(direction = -1)+
  theme(
    #panel.background = element_rect(fill = "lightgrey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )+
  labs(
    title = "Total landed mass for Southwest NB by gear type",
    x = "Year",
    y = "Landings (mt)",
    fill = "Gear type"
  )

nb.sum.gear <- catch.nb %>%
  group_by(YEAR, LICENCE_ID, GEAR_DESCRIPTION) %>% 
  summarise(
    catch_sum_kgs = sum(KGS)
  )

nb.sum.gear <- nb.sum.gear %>% filter(!grepl("UNKNOWN", GEAR_DESCRIPTION))
nb.sum.gear <- nb.sum.gear %>% filter(!grepl("SQUARE NET", GEAR_DESCRIPTION))
nb.sum.gear <- nb.sum.gear %>% na.omit()

for (i in unique(nb.sum.gear$GEAR_DESCRIPTION)) {
  
  nb.p <-
    nb.sum.gear %>% 
    filter(GEAR_DESCRIPTION == i) %>% 
    ggplot(aes(as.integer(YEAR), catch_sum_kgs/1000))+ # convert to metric tonnes
    geom_line()+
    geom_point()+
    facet_wrap(~LICENCE_ID)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
    ggtitle(paste("Total landed mass for Southwest NB with", i))+
    labs(
      #title = "Total landed mass for Southwest NB",
      x = "Year",
      y = "Landings (mt)"
    )
  
  print(nb.p)
  # ggsave(
  #   paste("plot_", i, ".png", sep = ""), plot = p,
  #   path = "C:/Users/graylo/Documents/plots")
  
}

# Did they fish? ###############################################################
source("C:/Users/graylo/Documents/git/ALOSA.functions/functions/convert.KGS.R")
catch <- convert.KGS(catch)
lic.vars <- c("LICENCE_ID", "YEAR", "RIVERNAME_CLEANED", "COUNTY", "KGS")
nb.lic <- catch[catch$PROVINCE == "NB", lic.vars]
nb.dnf <- didnotfish[didnotfish$LICENCE_ID %in% nb.lic$LICENCE_ID, ]
#nb.dnf <- catch[didnotfish$LICENCE_ID %in% nb.lic$LICENCE_ID, ]

nb.dnf.data <- nb.dnf %>%
  group_by(YEAR, LICENCE_ID) %>% 
  mutate(NIL_REPORT_FLAG = if_else(NIL_REPORT_FLAG == "Y", "DNF", "FISHED")) %>% 
  reframe(DNF = NIL_REPORT_FLAG)

# identify duplicates
remove <-
  nb.dnf %>% 
  summarise(n = n(), .by = c(YEAR, LICENCE_ID)) %>% 
  filter(n > 1L)

# remove those licence numbers for now
nb.dnf.cleaned <- nb.dnf.data[!(nb.dnf.data$LICENCE_ID %in% remove$LICENCE_ID), ]

nb.dnf.tab <- nb.dnf.cleaned %>% 
  pivot_wider(
    names_from = YEAR,
    values_from = DNF
  )


nb.dnf.tab$LICENCE_ID <- sort(as.integer(nb.dnf.tab$LICENCE_ID))

library(gt)

nb.table <-
  nb.dnf.tab %>% 
  gt(
    rowname_col = "YEAR"
  )

nb.table %>%
  tab_header(
    title = md("Licences from southwest NB reporting **Did Not Fish** from 2008 to 2023")
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  # tab_style(
  #   style = cell_borders(
  #     sides = c("top", "bottom", "left", "right"),
  #     color = "black",
  #     weight = px(1),
  #     style = "solid"
  #   ),
  # #   locations = cells_body()
  # ) %>% 
  data_color(
    columns = unique(nb.dnf.cleaned$YEAR),
    palette = c("coral1", "darkolivegreen")
  )

# for loop to make table with catch values for FISHED
dnf <- nb.dnf.cleaned

catch_sum <- catch %>% 
  filter(LICENCE_ID %in% dnf$LICENCE_ID) %>% 
  group_by(YEAR, LICENCE_ID) %>% 
  summarise(
    KGS = sum(KGS)
  )
catch_sum$KGS <- round(catch_sum$KGS)

# FOR LOOP WOULD GO HERE SOMEHWERE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merged_df <- merge(dnf, catch_sum, by = c("YEAR", "LICENCE_ID"), all = TRUE)

merged_df$DNF[merged_df$DNF == "FISHED"] <- merged_df$KGS[merged_df$DNF == "FISHED"]

merged_df <- merged_df %>% 
  select(-KGS)

merged_df <- merged_df %>% 
  pivot_wider(
    names_from = YEAR,
    values_from = DNF
  )

merged_df$LICENCE_ID <- sort(as.integer(merged_df$LICENCE_ID))

merged.table <- merged_df %>% gt(rowname_col = "YEAR")

# merged.table %>%
#   data_color(
#     columns = 2008:2023,
#     colors = if_else(YEAR == "DNF", "green", "red")
#   ) %>% 
#   tab_header(
#     title = md("Licences from southwest NB reporting **Did Not Fish** from 2008 to 2023")
#   ) %>% 
#   opt_align_table_header(align = "left")

# Did they report?

# I want a dataframe with licence_id, year, and a varaible for report status:
# good, errors, did-not-report