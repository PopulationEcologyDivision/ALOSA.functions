setwd("~/git/ALOSA.functions/gra csas 2025/2026 CSAS assessment")
source("assessment master rough.R")
setwd("~/git/ALOSA.functions/gra csas 2025/2026 CSAS assessment")
source("fsar-data.R")
source("plot-timeseries.R")

in.df <- org_fsar_data("long")

## English template
fsar_plot_base(in.df, "English")


