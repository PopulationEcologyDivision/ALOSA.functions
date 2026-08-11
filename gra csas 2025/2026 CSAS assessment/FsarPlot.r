setwd("~/git/ALOSA.functions/gra csas 2025/2026 CSAS assessment")
source("assessment master rough.R")
source("fsar-data.R")
source("plot-timeseries.R")
#need to run model, other lines in masterscript before running next one
#inclduing brps
in.df <- org_fsar_data("long")

## English template
fsar_plot_base(in.df, "English")


