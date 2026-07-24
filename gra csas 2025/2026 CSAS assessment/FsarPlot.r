source("fsar-data.R")
source("plot-timeseries.R")
#need to run model, other lines in masterscript before running next one
#inclduing brps
in.df <- org_fsar_data("long")

## English template
fsar_plot_base(in.df, "English")


