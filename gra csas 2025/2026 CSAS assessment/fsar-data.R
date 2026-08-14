#' Simulate fisheries data to demonstrate FSAR figures

#' @param format Long or wide data frame?
#'
#' @return A data frame
#' @export
#'
#' @examples
#' sim_fsar_data()
org_fsar_data <- function(format = c("wide", "long")) {
  format <- match.arg(format)
  categories <- c("Catch", "SSB", "Fishing", "Recruitment")
  years <- 1981:2026
  years.catch <- 1964:2026
  years.rec <- 1990:2026
  ## observed catch
  ts1<-c(catch.plot/1000)

  ## predicted SSB
  ts2 <- c(NA,ssb.plot/1000)
  ts2.lo <- c(NA,ssb.plot.lower/1000)
  ts2.hi <- c(NA,ssb.plot.upper/1000)

  ## observed F
  ts3 <- c(NA,u.plot)
  ts3.lo <- c(NA,u.plot.lower)
  ts3.hi <- c(NA,u.plot.upper)
  
  ## precipitated R
  ts4 <- c(rec.plot,NA,NA,NA)
  ts4.lo<-c(rec.plot.lower,NA,NA,NA)
  ts4.hi<-c(rec.plot.upper,NA,NA,NA)
  
  sim.df <-
    rbind(
      data.frame(
        panel.category = rep(categories[1], length(years.catch) * 1),
        year = rep(years.catch, 1),
        ts.name = rep(c("Catch"), each = length(years.catch)),
        ts.value = c(ts1)
      ),
      data.frame(
        panel.category = rep(categories[2], length(years) * 5),
        year = rep(years, 5),
        ts.name = rep(c("SSB", "SSB Low", "SSB High", "USR", "LRP"), each = length(years)),
        ts.value = c(ts2, ts2.lo, ts2.hi, rep(0.8*assessment.out$brps$SSBMSY/1000,length(years)), rep(0.5*assessment.out$brps$SSBF40/1000,length(years)))
      ),
      data.frame(
        panel.category = rep(categories[3], length(years) * 6),
        year = rep(years, 6),
        ts.name = rep(c("µ", "µ Low", "µ High", "Adult M", "RR", "TRR"), each = length(years)),
        ts.value = c(ts3, ts3.lo, ts3.hi, rep(1-exp(-assessment.out$madult),length(years)), rep(assessment.out$brps$TRRupper,length(years)), rep(1-exp(-assessment.out$brps$F40),length(years)))
      ),
      data.frame(
        panel.category = rep(categories[4], length(years.rec) * 3),
        year = rep(years.rec, 3),
        ts.name = rep(c("Age-3 Recruits", "Age-3 Recruits Low", "Age-3 Recruits High"), each = length(years.rec)),
        ts.value = c(ts4, ts4.lo, ts4.hi) / 1E6
      )
    )

  sim.df$panel.category <- factor(sim.df$panel.category, levels = c("Catch", "SSB", "Fishing", "Recruitment"), ordered = TRUE)
  if (format == "long") {
    return(sim.df)
  } else {
    df <- tidyr::pivot_wider(sim.df,
      id_cols = year,
      names_from = ts.name, values_from = ts.value
    )
    return(df)
  }
}
