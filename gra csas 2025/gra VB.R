library(RTMB)
require(ROracle)
library(tidyverse)

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

#bring in data
bio <- dbReadTable(channel, "ALOSA_FISH_BIO_DATA")
age <- dbReadTable(channel, "ALOSA_FISH_AGE_DATA")

bio |> 
  full_join(age, by = c("FISH_ID", "YEAR", "SITE_ID")) |> 
  filter(SITE_ID==3) |>
  filter(YEAR %in% c(1997:2002,2016:2024)) |> 
  select(FORK_LENGTH, CURRENT_AGE) |> 
  filter(!is.na(CURRENT_AGE)) |> 
  filter(!is.na(FORK_LENGTH)) -> age_length_df

dat<-list()
dat$fl<-age_length_df$FORK_LENGTH
dat$age<-age_length_df$CURRENT_AGE

par<-list()
par$log_Linf<-log(31.5)
par$t0<--1
par$log_k<-log(0.2)
par$log_sigma_min<-log(0.5)
# par$log_sigma_max<-log(3)

fit.VB<-function(par)
{
  getAll(par,dat)
  fl<-OBS(fl)
  Linf<-exp(log_Linf)
  t0<-t0
  K<-exp(log_k)
  sigmas<-exp(log_sigma_min)
  
  nll<-0
  
  pred_fl<-Linf*(1-exp(-K*(age-t0)))

  nll<- -sum(dnorm(x=log(fl),mean=log(pred_fl)-sigmas^2/2,sd=sigmas,log=T)) 
  ADREPORT(pred_fl)
  nll
}

obj <- MakeADFun(fit.VB, par, silent=FALSE)
opt <- nlminb(obj$par, obj$fn, obj$gr, control=list(eval.max=1000, iter.max=1000))


Linf<-exp(opt$par[1])
t0<-opt$par[2]
K<-exp(opt$par[3])
#plot
plot(dat$age,dat$fl,xlim=c(0,10),ylim=c(0,50))
age<-0:10
VB.len<-Linf*(1-exp(-K*(age-t0)))
lines(age,VB.len)
