library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(rriskDistributions)
library(stringr)
library(greta)
library(greta.gp)
source("./functions_auxiliary.R")
source("./functions_analysis_subreporting.R")
source("./age_strat_bayesian_fitting.R")

parameterFile <- "../datos_procesados/1_model_summaries.csv"
modelParams <- read.csv(parameterFile, stringsAsFactors=FALSE)

dynamicsFile <- "../public_uruguay_data/dynamics_uruguay.csv"
dynamicsDf <- read.csv(dynamicsFile, stringsAsFactors=FALSE) %>%
  dplyr::mutate(., date=lubridate::as_date(date))

colnames(dynamicsDf)[2:11] <- binsVec

onset2Deathquartiles <- c(8, 18, 24) # datos para ajustar curva de delay
onset2ICUquartiles <- c(4, 7, 10) # datos para ajustar curva de delay
onset2Hospquartiles <- c(3, 5, 9) # datos para ajustar curva de delay

# get the case dynamics for each age stratum
binsVec <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
             "60-69", "70-79", "80-89", "90+")

casesKnownStrat <- dplyr::select(dynamicsDf, all_of(binsVec)) %>%
  get_fitting_data_strat(., delay_fun_crit)
casesKnownStrat$date_num <- as.numeric(dynamicsDf$date)
casesKnownStrat$date_num <- with(casesKnownStrat, date_num-min(date_num)+1)
casesKnownStrat$date <- dynamicsDf$date

# Fit to deaths
delay_fun_death <- onset2Outcome(onset2Deathquartiles)
deathParams <- dplyr::filter(modelParams, Fit=="Fatality")

slopeDeath <- deathParams$Slope
interceptDeath <- deathParams$Intercept
slopeDeathCI <- with(deathParams, c(SlopeL, SlopeH))
interceptDeathCI <- with(deathParams, c(InterceptL, InterceptH))

deathVec <- dynamicsDf$deaths

predictionDeath <- age_stratified_bayesian_model(casesKnownStrat,
                                                    outcomeVec=deathVec,
                                                    slopeMean=slopeCrit,
                                                    slopeCI=slopeCritCI,
                                                    interceptMean=interceptCrit,
                                                    interceptCI=interceptCritCI)

write.csv(predictionDeath, "./estimate_subreporting_death_stratified.csv",
          row.names=FALSE)



# Fit to critical cases
delay_fun_crit <- onset2Outcome(onset2ICUquartiles)
casesKnownStrat <- dplyr::select(dynamicsDf, all_of(binsVec)) %>%
  get_fitting_data_strat(., delay_fun_crit)
casesKnownStrat$date_num <- as.numeric(dynamicsDf$date)
casesKnownStrat$date_num <- with(casesKnownStrat, date_num-min(date_num)+1)
casesKnownStrat$date <- dynamicsDf$date

criticalParams <- dplyr::filter(modelParams, Fit=="Critical")

slopeCrit <- criticalParams$Slope
interceptCrit <- criticalParams$Intercept
slopeCritCI <- with(criticalParams, c(SlopeL, SlopeH))
interceptCritCI <- with(criticalParams, c(InterceptL, InterceptH))

critVec <- dynamicsDf$newCritical

predictionCritical <- age_stratified_bayesian_model(casesKnownStrat,
                                                    outcomeVec=critVec,
                                                    slopeMean=slopeCrit,
                                                    slopeCI=slopeCritCI,
                                                    interceptMean=interceptCrit,
                                                    interceptCI=interceptCritCI)

write.csv(predictionCritical, "./estimate_subreporting_crit_stratified.csv",
          row.names=FALSE)

# Fit to severe cases
delay_fun_hosp <- onset2Outcome(onset2Hospquartiles)
casesKnownStrat <- get_fitting_data_strat(outcomeDf, delay_fun_hosp)
casesKnownStrat$date_num <- as.numeric(dynamicsDf$day)
casesKnownStrat$date <- dynamicsDf$day

severeParams <- dplyr::filter(modelParams, Fit=="Severe")
slopeSevere <- severeParams$Slope
interceptSevere <- severeParams$Intercept
slopeSevereCI <- with(severeParams, c(SlopeL, SlopeH))
interceptSevereCI <- with(severeParams, c(InterceptL, InterceptH))

severeVec <- dynamicsDf$newHosp

predictionSevere <- age_stratified_bayesian_model(casesKnownStrat,
                                                    outcomeVec=severeVec,
                                                    slopeMean=slopeSevere,
                                                    slopeCI=slopeSevereCI,
                                                    interceptMean=interceptSevere,
                                                    interceptCI=interceptSevereCI)

write.csv(predictionSevere, "./8_estimate_subreporting_severe_stratified.csv",
          row.names=FALSE)

