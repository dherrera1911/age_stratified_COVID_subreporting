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

minDate <- "2020-10-01" 
maxDate <- "2021-01-15" 

parameterFile <- "../data/outcome_rates/1_model_summaries.csv"
modelParams <- read.csv(parameterFile, stringsAsFactors=FALSE)

dynamicsFile <- "../data/public_uruguay_data/dynamics_uruguay.csv"
dynamicsDf <- read.csv(dynamicsFile, stringsAsFactors=FALSE) %>%
  dplyr::mutate(., date=lubridate::as_date(date)) %>%
  dplyr::filter(., (date>=minDate) & (date<=maxDate))

# remove first 10 days of deaths, which are from patients from
# previous of this period
dynamicsDf$deaths[c(1:10)] <- 0

# Change the names of the columns to the age strata
binsVec <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
             "60-69", "70-79", "80-89", "90+")
colnames(dynamicsDf)[2:11] <- binsVec

# quartiles for delays
onset2Deathquartiles <- c(8, 18, 24) # datos para ajustar curva de delay
onset2ICUquartiles <- c(4, 7, 10) # datos para ajustar curva de delay
onset2Hospquartiles <- c(3, 5, 9) # datos para ajustar curva de delay

#################
# Fit to deaths
#################
delay_fun_death <- onset2Outcome(onset2Deathquartiles)
deathParams <- dplyr::filter(modelParams, Fit=="Fatality")

slopeDeath <- deathParams$Slope
interceptDeath <- deathParams$Intercept
slopeDeathCI <- with(deathParams, c(SlopeL, SlopeH))
interceptDeathCI <- with(deathParams, c(InterceptL, InterceptH))

deathVec <- dynamicsDf$deaths


casesKnownStrat <- dplyr::select(dynamicsDf, all_of(binsVec)) %>%
  get_fitting_data_strat(., delay_fun_death)
casesKnownStrat$date_num <- as.numeric(dynamicsDf$date)
casesKnownStrat$date_num <- with(casesKnownStrat, date_num-min(date_num)+1)
casesKnownStrat$date <- dynamicsDf$date

predictionDeath <- age_stratified_bayesian_model(casesKnownStrat,
                                                    outcomeVec=deathVec,
                                                    slopeMean=slopeDeath,
                                                    slopeCI=slopeDeathCI,
                                                    interceptMean=interceptDeath,
                                                    interceptCI=interceptDeathCI)

write.csv(predictionDeath, "../results/estimate_subreporting_death_stratified.csv",
          row.names=FALSE)


#################
# Fit to critical cases
#################
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

write.csv(predictionCritical, "../results/3_estimate_subreporting_crit_stratified.csv",
          row.names=FALSE)


#################
# Fit to severe cases
#################
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

write.csv(predictionSevere, "../results/3_estimate_subreporting_severe_stratified.csv",
          row.names=FALSE)

