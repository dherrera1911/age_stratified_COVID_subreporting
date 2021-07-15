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
source("./bayesian_subreporting_fitting.R")

outcomeDf <- read.csv("../public_uruguay_data/dynamics_uruguay.csv",
                      stringsAsFactors=FALSE) %>%
  as_tibble(.) %>%
  dplyr::mutate(., newCases=casesTot, date=lubridate::date(date)) %>%
  dplyr::select(., date, newCases, critical, severe, deaths)

# parameters for fitting
deathMean <- 0.68
deathIC <- c(0.51, 0.90)
criticalMean <- 1.28
criticalIC <- c(0.96, 1.7)
severeMean <- 3.3
severeIC <- c(2.6, 4.3)

onset2Deathquartiles <- c(8, 18, 24) # datos para ajustar curva de delay
onset2ICUquartiles <- c(4, 7, 10) # datos para ajustar curva de delay
onset2Hospquartiles <- c(3, 5, 9) # datos para ajustar curva de delay

# Fit to deaths
delay_fun_death <- onset2Outcome(onset2Deathquartiles)
deathDf <- dplyr::mutate(outcomeDf, newOutcome=deaths)

fittingData_death <- get_fitting_data(deathDf, delay_fun_death,
                                baselineOutcomeProp=deathMean)
predictionDeaths <- run_bayesian_model(fittingData_death,
                                 percentageOutcome=deathMean,
                                 percentageOutcomeRange=deathIC)

# Fit to critical cases
delay_fun_crit <- onset2Outcome(onset2ICUquartiles)
criticalDf <- dplyr::mutate(outcomeDf, newOutcome=critical)
#
fittingData_crit <- get_fitting_data(criticalDf, delay_fun_crit,
                                baselineOutcomeProp=criticalMean)
predictionCritical <- run_bayesian_model(fittingData_crit,
                                 percentageOutcome=criticalMean,
                                 percentageOutcomeRange=criticalIC)

# Fit to severe cases
delay_fun_hosp <- onset2Outcome(onset2Hospquartiles)
severeDf <- dplyr::mutate(outcomeDf, newOutcome=severe)
#
fittingData_severe <- get_fitting_data(severeDf, delay_fun_hosp,
                                baselineOutcomeProp=severeMean)
predictionSevere <- run_bayesian_model(fittingData_severe,
                                 percentageOutcome=severeMean,
                                 percentageOutcomeRange=severeIC)

saveRDS(predictionDeaths, "../results/2_subreportingEstimate_deaths.RDS")
saveRDS(predictionCritical, "../results/2_subreportingEstimate_critical.RDS")
saveRDS(predictionSevere, "../results/2_subreportingEstimate_severe.RDS")

