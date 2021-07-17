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

minDate <- "2020-10-01" 
maxDate <- "2021-01-15" 
ageBins <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
             "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
             "65-69", "70-74", "75-79", "80-84", "85-89", "90+")

outcomeDf <- read.csv("../data/public_uruguay_data/processedData.csv",
                       stringsAsFactors=FALSE) %>%
  dplyr::mutate(., dateSevere=lubridate::date(dateSevere),
                dateCritical=lubridate::date(dateCritical),
                dateSymptoms=lubridate::date(dateSymptoms)) %>%
  caseDetails_2_dynamics(., ageBins=ageBins, minDate=minDate, maxDate=maxDate)

# remove first 10 days of deaths, which are from patients from
# previous of this period
outcomeDf$deaths[c(1:8)] <- 0
outcomeDf$critical[c(1:4)] <- 0
outcomeDf$severe[c(1:3)] <- 0

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
deathDf <- dplyr::mutate(outcomeDf, newOutcome=deaths, newCases=casesTot)

fittingData_death <- get_fitting_data(deathDf, delay_fun_death,
                                baselineOutcomeProp=deathMean)

predictionDeaths <- run_bayesian_model(fittingData_death,
                                 percentageOutcome=deathMean,
                                 percentageOutcomeRange=deathIC)
write.csv(predictionDeaths, "../results/2_estimate_subreporting_deaths.csv",
          row.names=FALSE)

# Fit to critical cases
delay_fun_crit <- onset2Outcome(onset2ICUquartiles)
criticalDf <- dplyr::mutate(outcomeDf, newOutcome=critical, newCases=casesTot)
#
fittingData_crit <- get_fitting_data(criticalDf, delay_fun_crit,
                                baselineOutcomeProp=criticalMean)
predictionCritical <- run_bayesian_model(fittingData_crit,
                                 percentageOutcome=criticalMean,
                                 percentageOutcomeRange=criticalIC)
write.csv(predictionCritical, "../results/2_estimate_subreporting_critical.csv",
          row.names=FALSE)

# Fit to severe cases
delay_fun_hosp <- onset2Outcome(onset2Hospquartiles)
severeDf <- dplyr::mutate(outcomeDf, newOutcome=severe, newCases=casesTot)
#
fittingData_severe <- get_fitting_data(severeDf, delay_fun_hosp,
                                baselineOutcomeProp=severeMean)
predictionSevere <- run_bayesian_model(fittingData_severe,
                                 percentageOutcome=severeMean,
                                 percentageOutcomeRange=severeIC)

write.csv(predictionSevere, "../results/2_estimate_subreporting_severe.csv",
          row.names=FALSE)


