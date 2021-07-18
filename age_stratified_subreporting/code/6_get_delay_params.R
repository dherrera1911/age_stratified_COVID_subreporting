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

minDate <- "2020-11-01" 
maxDate <- "2021-01-15" 
ageBins <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
             "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
             "65-69", "70-74", "75-79", "80-84", "85-89", "90+")

casesDf <- read.csv("../data/public_uruguay_data/processedData.csv",
                       stringsAsFactors=FALSE) %>%
  dplyr::mutate(., dateSevere=lubridate::date(dateSevere),
                dateCritical=lubridate::date(dateCritical),
                dateSymptoms=lubridate::date(dateSymptoms))


severeDf <- dplyr::filter(casesDf, severe)
distSevereDelay <- severeDf %>%
  with(., lubridate::interval(dateSymptoms, dateSevere)/lubridate::days(1))
severeQuants <- quantile(distSevereDelay)

criticalDf <- dplyr::filter(casesDf, critical)
distCriticalDelay <- criticalDf %>%
  with(., lubridate::interval(dateSymptoms, dateCritical)/lubridate::days(1))
criticalQuants <- quantile(distCriticalDelay)


