library(dplyr)
library(tidyr)
library(lubridate)
library(rriskDistributions)
library(stringr)


# receive minimum and maximum dates and return vector with all dates
# in between
date_sequence <- function(minDate, maxDate) {
  nDays <- lubridate::interval(date(minDate), date(maxDate)) / lubridate::days(1)
  allDays <- seq(from=0, to=nDays) + lubridate::ymd(minDate) 
  return(allDays)
}

# turn vectors indicating date of individual ocurrences into
# a vector indicating ocurrences by day
events_by_day <- function(datesVec, minDate=NA, maxDate=NA){
  if (is.na(minDate)) {
    minDate <- min(datesVec)
  }
  if (is.na(maxDate)) {
    maxDate <- max(datesVec)
  }
  datesVec[which(datesVec < minDate)] <- NA
  datesVec[which(datesVec > maxDate)] <- NA
  # make a vector with all dates to be returned
  nDays <- lubridate::interval(date(minDate), date(maxDate)) / lubridate::days(1)
  allDays <- seq(from=0, to=nDays) + lubridate::ymd(minDate) 
  # get count table of the events dates
  eventsByDay <- table(datesVec)
  nEvents <- rep(0, length(allDays))
  nEvents[which(as.character(allDays) %in% names(eventsByDay))] <- eventsByDay
  return(nEvents)
}


# take a vector with numbers and bin them as given in
# binsVec. binsVec can be a character vector indicating
# the ranges as "Xi-Xf" or a numeric vector indicating
# the superior limits of the bins
bin_ages <- function(agesVec, binsVec) {
  if (is.character(binsVec)) {
    ageBins <- strsplit(binsVec, "-") %>%
      lapply(., as.numeric) %>%
      unlist(.)
    supInd <-seq(2, length(ageBins), by = 2)
    ageBins <- ageBins[supInd]
    if (any(is.na(ageBins))) {
      ageBins <- ageBins[-which(is.na(ageBins))]
    }
    ageBins <- c(-1, ageBins, 200)
  } else {
    ageBins <- binsVec
  }
  indVec <- .bincode(agesVec, ageBins)
  return(indVec)
}


# for each age in agesVec, look for its bin in binsVec,
# and assign the corresponding proportion. Return
# a vector with the corresponding proportion for each age
assign_bin_prop <- function(agesVec, binsVec, propVec) {
  ageBin <- bin_ages(agesVec, as.character(binsVec))
  binProp <- propVec[ageBin]
  return(binProp)
}


# Return a vector with the mean age in each bin
mid_bin_age <- function(binsVec) {
  ageBins <- strsplit(binsVec, "-") %>%
    lapply(., as.numeric) %>%
    lapply(., mean) %>%
    unlist(.)
  for (naInd in which(is.na(ageBins))) {
    naVal <- as.numeric(substr(binsVec[naInd], start=1, stop=2))
    ageBins[naInd] <- mean(c(naVal, 90))
  }
  # extract last value
  return(ageBins)
}

# extrapolate bins
fit_to_lit_proportions <- function(proportionDf, newBins, mixed=FALSE) {
  # data frame for prediction
  newMidAge <- mid_bin_age(newBins) 
  ageData <- data.frame(meanAge=newMidAge, age=newBins, study=NA)
  # fit model
  proportionDf <- dplyr::filter(proportionDf, outcome > 0)
  if (mixed) {
    outcomeModel <- lmer(log(outcome) ~ meanAge + (meanAge+0|study) +
                         (1|study), data=proportionDf)
    predictions <- predict(outcomeModel, ageData, re.form=NA)
  } else {
    outcomeModel <- lm(log(outcome) ~ meanAge, data=proportionDf)
  }
  lmout <- as.data.frame(predict(outcomeModel, ageData, interval="confidence"))
  names(lmout) <- c("outcome", "outcomeL", "outcomeH")
  predictDf <- cbind(ageData, lmout)
  return(list(prediction=predictDf, meanParams=coef(outcomeModel),
              ciParams=confint(outcomeModel),
              model=outcomeModel))
}


get_bins_limits <- function(ageBins) {
  ageList <- strsplit(ageBins, "-") %>%
    lapply(., as.numeric)
  ageLow <- NULL
  ageHigh <- NULL
  for (l in c(1:length(ageList))) {
    if (is.na(ageList[[l]])) {
      naVal <- as.numeric(substr(ageBins[l], start=1, stop=2))
      ageList[[l]] <- c(naVal, 300)
    }
    ageLow[l] <- ageList[[l]][1]
    ageHigh[l] <- ageList[[l]][2]
  }
  return(list(lower=ageLow, upper=ageHigh))
}


caseDetails_2_dynamics <- function(casesDf, ageBins, minDate="2020-03-28",
                                   maxDate="2021-01-15") {
  casesDf <- dplyr::filter(casesDf, (dateSymptoms>=minDate) &
                           (dateSymptoms<=maxDate))
  dynamicsDates <- date_sequence(minDate=minDate, maxDate=maxDate)

  severeDynamics <- events_by_day(casesDf$dateSevere,
                minDate=minDate, maxDate=maxDate)
  criticalDynamics <- events_by_day(casesDf$dateCritical,
                minDate=minDate, maxDate=maxDate)

  # get death dynamics form GUIAD web page
  urlfile <- "https://raw.githubusercontent.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/master/datos/estadisticasUY.csv"
  guiadData <- read_csv(url(urlfile)) %>%
    dplyr::mutate(., date=lubridate::dmy(fecha)) %>%
    dplyr::filter(., (date>=minDate & date <=maxDate))
  deathDynamics <- guiadData$cantFallecidos

  binInds <- bin_ages(casesDf$age, ageBins)
  casesDf$ageStrat <- factor(ageBins[binInds], levels=ageBins)
  dynamicsDf <- data.frame(date=dynamicsDates)
  for (strat in ageBins) {
    stratCases <- dplyr::filter(casesDf, ageStrat==strat)
    newCasesStrat <- events_by_day(datesVec=stratCases$dateSymptoms,
                                   minDate=minDate, maxDate=maxDate)
    dynamicsDf[[strat]] <- newCasesStrat
  }

  # total cases
  newCasesTot <- events_by_day(datesVec=casesDf$dateSymptoms,
                                 minDate=minDate, maxDate=maxDate)

  # put all together into a full dynamics data frame
  dynamicsDf$critical <- criticalDynamics
  dynamicsDf$severe <- severeDynamics
  dynamicsDf$deaths <- deathDynamics
  dynamicsDf$casesTot <- newCasesTot
  return(dynamicsDf)
}

