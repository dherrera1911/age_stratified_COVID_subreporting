library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readr)
source("./functions_auxiliary.R")

minDate <- "2020-03-28"
maxDate <- "2021-01-15"

# load data
uruData <- read.csv("../data/public_uruguay_data/case_characteristics_uruguay.csv",
                    stringsAsFactors=FALSE) %>%
  dplyr::filter(., !((Edad=="S.D.") | (Edad=="") | (Inicio.Sínt.=="") |
                     (X.Estuvo.en.otros.países.de.circulación.de.COVID.19.=="S"))) %>%
  dplyr::mutate(., Inicio.Sínt.=lubridate::dmy(Inicio.Sínt.)) %>%
  dplyr::filter(., Inicio.Sínt.<=date(maxDate)) %>%
  tidyr::as_tibble(.)

# convert date columns from strings to dates
datesCols <- c("Ingreso.a.CI", "Ingreso.a.CTI", "F..internación",
               "Fecha.de.recuperación.en.BD")
for (dc in datesCols) {
  dateCol <- uruData[[dc]]
  blankStrInd <- which(dateCol=="")
  dateCol[blankStrInd] <- NA
  dateCol <- lubridate::dmy(dateCol)
  uruData[[dc]] <- dateCol
}

# tidy up ages, which are strings
ages <- str_replace_all(uruData$Edad, " ", "")
ages <- str_replace_all(ages, "a", "")
infants <- grep("m", ages)
ages[infants] <- "0" 
infants <- grep("d", ages)
ages[infants] <- "0" 
ages <- as.integer(ages)

# find critical and severe cases
critical <- (uruData$Requirió.internación.en.CI=="S" |
             uruData$Requirió.internación.en.CTI=="S" |
             uruData$Condición.actual.del.caso=="Fallecido")
severe <- (uruData$Internación=="S" |
           uruData$Requirió.internación.en.CI=="S" |
           uruData$Requirió.internación.en.CTI=="S" |
           uruData$Condición.actual.del.caso=="Fallecido")
death <- uruData$Condición.actual.del.caso=="Fallecido"

# find dates that cases turn critical or severe
uruData <- dplyr::rowwise(uruData) %>%
  dplyr::mutate(., critDate=min(c(Ingreso.a.CI, Ingreso.a.CTI,
                                  Fecha.de.recuperación.en.BD), na.rm=TRUE))
uruData$critDate[which(!critical)] <- NA

uruData <- dplyr::rowwise(uruData) %>%
  dplyr::mutate(., severeDate=min(c(F..internación, Ingreso.a.CI, Ingreso.a.CTI,
                                    Fecha.de.recuperación.en.BD), na.rm=TRUE))
uruData$severeDate[which(!severe)] <- NA

uruData <- dplyr::rowwise(uruData) %>%
  dplyr::mutate(., deathDate=min(c(F..internación, Ingreso.a.CTI), na.rm=TRUE))

# death dates are not present
uruData$deathDate[which(!death)] <- NA

# make and save tidy data frame for later use
processedData <- data.frame(age=ages,
                            dateSymptoms=uruData$Inicio.Sínt.,
                            critical=critical,
                            severe=severe,
                            death=death,
                            dateSevere=uruData$severeDate,
                            dateCritical=uruData$critDate)

write.csv(processedData, "../data/public_uruguay_data/processedData.csv",
          row.names=FALSE)


#############
# From tidy data, construct epidemic dynamics data frame
#############
processedData <- dplyr::filter(processedData, dateSymptoms>=minDate)
dynamicsDates <- date_sequence(minDate=minDate, maxDate=maxDate)

# get severe and critical dynamics
severeDynamics <- events_by_day(processedData$dateSevere,
              minDate=minDate, maxDate=maxDate)
criticalDynamics <- events_by_day(processedData$dateCritical,
              minDate=minDate, maxDate=maxDate)

# get death dynamics form GUIAD web page
urlfile <- "https://raw.githubusercontent.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/master/datos/estadisticasUY.csv"
guiadData <- read_csv(url(urlfile)) %>%
  dplyr::mutate(., date=lubridate::dmy(fecha)) %>%
  dplyr::filter(., (date>=minDate & date <=maxDate))
deathDynamics <- guiadData$cantFallecidos

# get the case dynamics for each age stratum
#binsVec <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
#             "60-69", "70-79", "80-89", "90+")
binsVec <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
             "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
             "65-69", "70-74", "75-79", "80-84", "85-89", "90+")

binInds <- bin_ages(processedData$age, binsVec)
processedData$ageStrat <- factor(binsVec[binInds], levels=binsVec)
dynamicsDf <- data.frame(date=dynamicsDates)
for (strat in binsVec) {
  stratCases <- dplyr::filter(processedData, ageStrat==strat)
  newCasesStrat <- events_by_day(datesVec=stratCases$dateSymptoms,
                                 minDate=minDate, maxDate=maxDate)
  dynamicsDf[[strat]] <- newCasesStrat
}

# total cases
newCasesTot <- events_by_day(datesVec=processedData$dateSymptoms,
                               minDate=minDate, maxDate=maxDate)

# put all together into a full dynamics data frame
dynamicsDf$critical <- criticalDynamics
dynamicsDf$severe <- severeDynamics
dynamicsDf$deaths <- deathDynamics
dynamicsDf$casesTot <- newCasesTot

# save dynamics dataframe
write.csv(dynamicsDf, "../data/public_uruguay_data/dynamics_uruguay_finer.csv",
          row.names=FALSE)

