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
severeDf <- uruData[which(severe),]
severeDate <- lubridate::date(NA)
for (nr in c(1:nrow(severeDf))) {
  severeRow <- severeDf[nr,]
  date1 <- severeRow[["F..internación"]]
  date2 <- severeRow[["Ingreso.a.CTI"]]
  date3 <- severeRow[["Ingreso.a.CI"]]
  date4 <- severeRow[["Fecha.de.recuperación.en.BD"]]
  if (is.na(date1) & is.na(date2) & is.na(date3) & is.na(date4)) {
    severeDate[nr] <- severeRow$Inicio.Sínt. + 7
  } else {
    severeDate[nr] <- min(c(date1, date2, date3, date4), na.rm=TRUE)
  }
}
uruData$severeDate <- lubridate::date(NA)
uruData$severeDate[which(severe)] <- severeDate

criticalDf <- uruData[which(critical),]
criticalDate <- lubridate::date(NA)
for (nr in c(1:nrow(criticalDf))) {
  criticalRow <- criticalDf[nr,]
  date2 <- criticalRow[["Ingreso.a.CTI"]]
  date3 <- criticalRow[["Ingreso.a.CI"]]
  date4 <- criticalRow[["Fecha.de.recuperación.en.BD"]]
  if (is.na(date1) & is.na(date2) & is.na(date3) & is.na(date4)) {
    criticalDate[nr] <- criticalRow$Inicio.Sínt. + 10
  } else {
    criticalDate[nr] <- min(c(date1, date2, date3, date4), na.rm=TRUE)
  }
}
uruData$criticalDate <- lubridate::date(NA)
uruData$criticalDate[which(critical)] <- criticalDate

# make and save tidy data frame for later use
processedData <- data.frame(age=ages,
                            dateSymptoms=uruData$Inicio.Sínt.,
                            critical=critical,
                            severe=severe,
                            death=death,
                            dateSevere=uruData$severeDate,
                            dateCritical=uruData$criticalDate)

write.csv(processedData, "../data/public_uruguay_data/processedData.csv",
          row.names=FALSE)

