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

