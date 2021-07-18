library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readr)
library(zoo)
source("./functions_auxiliary.R")

maxDate <- c("2020-09-18")
minDate <- c("2020-03-25")

# get death dynamics form GUIAD web page
urlfile <- "https://raw.githubusercontent.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/master/datos/estadisticasUY.csv"
guiadData <- read_csv(url(urlfile)) %>%
  dplyr::mutate(., date=lubridate::dmy(fecha))

#########
# Plot early deaths
#########

plotDeaths <- guiadData %>%
  dplyr::filter(., date <= maxDate) %>%
  #ggplot(., aes(x=date, y=cantFallecidos)) +
  #geom_point() +
  ggplot(., aes(x=date, y=acumFallecidos)) +
  geom_line() +
  theme_bw() +
  theme(strip.background=element_rect(fill="white", color="white"),
        strip.text=element_text(face="bold"),
        legend.position="top",
        panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour = "black"),
        axis.line.x=element_line(size=0.5, linetype="solid"),
        axis.line.y=element_line(size=0.5, linetype="solid"),
        legend.title=element_blank()) +
  xlab("Date") +
  ylab("Number of deaths")

ggsave("../data/deaths_uru1.png", plotDeaths, width=8, height=6, units="cm")

# load data
uruData <- read.csv("../data/public_uruguay_data/processedData.csv",
                    stringsAsFactors=FALSE)

#########
# Plot early deaths + severe
#########
severeDynamics <- events_by_day(uruData$dateSevere,
              minDate=minDate, maxDate=maxDate)
criticalDynamics <- events_by_day(uruData$dateCritical,
              minDate=minDate, maxDate=maxDate)
deathDynamics <- with(guiadData, cantFallecidos[date<=maxDate])

type <- c(rep("Severe", length(severeDynamics)),
          rep("Critical", length(criticalDynamics)),
          rep("Deaths", length(deathDynamics)))
type <- factor(type, levels=c("Severe", "Critical", "Deaths"))

dynamicsDf <- data.frame(Outcome=type,
                         date=date_sequence(minDate, maxDate),
                         cumCount=c(cumsum(severeDynamics),
                                 cumsum(criticalDynamics),
                                 cumsum(deathDynamics)))

plotOutcomes <- dynamicsDf %>%
  dplyr::filter(., Outcome != "Critical") %>%
  droplevels(.) %>%
  #ggplot(., aes(x=date, y=cantFallecidos)) +
  #geom_point() +
  ggplot(., aes(x=date, y=cumCount, color=Outcome)) +
  geom_line() +
  theme_bw() +
  theme(strip.background=element_rect(fill="white", color="white"),
        strip.text=element_text(face="bold"),
        legend.position="top",
        panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour = "black"),
        axis.line.x=element_line(size=0.5, linetype="solid"),
        axis.line.y=element_line(size=0.5, linetype="solid"),
        legend.title=element_blank()) +
  xlab("Date") +
  ylab("Number of outcomes")

ggsave("../data/deaths_uru2.png", plotOutcomes, width=8, height=6, units="cm")


#########

################
# Plot later death
################
maxDate <- "2021-01-15"
minDate <- "2020-09-18"

plotDeaths <- guiadData %>%
  dplyr::filter(., date <= maxDate & date >= minDate) %>%
  #ggplot(., aes(x=date, y=cantFallecidos)) +
  #geom_point() +
  ggplot(., aes(x=date, y=cantFallecidos)) +
  geom_line() +
  theme_bw() +
  theme(strip.background=element_rect(fill="white", color="white"),
        strip.text=element_text(face="bold"),
        legend.position="top",
        panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour = "black"),
        axis.line.x=element_line(size=0.5, linetype="solid"),
        axis.line.y=element_line(size=0.5, linetype="solid"),
        legend.title=element_blank()) +
  xlab("Date") +
  ylab("Number of deaths")

#########
# Plot changing IFR
#########
stratifiedDyn <- read.csv("../data/public_uruguay_data/dynamics_uruguay_finer.csv",
                          stringsAsFactors=FALSE)

parameterFile <- "../data/outcome_rates/1_model_summaries.csv"
modelParams <- read.csv(parameterFile, stringsAsFactors=FALSE)

# get the case dynamics for each age stratum
#binsVec <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
#             "60-69", "70-79", "80-89", "90+")

binsVec <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
             "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
             "65-69", "70-74", "75-79", "80-84", "85-89", "90+")

meanAges <- mid_bin_age(binsVec)

ifrSlope <- with(modelParams, Slope[Fit=="Fatality"])
ifrIntercept <- with(modelParams, Intercept[Fit=="Fatality"])

IFRs <- exp(meanAges*ifrSlope+ifrIntercept)

stratifiedDyn_cut <- filter(stratifiedDyn, date<=maxDate)
#caseMat <- as.matrix(stratifiedDyn_cut[2:11])
caseMat <- as.matrix(stratifiedDyn_cut[2:20])
sumCases <- rowSums(caseMat)

ifrDyn <- (caseMat %*% IFRs)/sumCases
while (any(is.na(ifrDyn))) {
  naIFR <- which(is.na(ifrDyn))
  ifrDyn[naIFR] <- ifrDyn[naIFR-1]
}

ifrDynSmooth <- zoo::rollmean(ifrDyn, k=7, fill=NA)

ifrDynDf <- data.frame(date=date(stratifiedDyn_cut$date),
                           ifr=ifrDynSmooth)

plotIFR <- ifrDynDf %>%
  ggplot(., aes(x=date, y=ifr)) +
  geom_line() +
  theme_bw() +
  theme(strip.background=element_rect(fill="white", color="white"),
        strip.text=element_text(face="bold"),
        legend.position="top",
        panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour = "black"),
        axis.line.x=element_line(size=0.5, linetype="solid"),
        axis.line.y=element_line(size=0.5, linetype="solid"),
        legend.title=element_blank()) +
  xlab("Date") +
  ylab("Estimated IFR (%)")

ggsave("../data/ifr_uru_dyn1.png", plotIFR, width=8, height=6, units="cm")


################
# Plot later death and positivity
################
maxDate <- "2021-01-30"
minDate <- "2020-11-01"

plotDeaths_late <- guiadData %>%
  dplyr::filter(., date <= maxDate & date >= minDate) %>%
  ggplot(., aes(x=date, y=cantFallecidos)) +
  geom_point() +
  #ggplot(., aes(x=date, y=acumFallecidos)) +
  #geom_line() +
  theme_bw() +
  theme(strip.background=element_rect(fill="white", color="white"),
        strip.text=element_text(face="bold"),
        legend.position="top",
        panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour = "black"),
        axis.line.x=element_line(size=0.5, linetype="solid"),
        axis.line.y=element_line(size=0.5, linetype="solid"),
        legend.title=element_blank()) +
  xlab("Date") +
  ylab("Number of deaths") +
  ggtitle("Deaths")

ggsave("../data/deaths_uru_late.png", plotDeaths_late,
       width=11, height=5, units="cm")

plotPositivity_late <- guiadData %>%
  dplyr::filter(., date <= maxDate & date >= minDate) %>%
  ggplot(., aes(x=date, y=Positividad*100)) +
  geom_line() +
  theme_bw() +
  theme(strip.background=element_rect(fill="white", color="white"),
        strip.text=element_text(face="bold"),
        legend.position="top",
        panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour = "black"),
        axis.line.x=element_line(size=0.5, linetype="solid"),
        axis.line.y=element_line(size=0.5, linetype="solid"),
        legend.title=element_blank()) +
  xlab("Date") +
  ylab("Positive tests (%)") +
  ggtitle("Positivity")

ggsave("../data/positivity_uruguay_late.png", plotPositivity_late,
       width=11, height=5, units="cm")


################
# Plot later IFR for cases, severe and lethal patients
################
# reference IFR date
refDate <- "2020-12-01"

stratifiedDyn_late <- filter(stratifiedDyn, date<=maxDate & date>=minDate)
#caseMat <- as.matrix(stratifiedDyn_late[2:11])
caseMat <- as.matrix(stratifiedDyn_late[2:20])
sumCases <- rowSums(caseMat)

ifrDyn <- (caseMat %*% IFRs)/sumCases
while (any(is.na(ifrDyn))) {
  naIFR <- which(is.na(ifrDyn))
  ifrDyn[naIFR] <- ifrDyn[naIFR-1]
}

ifrDynSmooth <- zoo::rollmean(ifrDyn, k=7, fill=NA)
ifrDynDf <- data.frame(date=date(stratifiedDyn_late$date), ifr=ifrDynSmooth)

# severe stratified dynamic
uruData_severe <- dplyr::filter(uruData, severe) %>%
  dplyr::filter(., (dateSymptoms>=minDate) & (dateSymptoms<=maxDate))

uruData_death <- dplyr::filter(uruData, death) %>%
  dplyr::filter(., (dateSymptoms>=minDate) & (dateSymptoms<=maxDate))

# get the case dynamics for each age stratum
#binsVec <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
#             "60-69", "70-79", "80-89", "90+")

# add column with age bin of person
binInds <- bin_ages(uruData_severe$age, binsVec)
uruData_severe$ageStrat <- factor(binsVec[binInds], levels=binsVec)
binInds <- bin_ages(uruData_death$age, binsVec)
uruData_death$ageStrat <- factor(binsVec[binInds], levels=binsVec)

dynamicsDf_severe <- data.frame(date=date_sequence(minDate, maxDate))
dynamicsDf_death <- data.frame(date=date_sequence(minDate, maxDate))
for (strat in binsVec) {
  stratCases <- dplyr::filter(uruData_severe, ageStrat==strat)
  newCasesStrat <- events_by_day(datesVec=stratCases$dateSymptoms,
                                 minDate=minDate, maxDate=maxDate)
  dynamicsDf_severe[[strat]] <- newCasesStrat
  stratCases <- dplyr::filter(uruData_death, ageStrat==strat)
  newCasesStrat <- events_by_day(datesVec=stratCases$dateSymptoms,
                                 minDate=minDate, maxDate=maxDate)
  dynamicsDf_death[[strat]] <- newCasesStrat
}

caseMat_sev <- as.matrix(dynamicsDf_severe[2:20])
sumCases_sev <- rowSums(caseMat_sev)
caseMat_death <- as.matrix(dynamicsDf_death[2:20])
sumCases_death <- rowSums(caseMat_death)

ifrDyn_sev <- (caseMat_sev %*% IFRs)/sumCases_sev
ifrDyn_death <- (caseMat_death %*% IFRs)/sumCases_death
while (any(is.na(ifrDyn_sev))) {
  naIFR <- which(is.na(ifrDyn_sev))
  ifrDyn_sev[naIFR] <- ifrDyn_sev[naIFR-1]
}

while (any(is.na(ifrDyn_death[-1]))) {
  naIFR <- which(is.na(ifrDyn_death))
  if (any(naIFR==1)) {
    naIFR[naIFR==1] <- 3
  }
  ifrDyn_death[naIFR] <- ifrDyn_death[naIFR-1]
}

ifrDynSmooth_severe <- zoo::rollmean(ifrDyn_sev, k=7, fill=NA)
ifrDynSmooth_death <- zoo::rollmean(ifrDyn_death, k=7, fill=NA)

ifrDynDf_severe <- data.frame(date=date(dynamicsDf_severe$date),
                              ifr=ifrDynSmooth_severe)
ifrDynDf_death <- data.frame(date=date(dynamicsDf_death$date),
                              ifr=ifrDynSmooth_death)

ifrDynDf_severe$population <- "Severe cases"
ifrDynDf_death$population <- "Fatal cases"
ifrDynDf$population <- "Cases"

refIFR1 <- with(ifrDynDf, ifr[date==refDate])
ifrDynDf$ifrRel <- (ifrDynDf$ifr-refIFR1)/refIFR1
refIFR2 <- with(ifrDynDf_severe, ifr[date==refDate])
ifrDynDf_severe$ifrRel <- (ifrDynDf_severe$ifr-refIFR2)/refIFR2
refIFR3 <- with(ifrDynDf_death, ifr[date==refDate])
ifrDynDf_death$ifrRel <- (ifrDynDf_death$ifr-refIFR3)/refIFR3

ifrDynDf_late <- rbind(ifrDynDf, ifrDynDf_severe, ifrDynDf_death)

plotIFR_late <- ifrDynDf_late %>%
  dplyr::filter(., date>=refDate) %>%
  dplyr::filter(., population!="Severe cases") %>%
  ggplot(., aes(x=date, y=ifrRel*100, color=population)) +
  geom_line() +
  theme_bw() +
  theme(strip.background=element_rect(fill="white", color="white"),
        strip.text=element_text(face="bold"),
        legend.position="top",
        panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour = "black"),
        axis.line.x=element_line(size=0.5, linetype="solid"),
        axis.line.y=element_line(size=0.5, linetype="solid"),
        legend.title=element_blank()) +
  xlab("Date") +
  ylab("IFR change (%)")

ggsave("../data/ifr_uru_dyn_severe.png", plotIFR_late,
       width=10, height=6, units="cm")


