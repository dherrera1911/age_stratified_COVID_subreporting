library(lubridate)
library(gridExtra)
library(ggpubr)
source("./plotting_functions.R")
source("./functions_analysis_subreporting.R")
source("./functions_auxiliary.R")


########################################################
## Plot unstratified subreporting estimates
########################################################
subreportDeath <- readRDS("../results/2_subreportingEstimate_deaths.RDS") %>%
  dplyr::mutate(., date=lubridate::as_date(date))
subreportCrit <- read.csv("../results/2_subreportingEstimate_critical.RDS") %>%
  dplyr::mutate(., date=lubridate::as_date(date))
subreportSevere <- read.csv("../results/2_subreportingEstimate_severe.RDS") %>%
  dplyr::mutate(., date=lubridate::as_date(date))

subreportDeathPlot <- plot_subreporting(subreportDeath, title="Deaths")
subreportCritPlot <- plot_subreporting(subreportCrit, title="Critical")
subreportSeverePlot <- plot_subreporting(subreportSevere, title="Severe")
estimateSubreport <- gridExtra::grid.arrange(subreportDeathPlot,
                                             subreportCritPlot,
                                             subreportSeverePlot, ncol=3)

ggsave("../graficas/4_estimado_instantaneo_ITISE.png", estimateSubreport,
       width=20, height=8, units="cm")

subreportCritShort <- readRDS("../datos_procesados/4_subreportingEstimate_critical_shorter.RDS")
subreportSevereShort <- readRDS("../datos_procesados/4_subreportingEstimate_severe_shorter.RDS")
subreportCritShortPlot <- plot_subreporting(subreportCritShort,
                                       title = "Estimacion con criticos")
subreportSevereShortPlot <- plot_subreporting(subreportSevereShort,
                                         title = "Estimacion con severos")
estimateSubreportShort <- gridExtra::grid.arrange(subreportCritShortPlot,
                                                  subreportSevereShortPlot,
                                                  ncol = 2)

ggsave("../graficas/4_estimado_instantaneo_corto.png", estimateSubreportShort,
       width=20, height=8, units="cm")


#######################################################
# 4 Plot estimated real dynamics
#######################################################
#daysCut <- 6
#caseDynamics <- readRDS("../datos_procesados/1_outcomeDynamics.RDS") %>%
#  dplyr::rename(., date = day) %>%
#  dplyr::mutate(., date = date(date))
#
#realDynCrit <- merge(caseDynamics, subreportCrit, by = "date") %>%
#  dplyr::mutate(., caseEst = newCases/estimate, caseEstH = newCases/lower,
#                caseEstL = newCases/upper) %>%
#  as_tibble(.)
#realDynSevere <- merge(caseDynamics, subreportSevere, by = "date") %>%
#  dplyr::mutate(., caseEst = newCases/estimate, caseEstH = newCases/lower,
#                caseEstL = newCases/upper) %>%
#  as_tibble(.)
#
## remove last daysCut days, to prevent distortions from symptoms backlog
#realDynCrit <- realDynCrit[c(1:(nrow(realDynCrit)-daysCut)),]
#realDynSevere <- realDynSevere[c(1:(nrow(realDynSevere)-daysCut)),]
#
#realDynCritPlot <- plot_estimated_dyn(realDynCrit, meanWindow=3,
#                                      title = "Estimacion con criticos")
#realDynSeverePlot <- plot_estimated_dyn(realDynSevere, meanWindow=3,
#                                      title = "Estimacion con severos")
#absSubreportCritPlot <- plot_abs_subreport(realDynCrit, meanWindow=3,
#                                      title = "Estimacion con criticos")
#absSubreportSeverePlot <- plot_abs_subreport(realDynSevere, meanWindow=3,
#                                      title = "Estimacion con severos")
#
#estimatedCases <- gridExtra::grid.arrange(realDynCritPlot, realDynSeverePlot,
#                                          ncol = 1)
#absoluteSubreport <- gridExtra::grid.arrange(absSubreportCritPlot,
#                                             absSubreportSeverePlot,
#                                             ncol = 1)
#
#ggsave("../graficas/4_estimado_casos_reales.png", estimatedCases,
#       width = 16, height = 12, units = "cm")
#ggsave("../graficas/4_estimado_casos_no_detectados.png", absoluteSubreport,
#       width = 16, height = 12, units = "cm")
#
########################################################
## 5 Plot simulated dynamics
########################################################
#simsResults <- readRDS("../datos_procesados/5_simulations.RDS") %>%
#  as_tibble(.)
#simsResultsShort <- readRDS("../datos_procesados/5_simulations_shorter.RDS") %>%
#  as_tibble(.)
#
## plot data standard
#simsPlots <- list()
#infPlots <- list()
#estimatePlots <- list()
#plottingSamples <- list()
#estimatedDyn <- list()
#for (f in c(1:length(unique(simsResults$funcInfInd)))) {
#  simsPlots[[f]] <- list()
#  infDf <- dplyr::filter(simsResults, funcInfInd==f)
#  for (s in c(1:length(unique(infDf$funcSRInd)))) {
#    simsPlots[[f]][[s]] <- list()
#    infSRDf <- dplyr::filter(infDf, funcSRInd==s)
#    for (r in c(1:length(unique(infSRDf$nRep)))) {
#      infSRDf_r <- dplyr::filter(infSRDf, nRep==r)
#      simsPlots[[f]][[s]][[r]] <- plot_subreport_sim(infSRDf_r)
#      if (r == 1) {
#        plottingSamples[[length(plottingSamples)+1]] <- simsPlots[[f]][[s]][[r]]
#        infPlots[[length(infPlots)+1]] <- plot_infections_sim(infSRDf_r)
#        estimatePlots[[length(estimatePlots)+1]] <- plot_infections_sim(infSRDf_r,
#                                                                   plotEst=TRUE)
#      }
#    }
#  }
#}
#
#simsSamplesPlot <- ggpubr::ggarrange(plotlist=plottingSamples, ncol=5, nrow=3,
#                                     common.legend=TRUE)
#examplesInfections <- ggpubr::ggarrange(plotlist=infPlots, ncol=5, nrow=3,
#                                        common.legend=TRUE)
#infectionsEstimates <- ggpubr::ggarrange(plotlist=estimatePlots, ncol=5, nrow=3,
#                                        common.legend=TRUE)
#
#simsSamplesPlot <- ggpubr::annotate_figure(simsSamplesPlot,
#                top = text_grob("Estimacion subreporte simulaciones",
#                                color = "black", face = "bold", size = 14),
#                left = text_grob("% infecciones reportadas",
#                                 color = "black", rot = 90))
#examplesInfections <- ggpubr::annotate_figure(examplesInfections,
#                top = text_grob("Epidemias simuladas",
#                                color = "black", face = "bold", size = 14),
#                left = text_grob("Número de casos",
#                                 color = "black", rot = 90))
#infectionsEstimates <- ggpubr::annotate_figure(infectionsEstimates,
#                top = text_grob("Estimacion de infecciones en epidemias simuladas",
#                                color = "black", face = "bold", size = 14),
#                left = text_grob("Número de casos",
#                                 color = "black", rot = 90))
#
#ggsave("../graficas/5_subreporte_simulado.png", simsSamplesPlot,
#       width = 24, height = 17, units = "cm")
#ggsave("../graficas/5_dinamicas_simuladas.png", examplesInfections,
#       width = 24, height = 17, units = "cm")
#ggsave("../graficas/5_estimaciones_metodo_simulacion.png", examplesInfections,
#       width = 24, height = 17, units = "cm")
#
#
## plot data with shorter temporal correlation
#simsPlotsShort <- list()
#plottingSamplesShort <- list()
#infPlotsShort <- list()
#estimatePlotsShort <- list()
#infFuncs <- unique(simsResultsShort$funcInfInd)
#for (f in c(1:length(infFuncs))) {
#  simsPlotsShort[[f]] <- list()
#  ff <- infFuncs[f]
#  infDf <- dplyr::filter(simsResultsShort, funcInfInd==ff)
#  for (s in c(1:length(unique(infDf$funcSRInd)))) {
#    simsPlotsShort[[f]][[s]] <- list()
#    infSRDf <- dplyr::filter(infDf, funcSRInd==s)
#    for (r in c(1:length(unique(infSRDf$nRep)))) {
#      infSRDf_r <- dplyr::filter(infSRDf, nRep==r)
#      simsPlotsShort[[f]][[s]][[r]] <- plot_subreport_sim(infSRDf_r)
#      if (r == 1) {
#        plottingSamplesShort[[length(plottingSamplesShort)+1]] <- simsPlotsShort[[f]][[s]][[r]]
#        infPlotsShort[[length(infPlotsShort)+1]] <- plot_infections_sim(infSRDf_r)
#        estimatePlotsShort[[length(estimatePlotsShort)+1]] <- plot_infections_sim(infSRDf_r,
#                                                                   plotEst=TRUE)
#      }
#    }
#  }
#}
#
#simsSamplesShortPlot <- ggpubr::ggarrange(plotlist=plottingSamplesShort,
#                                          ncol=5, nrow=3, common.legend=TRUE)
#infectionsEstimatesShort <- ggpubr::ggarrange(plotlist=estimatePlotsShort,
#                                          ncol=5, nrow=3, common.legend=TRUE)
#
#simsSamplesShortPlot <- ggpubr::annotate_figure(simsSamplesShortPlot,
#                top = text_grob("Estimacion subreporte simulaciones",
#                                color = "black", face = "bold", size = 14),
#                left = text_grob("% infecciones reportadas",
#                                 color = "black", rot = 90))
#infectionsEstimatesShort <- ggpubr::annotate_figure(infectionsEstimatesShort,
#                top = text_grob("Estimacion de infecciones en epidemias simuladas",
#                                color = "black", face = "bold", size = 14),
#                left = text_grob("Número de casos",
#                                 color = "black", rot = 90))
#
#ggsave("../graficas/5_subreporte_simulado_short.png", simsSamplesShortPlot,
#       width = 24, height = 17, units = "cm")
#ggsave("../graficas/5_estimaciones_metodo_simulacion_short.png",
#       infectionsEstimatesShort, width = 24, height = 17, units = "cm")
#
## plot summary of estimates
#simPerformancePlot <- plot_simulation_fit_summary(simsResults)
#simPerformancePlotShort <- plot_simulation_fit_summary(simsResultsShort)
#ggsave("../graficas/5_rendimiento_simulaciones.png", simPerformancePlot,
#       width = 24, height = 17, units = "cm")
#
#
########################################################
## 7 Plot predicted outcomes vs actual outcomes
########################################################
#predictionDf <- readRDS("../datos_procesados/7_expected_outcomes.RDS")
#criticalPrediction <- predictionDf$critical %>%
#  dplyr::mutate(., smoothOutcomeRate = zoo::rollmean(outcomeInTodaysCases/cases,
#                                 k=5, fill=NA), newOutcome = newCritical)
#severePrediction <- predictionDf$severe %>%
#  dplyr::mutate(., smoothOutcomeRate = zoo::rollmean(outcomeInTodaysCases/cases,
#                                 k=5, fill=NA), newOutcome = newHosp)
#
#critPlot <- plot_estimated_outcomes(criticalPrediction,
#                                    title="Predicción de críticos") +
#  ylab("Casos críticos")
#severePlot <- plot_estimated_outcomes(severePrediction,
#                                      title="Predicción de severos") +
#  ylab("Casos severos")
#
#predictionPlot <- ggpubr::ggarrange(plotlist=list(critPlot, severePlot),
#                                          ncol=1, nrow=2, common.legend=TRUE)
#
#ggsave("../graficas/7_prediccion_hospitalaria.png", predictionPlot,
#       width=12, height=7)
#
#
#adjustedCriticalPred <- dplyr::rename(subreportCritStrat, day = date) %>%
#  merge(., criticalPrediction, by="day") %>%
#  dplyr::mutate(., expectedOutcomes = expectedOutcomes / estimate)
#adjustedSeverePred <- dplyr::rename(subreportSevereStrat, day = date) %>%
#  merge(., severePrediction, by="day") %>%
#  dplyr::mutate(., expectedOutcomes = expectedOutcomes / estimate)
#critPlotAdj <- plot_estimated_outcomes(adjustedCriticalPred,
#                                    title="Predicción de críticos") +
#  ylab("Casos críticos")
#severePlotAdj <- plot_estimated_outcomes(adjustedSeverePred,
#                                    title="Predicción de severos") +
#  ylab("Casos severos")
#
#predictionPlotAdj <- ggpubr::ggarrange(plotlist=list(critPlotAdj, severePlotAdj),
#                                          ncol=1, nrow=2, common.legend=TRUE)
#
#ggsave("../graficas/7_prediccion_hospitalaria_ajustada.png", predictionPlotAdj,
#       width=12, height=7)
#

#######################################################
# 8 Plot results from instantaneous subreporting with age stratified dynamics
#######################################################

fecha <- "20210404"
deathSRFile <- paste("../datos_procesados/8_estimate_subreporting_death_stratified_",
                    fecha, "cutLast_7.csv", sep="")
critSRFile <- paste("../datos_procesados/8_estimate_subreporting_crit_stratified_",
                    fecha, "cutLast_7.csv", sep="")
severeSRFile <- paste("../datos_procesados/8_estimate_subreporting_severe_stratified_",
                    fecha, "cutLast_7.csv", sep="")

subreportDeathStrat <- read.csv(deathSRFile, stringsAsFactors=FALSE)
subreportDeathStrat$date <- lubridate::as_date(subreportDeathStrat$date)

subreportCritStrat <- read.csv(critSRFile, stringsAsFactors=FALSE)
subreportCritStrat$date <- lubridate::as_date(subreportCritStrat$date)

subreportSevereStrat <- read.csv(severeSRFile, stringsAsFactors=FALSE)
subreportSevereStrat$date <- lubridate::as_date(subreportSevereStrat$date)

medianCritDelay <- median(critDelayDf[[1]])
medianSevereDelay <- median(severeDelayDf[[1]])
subreportCritStrat$date <- subreportCritStrat$date - medianCritDelay
subreportSevereStrat$date <- subreportSevereStrat$date - medianSevereDelay

subreportDeathStratPlot <- plot_subreporting(subreportDeathStrat, title="Deaths")
subreportCritStratPlot <- plot_subreporting(subreportCritStrat, title="Critical")
subreportSevereStratPlot <- plot_subreporting(subreportSevereStrat, title="Severe")
estimateSubreportStrat <- gridExtra::grid.arrange(subreportDeathStratPlot,
                                                  subreportCritStratPlot,
                                                  subreportSevereStratPlot,
                                                  ncol=3)

plotSaveFile <- paste("../graficas/8_estimado_subreporte_estratificado",
                      fecha, "_ITISE.png", sep="")
ggsave(plotSaveFile, estimateSubreportStrat, width=20, height=8, units="cm")


#######################################################
# 8 Plot estimated real dynamics
#######################################################
daysCut <- 7
caseDynamicsFile <- paste("../datos_procesados/1_outcomeDynamics_", 
                          fecha, "_cut.csv", sep="")
caseDynamics <- read.csv(caseDynamicsFile) %>%
  dplyr::rename(., date = day) %>%
  dplyr::mutate(., date = lubridate::as_date(date))

realDynCrit <- merge(caseDynamics, subreportCritStrat, by = "date") %>%
  dplyr::mutate(., caseEst = newCases/estimate, caseEstH = newCases/lower,
                caseEstL = newCases/upper) %>%
  as_tibble(.)

realDynSevere <- merge(caseDynamics, subreportSevereStrat, by = "date") %>%
  dplyr::mutate(., caseEst = newCases/estimate, caseEstH = newCases/lower,
                caseEstL = newCases/upper) %>%
  as_tibble(.)

# remove last 7 days, to prevent distortions from symptoms backlog
realDynCrit <- realDynCrit[c(1:(nrow(realDynCrit)-daysCut)),]
realDynSevere <- realDynSevere[c(1:(nrow(realDynSevere)-daysCut)),]

realDynCritPlot_strat <- plot_estimated_dyn(realDynCrit, meanWindow=3,
                                      title = "Estimacion con criticos")
absSubreportCritPlot_strat <- plot_abs_subreport(realDynCrit, meanWindow=3,
                                      title = "Estimacion con criticos")
realDynSeverePlot_strat <- plot_estimated_dyn(realDynSevere, meanWindow=3,
                                      title = "Estimacion con severos")
absSubreportSeverePlot_strat <- plot_abs_subreport(realDynSevere, meanWindow=3,
                                      title = "Estimacion con severos")

estimatedCasesStrat <- gridExtra::grid.arrange(realDynCritPlot_strat,
                                          realDynSeverePlot_strat,
                                          ncol = 1)
absoluteSubreportStrat <- gridExtra::grid.arrange(absSubreportCritPlot_strat,
                                             absSubreportSeverePlot_strat,
                                             ncol = 1)

ggsave("../graficas/8_estimado_casos_reales_estratificado.png",
       estimatedCasesStrat, width = 16, height = 12, units = "cm")
ggsave("../graficas/8_estimado_casos_no_detectados_estratificado.png",
       absoluteSubreportStrat, width = 16, height = 12, units = "cm")


#######################################################
# 9 Demographics of cases
#######################################################

forecastDate <- "2021-04-15"
daysUsed <- 150
daysFirstCum <- 8
cutLastDays <- 2
fecha <- "20210404"
fitModel <- "exponential"
lastDay <- "2021-04-04"

individualCasesFile <- paste("../datos_procesados/1_resumen_casos_individuales_",
                             fecha, ".csv", sep="")

individualCasesDf <- read.csv(individualCasesFile, stringsAsFactors=FALSE) %>%
  dplyr::mutate(., FechaResultado=lubridate::as_date(FechaResultado),
                FechaInicioSintomas=lubridate::as_date(FechaInicioSintomas),
                FechaRec=lubridate::as_date(FechaRec),
                dateCritical=lubridate::as_date(dateCritical),
                dateHosp=lubridate::as_date(dateHosp)) %>%
  dplyr::filter(., FechaInicioSintomas>="2020-12-01")

deathPercentageLit <-
  read.csv("../datos_procesados/severity_stratified_calculation/1_fitted_ifr.csv",
           stringsAsFactors=FALSE) %>%
  dplyr::filter(., study=="Fitted") %>%
  dplyr::mutate(., outcome=IFR/100, outcomeL=IFR_L/100, outcomeH=IFR_H/100)

criticalPercentageLit <-
  read.csv("../datos_procesados/severity_stratified_calculation/1_fitted_critical.csv",
           stringsAsFactors=FALSE) %>%
  dplyr::filter(., study=="Fitted") %>%
  dplyr::mutate(., outcome=critical/100, outcomeL=criticalL/100, outcomeH=criticalH/100)

severePercentageLit <-
  read.csv("../datos_procesados/severity_stratified_calculation/1_fitted_severe.csv",
           stringsAsFactors=FALSE) %>%
  dplyr::filter(., study=="Fitted") %>%
  dplyr::mutate(., outcome=severe/100, outcomeL=severeL/100, outcomeH=severeH/100)


deathOutcomeDf <- expected_proportion_in_time(individualCasesDf,
                                             deathPercentageLit,
                                             delay_fun_death) %>%
  dplyr::mutate(., deathProp=outcomeInTodaysCases/cases) %>%
  dplyr::filter(., day<=lastDay)


critOutcomeDf <- expected_proportion_in_time(individualCasesDf,
                                             criticalPercentageLit,
                                             delay_fun_crit) %>%
  dplyr::mutate(., critProp=outcomeInTodaysCases/cases) %>%
  dplyr::filter(., day<=lastDay)

severeOutcomeDf <- expected_proportion_in_time(individualCasesDf,
                                             severePercentageLit,
                                             delay_fun_hosp) %>%
  dplyr::mutate(., critProp=outcomeInTodaysCases/cases) %>%
  dplyr::filter(., day<=lastDay)

deathOutcomeDf$meanProp <- zoo::rollmean(deathOutcomeDf$deathProp, k=5, fill=NA)
deathOutcomeDf$meanVal <- zoo::rollmean(deathOutcomeDf$expectedOutcomes, k=5, fill=NA)
critOutcomeDf$meanProp <- zoo::rollmean(critOutcomeDf$critProp, k=5, fill=NA)
critOutcomeDf$meanVal <- zoo::rollmean(critOutcomeDf$expectedOutcomes, k=5, fill=NA)
severeOutcomeDf$meanProp <- zoo::rollmean(severeOutcomeDf$critProp, k=5, fill=NA)
severeOutcomeDf$meanVal <- zoo::rollmean(severeOutcomeDf$expectedOutcomes, k=5, fill=NA)


plotDeathProp <- dplyr::filter(deathOutcomeDf, day >= "2021-01-01") %>%
  ggplot(., aes(x=day, y=meanProp*100)) +
  geom_line() +
  ylab("% muertes") +
  #ylim(c(0.8, 2)) +
  theme_bw()

plotCritProp <- dplyr::filter(critOutcomeDf, day >= "2021-01-01") %>%
  ggplot(., aes(x=day, y=meanProp*100)) +
  geom_line() +
  ylab("% criticos") +
  #ylim(c(0.8, 2)) +
  theme_bw()

plotSevereProp <- dplyr::filter(severeOutcomeDf, day >= "2021-01-01") %>%
  ggplot(., aes(x=day, y=meanProp*100)) +
  geom_line() +
  ylab("% graves") +
  #ylim(c(0.8, 2)) +
  theme_bw()


expectedPropsPlot <- ggpubr::ggarrange(plotlist=list(plotDeathProp,
                                                     plotCritProp, plotSevereProp),
                                       nrow=1, labels=c("% Muertes", "% críticos", "% hospital"))
ggsave("../expectedPropsPlot.png", expectedPropsPlot, width=10, height=3)


plotCritN <- dplyr::filter(critOutcomeDf, day >= "2020-12-01") %>%
  ggplot(., aes(x=day, y=meanVal/100)) +
  geom_line() +
  ylab("N criticos") +
  #ylim(c(0.8, 2)) +
  theme_bw()

plotSevereN <- dplyr::filter(severeOutcomeDf, day >= "2020-12-01") %>%
  ggplot(., aes(x=day, y=meanVal/100)) +
  geom_line() +
  ylab("N graves") +
  #ylim(c(0.8, 2)) +
  theme_bw()



subreportCrit <- readRDS("../datos_procesados/4_subreportingEstimate_critical_20210126.RDS")
subreportSevere <- readRDS("../datos_procesados/4_subreportingEstimate_severe_20210126.RDS")


#######################################################
# 10 Observed outcome rates
#######################################################
ou                    stringsAsFactors=FALSE)) %>%
  dplyr::filter(., day >= "2020-12-01")

deathProp <- proportion_outcome_delay(outcomeDf$newCases, outcomeDf$newDeaths,
                         delay_fun_death)*100
critProp <- proportion_outcome_delay(outcomeDf$newCases, outcomeDf$newCritical,
                         delay_fun_crit)*100
hospProp <- proportion_outcome_delay(outcomeDf$newCases, outcomeDf$newHosp,
                         delay_fun_hosp)*100
deathProp$day <- lubridate::as_date(outcomeDf$day)
critProp$day <- lubridate::as_date(outcomeDf$day)
hospProp$day <- lubridate::as_date(outcomeDf$day)

deathProp <- deathProp[-c(1:15),]
critProp <- critProp[-c(1:15),]
hospProp <- hospProp[-c(1:15),]

deathProp$meanProp <- zoo::rollmean(deathProp$proportionOutcome, k=5, fill=NA)
critProp$meanProp <- zoo::rollmean(critProp$proportionOutcome, k=5, fill=NA)
hospProp$meanProp <- zoo::rollmean(hospProp$proportionOutcome, k=5, fill=NA)

plotDeathPropObs <- dplyr::filter(deathProp, day >= "2021-01-01" & !is.na(meanProp)) %>%
  ggplot(., aes(x=day, y=meanProp)) +
  geom_line() +
  ylab("% muertes") +
  #ylim(c(0.8, 2)) +
  theme_bw()

plotCritPropObs <- dplyr::filter(critProp, day >= "2021-01-01" & !is.na(meanProp)) %>%
  ggplot(., aes(x=day, y=meanProp)) +
  geom_line() +
  ylab("% criticos") +
  #ylim(c(0.8, 2)) +
  theme_bw()

plotSeverePropObs <- dplyr::filter(hospProp, day >= "2021-01-01" & !is.na(meanProp)) %>%
  ggplot(., aes(x=day, y=meanProp)) +
  geom_line() +
  ylab("% graves") +
  #ylim(c(0.8, 2)) +
  theme_bw()


observedPropsPlot <- ggpubr::ggarrange(plotlist=list(plotDeathPropObs,
                                                     plotCritPropObs, plotSeverePropObs),
                                       nrow=1, labels=c("% Muertes", "% críticos", "% hospital"))
ggsave("../observedPropsPlot.png", observedPropsPlot, width=10, height=3)


