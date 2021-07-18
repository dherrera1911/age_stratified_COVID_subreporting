library(lubridate)
library(gridExtra)
library(ggpubr)
source("./plotting_functions.R")
source("./functions_analysis_subreporting.R")
source("./functions_auxiliary.R")


########################################################
## Plot unstratified subreporting estimates
########################################################
subreportDeath <- read.csv("../results/2_estimate_subreporting_deaths.csv",
                           stringsAsFactors=FALSE) %>%
  dplyr::mutate(., date=lubridate::as_date(date))
subreportCrit <- read.csv("../results/2_estimate_subreporting_critical.csv",
                           stringsAsFactors=FALSE) %>%
  dplyr::mutate(., date=lubridate::as_date(date))
subreportSevere <- read.csv("../results/2_estimate_subreporting_severe.csv",
                           stringsAsFactors=FALSE) %>%
  dplyr::mutate(., date=lubridate::as_date(date))

subreportDeathPlot <- plot_subreporting(subreportDeath, title="Deaths")
subreportCritPlot <- plot_subreporting(subreportCrit, title="Critical")
subreportSeverePlot <- plot_subreporting(subreportSevere, title="Severe")
estimateSubreport <- gridExtra::grid.arrange(subreportDeathPlot,
                                             subreportCritPlot,
                                             subreportSeverePlot, ncol=3)

ggsave("../data/subreport_estimate.png", estimateSubreport,
       width=20, height=8, units="cm")

#######################################################
# Plot stratified subreporting estimates
#######################################################

subreportDeathStrat <- read.csv("../results/estimate_subreporting_death_stratified.csv",
                                stringsAsFactors=FALSE) %>%
  dplyr::mutate(., date=lubridate::as_date(date))

subreportCritStrat <- read.csv("../results/3_estimate_subreporting_crit_stratified.csv",
                                stringsAsFactors=FALSE) %>%
  dplyr::mutate(., date=lubridate::as_date(date))

subreportSevereStrat <- read.csv("../results/3_estimate_subreporting_severe_stratified.csv",
                                stringsAsFactors=FALSE) %>%
  dplyr::mutate(., date=lubridate::as_date(date))


subreportDeathPlot_strat <- plot_subreporting(subreportDeathStrat, title="Deaths")
subreportCritPlot_strat <- plot_subreporting(subreportCritStrat, title="Critical")
subreportSeverePlot_strat <- plot_subreporting(subreportSevereStrat, title="Severe")
estimateSubreport_strat <- gridExtra::grid.arrange(subreportDeathPlot_strat,
                                             subreportCritPlot_strat,
                                             subreportSeverePlot_strat, ncol=3)

ggsave("../data/subreport_estimate_stratified.png", estimateSubreport_strat,
       width=20, height=8, units="cm")


