## Workflow for predicting species (Pc vs UO) for glider events
# 
# Requires 'detsFilt' AcousticStudy and an existing BANTER model
#
# Creates several output .rda files:
#   'pamVer_***_banterDets.rda': banter-formatted dets data 
#   'pamVer_***_predictedEvents.csv': event table with appended columns with 
#                                     species prediction scores
#
# Entire Process Overview
# 1a. Manually identify start/end times of possible odontocete events 
# 1b. Run Pamguard click, WM, cepstrum detector
# 2. Generate AcousticStudy - define params, calc features, apply
#    filter, output 'dets' and 'detsFilt'
# 3. [THIS SCRIPT] Predict species with BANTER and save to events table


# ------ install/update necessary packages --------------------------------

# make sure you have Rtools installed
if(!require('devtools')) install.packages('devtools')

# install latest ver from github (won't do anything if up to date!)
devtools::install_github('ericarcher/banter')
devtools::install_github('taikisan21/PAMpal')

# # Installation of CRAN versions - only have to run once
# install.packages('banter')
# install.packages('PAMpal')

library(here)
library(PAMpal)
library(banter)
library(rfPermute)
# library(tidyverse)

# ------ USER DEFINED INPUTS ----------------------------------------------

# ------- >>TRIP/RECORDER INFO --------------------------------------------
# define glider mission
mission <- 'sg639_MHI_Apr2022'

# Pamguard version
pgVer <- '20207b'

# choose BANTER model to use for predictions (uncomment desired model)
# HICEAS 2017 towed array
modelType <- 'HICEAS'
modelName <- 'bantMDL_HICEAS2017-CH5_Filtered_577.rdata'
# LLHARP
# modelType <- 'LLHARP
# modelName <-  'LLBanterModel_2023-01-26.rda' 'LLHARP_BANTER_2023-03-23.rda'

# define paths
path_analysis <- 'T:/glider_MHI_analysis'

# ------ >>generated some variables from user inputs ----------------------
# these typically will not change but file names may need slight modifications
pgVerPrfx <- paste0('pam', pgVer)
fnStr <- paste0('pam', pgVer, '_', mission)

# some paths
path_pg <- paste0(path_analysis, 'pamguard')

# Some file paths
# detsFile = file.path(path_analysis, 'eventDetections', 'dets',
#                     paste0(fnStr, '_dets.rda'))
detsFiltFile <- file.path(path_analysis, 'classification', 'dets_filtered',
                          paste0(fnStr, '_detsFilt.rda'))
banterDetsFile <- file.path(path_analysis, 'classification', 'for_banter', 
                            paste0(fnStr, '_banterDets.rda'))
if (modelType == 'HICEAS'){
  modelFile <- file.path(path_analysis, 'classification', 'models', 'HICEAS_BANTER', modelName)
} else if (modelType == 'LLHARP'){
  modelFile <- file.path(path_analysis, 'classification', 'models', modelName)
}
logFile <- file.path(path_analysis, 'triton', 'merged_logs', 
                     paste0(mission, '_log_merged.csv'))
# tsFile <- file.path(path_analysis, 'tripSummaries', paste0(tripStr, '_summary.csv'))
resFile <- file.path(path_analysis, 'classification', 'predictions', 
                     paste0(fnStr, '_predictedEvents_', modelType, '.csv'))

# ------ LOAD and export AcousticStudies objects --------------------------

# load the previously generated AcousticStudy
# there is a 'raw' version and a 'filtered' version - use filtered
load(detsFiltFile)

# export PAMpal detection features to BANTER compatible format
# use training = FALSE if just predicting to avoid any auto-removal of events 
# that don't meet the training minimum requirements
banterDets <- export_banter(detsFilt, training = FALSE)
# remove any lingering Click_Detector_0 detections (unclassified clicks)
banterDets$detectors$Click_Detector_0<-NULL
# View(banterDets$events)

# save 'em
save(banterDets, file = banterDetsFile)

# ------ BANTER steps -----------------------------------------------------
# predict species!

# load banter model
load(modelFile)

# predict species scores
score <- predict(bant.mdl, banterDets)
# View(score$predict.df)


# ------ SAVE results -----------------------------------------------------

# add event timing info to predicted output
et <- read.csv(logFile)

res <- full_join(et, score$predict.df, by = c('id' = 'event.id'))
# reorder - col names depend on model
if (modelType == 'HICEAS'){
  res <- res %>% relocate(id) %>% 
    relocate(c(predicted, original, X33, X577, correct), .after = 'end') %>%
    select(!sp)
} else if (modelType == 'LLHARP'){
  res <- res %>% relocate(id) %>% 
    relocate(c(predicted, original, Pc, UO, correct), .after = 'end') %>%
    select(!species)
}

# View(res)
# Pc <- score$predict.df[which(score$predict.df$predicted == 'X33'),]

# save it! 
write.csv(res, file = resFile)


