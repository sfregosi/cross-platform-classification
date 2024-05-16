## Workflow for generating PAMpal AcousticStudy objects for glider data
# 
# Requires manually marked cetacean events (from Triton and simplified into an 
# 'LL000_encounterTimes.csv')
#
# Creates several output .rda files:
#   'pamVer_LL###_FR##_DL##_params.rda': parameters (pps) input for PAMpal 
#   'pamVer_LL###_FR##_DL##_dets.rda': 'dets' AcousticStudy with all all 
#                                     detection feature measurements by event
#
# Entire Process Overview
# 1a. Manually identify start/end times of possible odontocete events 
# 1b. Run Pamguard click, WM, cepstrum detector
# 2. [THIS SCRIPT] Generate AcousticStudy - define params, calc features, apply
#    filter, output 'dets' and 'detsFilt'
# 3. Predict species with BANTER and save to events table


# ------ install/update necessary packages --------------------------------

# make sure you have Rtools installed
if(!require('devtools')) install.packages('devtools')

# install latest ver from github (won't do anything if up to date!)
# devtools::install_github('ericarcher/banter')
devtools::install_github('taikisan21/PAMpal')

# # Installation of CRAN versions - only have to run once
# install.packages('banter')
# install.packages('PAMpal')

library(here)
library(PAMpal)

# ------ USER DEFINED INPUTS ----------------------------------------------

# ------- >>TRIP/RECORDER INFO --------------------------------------------
# define trip and recorder frame
tripStr <- 'LL070'
# recStr <- 'FR04_DL88'
# recStr <- 'FR05_DL96'
# recStr <- 'FR10_DL73'
# recStr <- 'FR10_DL63'
# recStr <- 'FR38_DL65'
# recStr <- 'FR45_DL60'
# recStr <- 'FR45_DL84'
# recStr <- 'FR51_DL64'
# recStr <- 'FR52_DL59'
# recStr <- 'FR63_DL73'
# recStr <- 'FR63_DL84'
recStr <- 'FR64_DL88'
# Pamguard version
pgVer <- '20207b'


# ------- >>rarely modified inputs ----------------------------------------
# ### define transfer function ###
# calFile <- 0         # for no calibration
# OLD NOTE: for LL050 onward...use this one - this is flat at 74 dB
# calFile <- '//piccrp4nas/grampus/llharp/calibration/transfer_functions/RevN_PAMpal_invSensit.csv'
# more recent from S. Wiggins for Phase 3
# calFile <- '//piccrp4nas/grampus/llharp/calibration/RevN_type4_110516-07_manual.csv'
# older deployments will vary by datalogger
# calFile <- '//picqueenfish/psd2/crp/LLHARP/calibration/transfer_functions/longline_old/DL65/699_PAMr_invSensit.csv'

# ### define paths ###
# path_analysis should point to the folder above the 'pamguard' folder (that 
#               contains the 'databases' and 'binaries' subdirectories), must
#               contain a 'manualPicks' folder of event .csvs, a 'banterModels' 
#               folder with models for prediction, and an 'eventData folder
#               for outputs
path_analysis <- '//piccrp4nas/grampus/llharp/analysis/'
# path_llamp <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
path_llamp <- here()
# path_crptools <- file.path(dirname(path_llamp), 'crptools') # assumes same as llamp

# ### configurable PAMpal settings - typically DO NOT change ###
sr_hz          <- 'auto'
filterfrom_khz <- 2
filterto_khz   <- NULL
winLen_sec     <- 0.0025


# ------ >>generated some variables from user inputs ----------------------
# these typically will not change but file names may need slight modifications
pgVerPrfx <- paste0('pam', pgVer)
trStr <- paste0(tripStr, '_', recStr)
fnStr <- paste0('pam', pgVer, '_', tripStr, '_', recStr)

# some paths
path_pg <- paste0(path_analysis, 'pamguard')

# some file paths
binDir <- file.path(path_pg, 'binaries', tripStr, 
                    paste0(pgVerPrfx, '_llharp_banter_', trStr))
dbFile <- file.path(path_pg, 'databases',  tripStr, 
                    paste0(pgVerPrfx, '_llharp_banter_', trStr, '.sqlite3'))

# encounter times file - phase 3 naming scheme
encTimesFile <- file.path(path_analysis, 'manualPicks', tripStr, 
                          paste0(trStr, '_encounterTimes.csv'))

# files to be created
paramFile <- file.path(path_analysis, 'eventDetections', 'parameters', 
                       paste0(fnStr, '_params.rda'))
detsFile <- file.path(path_analysis, 'eventDetections', 'dets',
                      paste0(fnStr, '_dets.rda'))
detsFiltFile <- file.path(path_analysis, 'eventDetections', 'dets_filtered',
                          paste0(fnStr, '_detsFilt.rda'))

# ------ PAMpal steps -----------------------------------------------------


# ------ >>define parameters ----------------------------------------------
# (database, binaries, default settings)

if (!file.exists(paramFile)){
  fkwPps <- PAMpalSettings(db = dbFile, binaries = binDir, 
                           sr_hz = sr_hz, winLen_sec = winLen_sec,
                           filterfrom_khz = filterfrom_khz, 
                           filterto_khz = filterto_khz)
  # add calibration file
  # fkwPps <- addCalibration(fkwPps, calFile = calFile, units = 2, all = TRUE)
  # enter '16' when prompted for bit rate of data
  save(fkwPps, file = paramFile)
} else if (file.exists(paramFile)){
  load(paramFile)
  cat('Loaded existing paramFile:', paramFile, '\n')
}


# ------ >>process all events ---------------------------------------------
# pamguard detections into an acoustics study object
# (slow so only run if doesn't exist already)

if (!file.exists(detsFile)){
  dets <- processPgDetections(fkwPps, mode = 'time', id = paste0(trStr),
                              grouping = encTimesFile, 
                              format = '%m/%d/%Y %H:%M')
  # format = '%m/%d/%Y %H:%M:%S')
  # the time format may need to be modified depending on how the csv was made
  save(dets, file = detsFile)
  cat(length(names(events(dets))), 'events processed and saved\n')
}else if(file.exists(detsFile)){
  load(detsFile)
  cat('Loaded existing detsFile:', detsFile, '\n')
}



# ------ >>filter out bad clicks ------------------------------------------
# apply 'standard' banter filter/clean up steps

# filters out:
# clicks with bandwidth at 10dB < 5 kHz, peak below 5 or above 80 kHz, duration 
# less than 2 ms or greater than 1000 ms AND cleans up within each detector by 
# peak (Click_Detector_1 peak > 2 & < 15, Click_Detector_2 peak > 15 & < 30, 
# Click_Detector_3 peak > 30 & < 50, Click_Detector_4 peak > 30 & < 50, 
# Click_Detector_5 peak > 50 & < 80

source(here('R', 'functions', 'filterClicks.R'))
detsFilt <- filterClicks(dets)

# # additional filtering specific for LLHARP banter work
# detectorList <- unique(getClickData(detsFilt)$detectorName)
# for (cd in 1:length(detectorList)){  
#   # set detector to filter 
#   dtf <- detectorList[cd]
#   # remove clicks with peak to peak level < 110
#   detsFilt <- filter(detsFilt, (detectorName != dtf) | (dBPP >= 110))
# }

# save filtered dets
save(detsFilt, file = detsFiltFile)

# number of events may change after filtering (all clicks may be filtered out)
# eventListF = events(detsFilt)
# detEventsF = names(events(detsFilt))
# nEventsF = length(detEventsF)
cat(length(names(events(detsFilt))), 'events after filtering\n')



