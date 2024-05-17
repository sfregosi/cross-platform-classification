## Workflow for generating PAMpal AcousticStudy objects for glider data
# 
# Requires manually marked cetacean events (from Triton and simplified into an 
# a csv)
#
# Creates several output .rda files:
#   'pamVer_***_params.rda': parameters (pps) input for PAMpal 
#   'pamVer_***_dets.rda': 'dets' AcousticStudy with all all detection feature 
#                           measurements by event
#
# Entire Process Overview
# 1a. Manually identify start/end times of possible odontocete events in Triton
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
library(tidyverse)

# ------ USER DEFINED INPUTS ----------------------------------------------

mission <- 'sg639_MHI_Apr2023'
pgVer <- '20207b'

# define transfer function
calFile <- 0 # 'C:/path/cal.csv'; to skip calibration set to 0

# define paths
path_analysis <- 'T:/glider_MHI_analysis'
# path_pg <- 'T:/glider_MHI_analysis/pamguard'
# path_out <- 'T:/glider_MHI_analysis/classification'

path_llamp <- 'Z:/LLHARP/processingCode/llamp' # bc pull some functions from llamp

# ### configurable PAMpal settings - typically DO NOT change ###
sr_hz          <- 'auto'
filterfrom_khz <- 2
filterto_khz   <- NULL
winLen_sec     <- 0.0025


# ------ >>generated some variables from user inputs ----------------------
# these typically will not change but file names may need slight modifications
pgVerPrfx <- paste0('pam', pgVer)
fnStr <- paste0('pam', pgVer, '_', mission)

# some paths
path_pg <- file.path(path_analysis, 'pamguard')

# some file paths
binDir <- file.path(path_pg, 'binaries', paste0(pgVerPrfx, '_glider_banter_', 
                                                mission))
dbFile <- file.path(path_pg, 'databases', paste0(pgVerPrfx, '_glider_banter_', 
                                                   mission, '.sqlite3'))
# encounter times file - phase 3 naming scheme
logFile <- file.path(path_analysis, 'triton', 'merged_logs', 
                          paste0(mission, '_log_merged.csv'))

# files to be created
paramFile <- file.path(path_analysis, 'classification', 'params', 
                       paste0(mission, '_params.rda'))
detsFile <- file.path(path_analysis, 'classification', 'dets',
                      paste0(mission, '_dets.rda'))
detsFiltFile <- file.path(path_analysis, 'classification', 'dets_filtered',
                          paste0(mission, '_detsFilt.rda'))

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
                              grouping = logFile, 
                              format = '%m/%d/%Y %H:%M:%S')
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



