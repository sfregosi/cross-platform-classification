#' Scratchpad script for working towards overhaul of LLAMP banter model
#'
#' Goal is to eventually turn this (or parts of this) into a more detailed .qmd
#' document (see banterOverhaul.qmd) but will use this to sketch out sections 
#' until things are more solidified
#'
#'


# --- SETUP ---------------------------------------------------------------

library(here)
library(lubridate)



# LOAD DATA ---------------------------------------------------------------

# load`Events` dataframe with start/end times for each event in phases 1 and 2
# but contains no species info. Times are in HST. 
load("~/GitHub/LLHARP/data/EventInfo_2023-03-23.rda")

# load `TrainingDataset` list formatted for training in banter with all phase 1
# and phase 2 events (Pc, Pm, and UO)
load("~/GitHub/LLHARP/data/TrainingDataset_Pc_UO_Pm2023-05-30_edited.rda")
View(TrainingDataset$events)

# create new dataframe of just true Pc events
pcIdx = which(TrainingDataset$events$species == 'Pc')
truePc = data.frame(trip = NA, event = TrainingDataset$events$event.id[pcIdx], 
                    species_og= TrainingDataset$events$species[pcIdx], 
                    start_HST = as.POSIXct(NA, tz = 'HST'), 
                    end_HST = as.POSIXct(NA, tz = 'HST'))

for (e in 1:nrow(truePc)){
  eventIdx = which(Events$EventID == truePc$event[e])
  truePc$trip[e] = Events$TripID[eventIdx]
  truePc$start_HST[e] = as.POSIXct(Events$EventStart[eventIdx])
  truePc$end_HST[e] = as.POSIXct(Events$EventStop[eventIdx])
}

truePc$start_UTC = with_tz(truePc$start_HST, tzone = 'GMT')
truePc$end_UTC = with_tz(truePc$end_HST, tzone = 'GMT')

# write to csv so can work with it more interactively in Excel
write.csv(truePc, file = here('banter', 'phase1_2_trainingDatasetEvents.csv'))
# opened this csv and pasted into 'truePcEvents.xlsx' 
# standardized event ID labels by hand so all start with 'LL' and added columns
# for recorder time zone, clicks present, whistles present, new species ID, and
# what the pamguard filter settings were (pps)
# 

# NEXT...ideally each of these 20 events will be reassessed independently 
# by SF and JT. Must be re-exported with PAMpal to fix filtering issue, ideally 
# generate click reports and perhaps some whistle reporting? 


# inde