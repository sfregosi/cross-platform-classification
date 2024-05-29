# workflow to combine multiple AcousticStudies into a single AS 
# to create the training data for the glider BANTER model

# ------ Set up  ----------------------------------------------------------

# Load packages
library(PAMpal)
library(here)

# Set path to filtered AcousticStudies
path_acSt <- '//piccrpnas/crp4/glider_mhi_analysis/classification/dets_filtered'
path_big_data <- 'T:/fregosi/cross_platform_banter_big_data'


# ------ Load available AccousticStudies ----------------------------------
# Set trips to combine
# starting with just phase 3 trips
# trips <- sprintf("LL%03d", seq(60,70))

# load each trips available AcSts
acStFiles <- list.files(path = file.path(path_acSt), pattern = '\\.rda$')

# set up output list/df
acStList <- list()
evTable <- data.frame()
keepColNames <- c('start', 'end', 'id', 'sp', 'species', 'x', 'notes')

# loop through all the .rda files, load, and add to the list
for (a in seq_along(acStFiles)){
  load(file.path(path_acSt, acStFiles[a]))
  
  # get some trip/event data
  idStr <- PAMpal::id(detsFilt) # get trip_rec info for list name
  evsFilt <- names(PAMpal::events(detsFilt))
  # pull ancillary grouping data
  anc <- PAMpal::ancillary(detsFilt)
  # check column names
  cn <- colnames(anc$grouping)
  keepCols <- cn %in% keepColNames
  evData <- anc$grouping[,keepCols]
  # standardize col name - sometimes the col is 'species', sometimes 'sp' 
  spCol <- grepl('sp', colnames(evData))
  colnames(evData)[spCol] <- 'species'
  # change end column to stop 
  endCol <- grepl('end', colnames(evData)) 
  colnames(evData)[endCol] <- 'stop'
  # look for notes column or add a blank one 
  if (length(colnames(evData)) == 5){ # notes col present
    colnames(evData)[5] <- 'notes'
  } else if (length(colnames(evData)) < 5){
    evData$notes <- rep('NA', nrow(evData))
  }
  # add mission col
  evData$mission <- rep(idStr, nrow(evData))
  # add mission to event IDs
  fullIds <- paste0(idStr, '.', evData$id)
  # names(PAMpal::events(detsFilt)) <- fullIds
  evData$id <- fullIds
  detsFilt@ancillary$grouping$id <- fullIds
  for (e in seq_along(names(PAMpal::events(detsFilt)))){
    detsFilt[[e]]@id <- fullIds[e]
  }
  # reorder to standardize
  evData <- evData[,c('mission', 'id', 'species', 'start', 'stop', 'notes')]
  # add to the able of all events
  evTable <- rbind(evTable, evData)
  
  # re-assign the species (for instances with 'sp' as NA)
  # get species names ONLY for events that weren't filtered out
  spToSet <- evData$species[evData$id %in% fullIds]
  detsFilt <- PAMpal::setSpecies(detsFilt, method = 'manual', value = spToSet)
  
  # add the AcSt to the list
  acStList[[idStr]] <- detsFilt
}

# ------ Save intermediate step -------------------------------------------

save(acStList, file = file.path(path_big_data, paste0('acousticStudyList_glider_',
                                                      Sys.Date(), '.rda')))

save(evTable, file = file.path(path_big_data, paste0('eventTable_glider_',
                                                     Sys.Date(), '.rda')))
write.csv(evTable, file = here('data', paste0('eventTable_glider_', 
                                              Sys.Date(), '.csv')))

# 2 acoustic studies
# 202 total events

# ------ Combine AcousticStudies ------------------------------------------

# combine them! 
detsFiltAll <- PAMpal::bindStudies(acStList)

# save it - to the dir above dets_filtered
save(detsFiltAll, file = file.path(path_big_data, paste0('detsFiltAll_glider_', 
                                                         Sys.Date(), '.rda')))


# ------ MANUAL species ID updates OUTSIDE R ------------------------------

# manual assessments are in Excel in glider_mhi_analysis folder on server
# merged the two prediction_comparison_summary sheets for sg639 and added a col
# for additional analyst assessment then final 'species' adjudicating those ids
# saved that on glider_MHI_analysis/classification/checked_for_Pc as
# glider_events_training_2024-05-28.csv
# Pasted the final 'species' col from the above into the eventTable as a new
# column called 'new_species' and renamed/saved as 
# 'event_table_2024-05-28_manualEdits.csv'


# ------ Read in and clean up new species ID info -------------------------

# read in table with final manual IDs - 202 events
evTableNew <- read.csv(here('data', paste0('eventTable_glider_', '2024-05-28', 
                                           '_manualEdits.csv')))

# rename id col to event_id
colnames(evTableNew)[grep('id', colnames(evTableNew))] <- 'event_id'
# get rid of old species and rename species_new to species
evTableNew <- subset(evTableNew, select = -species)
colnames(evTableNew)[grep('new_species', colnames(evTableNew))] <- 'species'

# remove all rows/events with NA or MIN in the new_species
badRows <- grepl('NA', evTableNew$species) | is.na(evTableNew$species) | 
  grepl('MIN', evTableNew$species)
# sum(badRows) # removes 50 events
evTableClean <- evTableNew[!badRows,]
# down to 152 events

# remove these events from the combined AcousticStudy
# some were already removed via filtering so need to find idxs for those to save
evAll <- names(PAMpal::events(detsFiltAll))
match <- evAll %in% evTableClean$event_id
# sum(match) # 153 events will remain
# make the unmatched events NULL
PAMpal::events(detsFiltAll)[!match] <- NULL

# now update spaces in detsFiltAll with the evTableClean species
detsFiltClean <- PAMpal::setSpecies(detsFiltAll, method = 'manual', 
                                    evTableClean$species)
# check for unique sp ids
unique(evTableClean$species)
unique(PAMpal::species(detsFiltClean))

# get some counts for checking
cat('Total events:', length(PAMpal::events(detsFiltClean)), '\n')
cat('Total Pc events:', length(which(evTableClean$species == 'Pc')), '\n')

# save it - to the dir above dets_filtered
save(detsFiltClean, file = file.path(path_big_data, paste0('detsFiltClean_glider_', 
                                                           Sys.Date(), '.rda')))

# ------ Export for banter ------------------------------------------------


# export for banter with and without unknown 'X' events
# 153 events including X events
banterDetsAll <- PAMpal::export_banter(detsFiltClean, training = FALSE)
save(banterDetsAll, file = file.path(path_big_data, paste0('banterDetsAll_glider_', 
                                                           Sys.Date(), '.rda')))
# 141 events excluding X events
banterDetsTrain <- PAMpal::export_banter(detsFiltClean, training = TRUE, 
                                         dropSpecies = c('X'))
save(banterDetsTrain, file = file.path(path_big_data, 
                                       paste0('banterDetsTrain_glider_', 
                                              Sys.Date(), '.rda')))

