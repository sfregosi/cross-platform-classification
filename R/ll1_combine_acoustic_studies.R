# workflow to combine multiple AcousticStudies into a single AS 
# to create the training data for the newer LLHARP BANTER model



# ------ Set up  ----------------------------------------------------------

# Load packages
library(PAMpal)
library(here)

# Set path to filtered AcousticStudies
path_acSt <- '//piccrp4nas/grampus/llharp/analysis/eventDetections/dets_filtered'
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
 # add trip col
 evData$trip <- rep(idStr, nrow(evData))
 # reorder to standardize
 evData <- evData[,c('trip', 'id', 'species', 'start', 'stop', 'notes')]
 # add to the able of all events
 evTable <- rbind(evTable, evData)
 
 # re-assign the species (for instances with 'sp' as NA0)
 detsFilt <- PAMpal::setSpecies(detsFilt, method = 'manual', value = evData$species)
 # add the AcSt to the list
 acStList[[idStr]] <- detsFilt
}

# ------ Combine AcousticStudies ------------------------------------------

# combine them! 
detsFiltAll <- PAMpal::bindStudies(acStList)

# save it - to the dir above dets_filtered
save(detsFiltAll, file = file.path(
  dirname(path_acSt), paste0('detsFiltAll_LLHARP_', Sys.Date(), '.rda')))

# export for banter and save
banterDetsAll <- export_banter(detsFiltAll, training = TRUE)
save(banterDets, file = banterDetsFile)



# ------ Save intermediate step -------------------------------------------

save(acStList, file = file.path(path_big_data, paste0('acousticStudyList_',
                                                      Sys.Date(), '.rda')))

save(evTable, file = file.path(path_big_data, paste0('eventTable_',
                                                          Sys.Date(), '.rda')))
write.csv(evTable, file = here('data', paste0('eventTable_', Sys.Date(), '.csv')))

# 71 acoustic studies
# 634 events

# ------ MANUAL species ID updates OUTSIDE R ------------------------------

# manual assessments are on Google Sheets. Created new column in eventTable csv 
# 'new_species' and pasted in updated manual IDs. Saved as 
# 'event_table_2024-05-26_manualEdits.csv'


# ------ Read in and clean up new species ID info -------------------------

evTableNew <- read.csv(here('data', paste0('eventTable_', '2024-05-26', 
                                           '_manualEdits.csv')))

# rename trip col (has recorder string) and make trip column
colnames(evTableNew)[grep('trip', colnames(evTableNew))] <- 'trip_rec'
evTableNew$trip <-  substr(evTableNew[['trip_rec']], 1, 5)

# rename id col to event_id
colnames(evTableNew)[grep('id', colnames(evTableNew))] <- 'event_id'

# get rid of old species and rename species_new to species
evTableNew <- subset(evTableNew, select = -species)
colnames(evTableNew)[grep('new_species', colnames(evTableNew))] <- 'species'

# remove all rows with NA or MIN in the new_species
badRows <- grepl('NA', evTableNew$species) | is.na(evTableNew$species) | 
  grepl('MIN', evTableNew$species)
# sum(badRows) # how many to remove?
evTableClean <- evTableNew[!badRows,]

# check for unique sp ids
unique(evTableClean$species)
cat('Total events:', nrow(evTableClean), '\n')
cat('Total Pc events:', length(which(evTableClean$species == 'Pc')), '\n')
cat('Unique recorders:', length(unique(evTableClean$trip_rec)), '\n')
cat('Unique trips:', length(unique(evTableClean$trip)), '\n')

# only 1 entire AcSt got dropped (Trip 38) 


# rename all non-Pc to UO (Pm, Kspp, Gg, BW)
PmEvents<-which(TrainingDataset$events$species=='Pm')
TrainingDataset$events$species[PmEvents]<-'UO'



