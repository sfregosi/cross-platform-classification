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



# ------ Save intermediate step -------------------------------------------

save(acStList, file = file.path(path_big_data, paste0('acousticStudyList_',
                                                      Sys.Date(), '.rda')))

save(evTable, file = file.path(path_big_data, paste0('eventTable_',
                                                          Sys.Date(), '.rda')))
write.csv(evTable, file = here('data', paste0('eventTable_', Sys.Date(), '.csv')))


# ------ MANUAL species ID updates OUTSIDE R ------------------------------



# ------ Combine AcousticStudies ------------------------------------------


# combine them! 
detsFiltAll <- PAMpal::bindStudies(acStList)

# save it - to the dir above dets_filtered
save(detsFiltAll, file = file.path(
  dirname(path_acSt), paste0('detsFiltAll_LLHARP_', Sys.Date(), '.rda')))

# export for banter and save
banterDetsAll <- export_banter(detsFiltAll, training = TRUE)
save(banterDets, file = banterDetsFile)

