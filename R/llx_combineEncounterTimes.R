# workflow to combine all LLHARP manual pick encounterTimes.csvs into single table

# Load packages
library(here)

# Set paths
path_picks <- '//piccrp4nas/grampus/llharp/analysis/manualpicks'

# Set trips to merge
# starting with just phase 3 trips
trips <- sprintf("LL%03d", seq(60,70))

# set up output data.frame
allPicks <- data.frame()

# loop through each trip
for (t in 1:length(trips)){
  # t <- 1 # for testing
  # trip num
  tripStr <- trips[t]
  # find .csvs
  encFileList <- list.files(path = file.path(path_picks, tripStr), pattern = '\\.csv$')
  for (e in 1:length(encFileList)){
    encFile <- encFileList[e]
    tmp <- read.csv(file.path(path_picks, tripStr, encFile))
    allPicks <- rbind(allPicks, tmp)
  }
  
} # end trips loop

# remove any duplicates
# (there shouldn't be any but in case something got read in twice)
allPicks <- unique(allPicks)
# clean up NAs in the notes so they don't print when saving to csv
allPicks$notes[is.na(allPicks$notes)] <- ''


# save to csv
write.csv(allPicks, here('data', paste0('allPicks_phase3_', Sys.Date(), '.csv')))



