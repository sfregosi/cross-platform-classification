# workflow to combine multiple AcousticStudies into a single AS

# Last updated 2024 April 30, S. Fregosi

# Load packages
library(here)

# Set path to filtered AcousticStudies
path_acSt <- '//piccrp4nas/grampus/llharp/analysis/eventDetections/dets_filtered'

# Set trips to combine
# starting with just phase 3 trips
# trips <- sprintf("LL%03d", seq(60,70))

# load each trips available AcSts
acStFiles <- list.files(path = file.path(path_acSt), pattern = '\\.rda$')

# set up output list
acStList <- list()

# loop through all the .rda files, load, and add to the list
for(a in 1:length(acStFiles)){
  load(file.path(path_acSt, acStFiles[a]))
  # get trip_rec info for list name
  acStName <- substr(acStFiles[a], regexpr('_LL', acStFiles[a]) + 1, 
         regexpr('_dets', acStFiles[a]) - 1)
  # have to rename it
 acStList[[acStName]] <- detsFilt
}

# combine them! 
detsFiltAll <- PAMpal::bindStudies(acStList)

# save it - to the dir above dets_filtered
save(detsFiltAll, file = file.path(
  dirname(path_acSt), paste0('detsFiltAll_LLHARP_', Sys.Date(), '.rda')))

