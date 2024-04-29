# workflow to combine all LLHARP manual pick encounterTimes.csvs into single table

# Load packages
library(here)

# Set paths
path_picks <- '//piccrp4nas/grampus/llharp/analysis/manualpicks'

# Set trips to merge
trips <- sprintf("LL%03d", seq(60,70))

# set up output data.frame
allPicks <- data.frame()

# loop through each trip
for (t in 1:length(trips)){
  # trip num
  tripStr <- trips[t]
  # find .csvs
  dir(file.path(path_picks, tripStr, '*.csv'))
  
}
# read in first one
tmp <- read.csv(file.path(path_picks, tripStr))