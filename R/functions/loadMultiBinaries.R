loadMultiBinaries <- function(wmBinFiles){
  #' loadMultiBinaries
  #' 
  #' @description Load in multiple Pamguard binary files and compile the data 
  #' lists into a single list. For now it works with WMD data...
  #' 
  #' author: Selene Fregosi selene.fregosi [at] noaa.gov
  #' last updated: 25 April 2024
  #'
  #' @param wmFiles character array of whistle moan binary files to be loaded
  #' 
  #' @return a list of binary WMD data
  #'
  #' @examples
  #' loadMultiBinaries(as)
  #' 
  #' ######################################################################
  
  binOut <- list() # create output that will compile across files
  for (f in 1:length(wmBinFiles)){
    whBin <- PamBinaries::loadPamguardBinaryFile(wmBinFiles[f], convertDate = TRUE)
    # get contour frequency and time info
    whBinCont <- PamBinaries::contourToFreq(whBin$data, verbose = FALSE)
    # loop through each whistle and add to whOut
    whNames <- names(whBinCont)
    for (h in 1:length(whNames)){
      binOut[whNames[h]] <- whBinCont[whNames[h]]
    }
  }
  
  return(binOut)
}

