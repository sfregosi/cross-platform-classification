plotContours <- function(binaryData){
  #' plotContours
  #' 
  #' @description Plot multiple whistle contours from Pamguard binaries on a 
  #' single plot
  #' 
  #' author: Selene Fregosi selene.fregosi [at] noaa.gov
  #' laste updated: 24 April 2024
  #'
  #' @param binaryData the 'data' list item from a loadPamguardBinaryFile, or a
  #' compiled list of whistle data from multiple binaries loaded with 
  #' loadMultiBinaries
  #' 
  #' @return a plot (ggplot) with all the whistles plotted on a single axis
  #'
  #' @examples
  #' p <- plotContours(whOut)
  #' 
  #' ######################################################################
  # plot all the whistles
  xMax <- 0
  yMax <- 0
  p <- ggplot()
  
  for (f in 1:length(names(binaryData))){
    df <- data.frame(time = binaryData[[f]]$time - binaryData[[f]]$time[1],
                    freq = binaryData[[f]]$freq/1000)
    xMaxTmp <- max(df$time)
    yMaxTmp <- max(df$freq)
    if (xMaxTmp > xMax){xMax <- xMaxTmp}
    if (yMaxTmp > yMax){yMax <- yMaxTmp}
    
    p <- p + 
      geom_line(data = df, aes(x = time, y = freq), alpha = 0.3)
  }
  
  p <- p + xlim(0,xMax) + 
    ylim(0,yMax) +
    labs(x = 'Time [s]', y = 'Frequency [kHz]') + 
    theme_bw() 
  
return(p)
}