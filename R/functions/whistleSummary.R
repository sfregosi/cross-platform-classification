#' whistleSummary
#' 
#' @description Summarize whistles for a single AcousticStudy event, to be used 
#' within the event_summary_report Rmd. Summarizes total number of whistles and 
#' calculates some summary statistics (begin and end frequency, mean frequency, 
#' duration, and frequency range) output into a nicely formatted table. 
#'
#' @param asF 
#' @param eventUID 
#'
#' @return 'wl' list with 3 elements: total number of whistles, PAMpal whistle 
#' data for each whistle, and 'mt', the formatted table of median summary 
#' statistics
#' 
#' @export
#'
#' @examples  
#' wl <- whistleSummary(detsFilt, eventUID)
#' 
#' 
whistleSummary <- function(asF, eventUID){
  
  # set up output list
  wl <- list()
  
  # pull just whistle data for this event
  wh <- getWhistleData(asF[[eventUID]])
  
  if (!is.null(wh)){
    # Order whistles by time
    wh <- wh[order(wh$UTC),]
    # Remove duplicate whistles (shouldn't be any like there is for clicks)
    wh <- wh[!duplicated(wh$UID),]
  }
  
  # add to output list
  wl$nWhistles <- length(wh$UTC)
  wl$wh <- wh
  
  # if valid whistles exist...
  if (wl$nWhistles > 0) {
    # calculate a bunch of medians nicely formatted
    freqBeg    <- formatC(median(wh$freqBeg, na.rm = TRUE)/1000, digits = 2, 
                          format = 'f')
    freqEnd    <- formatC(median(wh$freqEnd, na.rm = TRUE)/1000, digits = 2, 
                          format = 'f')
    freqMean   <- formatC(median(wh$freqMean, na.rm = TRUE)/1000, digits = 2, 
                          format = 'f')
    freqStdDev <- formatC(median(wh$freqStdDev, na.rm = TRUE)/1000, digits = 2, 
                          format = 'f')
    duration   <- formatC(median(wh$duration, na.rm = TRUE), digits = 3, 
                          format = 'fg')
    freqMin    <- formatC(median(wh$freqMin, na.rm = TRUE)/1000, digits = 2, 
                          format = 'f')
    freqMax    <- formatC(median(wh$freqMax, na.rm = TRUE)/1000, digits = 2, 
                          format = 'f')
    freqRange  <- formatC(median(wh$freqRange, na.rm = TRUE)/1000, digits = 2,
                          format = 'f')
    
    # compile into a summary table with nice formatting
    clnms <- c('parameter', 'value') # column names
    mt <- setNames(data.frame(matrix(ncol = length(clnms), nrow = 5)), clnms)
    mt$parameter <- c('Median begin frequency [kHz]', 
                      'Median end frequency [kHz]', 
                      'Median mean frequency [kHz] (SD)', 
                      'Median duration [s]', 
                      'Median frequency range [kHz] (min-max)')
    mt$value <- c(freqBeg, 
                  freqEnd,
                  paste0(freqMean, " (", freqStdDev, ")"),
                  duration,
                  paste0(freqRange, " (", freqMin, " - ", freqMax, ")"))
    
    # attach to output
    wl$mt <- mt
  }
  return(wl)
}