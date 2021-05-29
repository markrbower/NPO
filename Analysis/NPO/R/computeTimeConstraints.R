computeTimeConstraints <- function( centerTime, info, windowInHours ) {
  # This should be tied to "filterCases" in "MEF_analysisLoop".
  # Perhps I could make a constraint structure of some type that could be passed to both functions?
  # Start with a vector of 'start' and 'stop' times.
  # 'windowInHours' is assumed to be a 2-element vector with relative [start,stop] times to 'centerTime'.
  
  result <- vector()
  result['isValid'] <- 0
  usWindow <- 1E6 * 3600 * windowInHours
  
  # If there are no discontinuities in the desired window:
  D <- info$ToC[ 1, which( info$discontinuities == 1 ) ]
  contiguousStarts <- which( info$discontinuities == 1 )
  contiguousStops <- c(contiguousStarts-1, ncol(info$ToC) )
  contiguousStops <- contiguousStops[-1]
  time0 <- centerTime - usWindow[1]
  time1 <- centerTime + usWindow[2]
  contiguousStartTimes <- info$ToC[1,contiguousStarts]

  # The time of the first "missing" sample is ...
  ds <- diff( info$ToC[3,] )
  contiguousStopTimes <- info$ToC[1,contiguousStops] + 1E6*ds[contiguousStops]/info$header$sampling_frequency
  # Append the end time of the entire file
  contiguousStopTimes <- c( contiguousStopTimes, info$header$recording_end_time )
  
  keepa <- union( which( contiguousStartTimes>=time0 & contiguousStartTimes<=time1), which( contiguousStopTimes>=time0 & contiguousStopTimes<=time1) )
  keepb <- which( contiguousStartTimes<=time0 & contiguousStopTimes>=time1 )
  keep <- sort( union( keepa, keepb ) )
  
  if ( length(keep)>0 ) {
    result['start'] <- time0
    result['stop'] <- time1
    result['usWindow'] <- usWindow
    result['isValid'] <- 1
  }
  return( result )
}
