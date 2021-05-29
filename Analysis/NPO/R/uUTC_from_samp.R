uUTC_from_samp <- function( samp, TOC, header) {
  
  # Is there a general form for sample# -> UTC -> sample# ?

#  indx = binarySearch(mef_TOC[3,], samp, -1, -1);
  indx = findInterval( samp, TOC[3,] )

  uUTC = TOC[1, indx] + round(1E6*(samp - TOC[3, indx])/header$sampling_frequency )
}
