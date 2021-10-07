isLocalPeak <- function( c_, width, filt_data ) {
  flag <- FALSE
  idx = (c_-width):(c_+width);
  bad <- which( idx<1 | idx>length(filt_data) )
  if ( length(bad) == 0 ) {
    check <- abs( filt_data[idx] )
    max_idx <- which( check == max(check) )
    if ( max_idx >=(width-4) & max_idx<=(width+5) ) {
      flag <- TRUE
    }
  }
  return( flag )
}
