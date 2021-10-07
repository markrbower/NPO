isLocalPeak_IIS <- function( filt_data, c_, width ) {
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

#  flag = (filt_data[c_] == min(filt_data[idx])) | (filt_data[c_] == max(filt_data[idx]));  
}
