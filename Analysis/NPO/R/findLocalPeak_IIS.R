findLocalPeak_IIS <- function( filt_data, c_, width ) {
  max_idx <- 0
  idx = (c_-width):(c_+width);
  bad <- which( idx<1 | idx>length(filt_data) )
  if ( length(bad) == 0 ) {
    check <- abs( filt_data[idx] )
    max_idx <- which( check == max(check) )
  }
  return( max_idx )

#  flag = (filt_data[c_] == min(filt_data[idx])) | (filt_data[c_] == max(filt_data[idx]));  
}
