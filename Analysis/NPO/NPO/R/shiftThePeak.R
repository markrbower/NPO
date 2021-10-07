shiftThePeak <- function( x, width, data ) {
  
  max_idx <- 0
  idx = (x-width):(x+width)
  bad <- which( idx<1 | idx>length(data) )
  if ( length(bad) > 0 ) {
    idx <- idx[-bad]
  }
  check <- abs( data[idx] )
  max_idx <- which( check == max(check) )
  return( idx[max_idx] )
  
}
