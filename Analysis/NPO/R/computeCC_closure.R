computeCC_closure <- function( compArgs ) {
  # Handle parameters: What parameter info is needed for this task?
  CW <- compArgs$get( 'correlationWindow' )
  cc_threshold <- compArgs$get( 'CCthreshold' )
  ed_threshold <- compArgs$get( 'EDthreshold' )
  counter <- compArgs$get( 'counter' )
  prev_named_peak_matrix <- NULL
  ca <- compArgs
  
  # Compute the function
  function(named_peak_matrix) {
    # Default and check
    CCandWaveforms <- data.frame()
    if ( nrow( named_peak_matrix ) > 0 ) {
#      print( "Running computeCC_closure" )
      CCandWaveforms <- computeCCbwd( CW, cc_threshold, ed_threshold, prev_named_peak_matrix, named_peak_matrix, ca )
      attr( CCandWaveforms, 'counter' ) <- attr( named_peak_matrix, 'counter' )
      prev_named_peak_matrix <<- named_peak_matrix
    }
    return( CCandWaveforms ) # This should be a dataframe, unlike the input.
  }
}


 