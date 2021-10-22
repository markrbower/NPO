computePeaks_closure <- function( compArgs ) {
  # Handle parameters: What parameter info is needed for this task?
  waveform_mask <- compArgs$get( 'waveform_mask' )

  # set the filter parameters
  variables <- list()
  info <- compArgs$get('info')
  Flow  <- compArgs$get('filter_detect_lowF')  / ( info$header$sampling_frequency / 2 )
  Fhigh <- compArgs$get('filter_detect_highF') / ( info$header$sampling_frequency / 2 )
  variables$parms_filter_detect <- signal::butter( 3, c(Flow,Fhigh), 'pass' )
  Flow  <- compArgs$get('filter_keep_lowF')  / ( info$header$sampling_frequency / 2 )
  Fhigh <- compArgs$get('filter_keep_highF') / ( info$header$sampling_frequency / 2 )
  variables$parms_filter_keep <- signal::butter( 3, c(Flow,Fhigh), 'pass' )
  blackout <- compArgs$get('blackout')
  microsecondsPerSample <- 1E6 / info$header$sampling_frequency
  samplesInBlackout <- round( blackout / microsecondsPerSample )
  blackout_mask <- seq( -samplesInBlackout, samplesInBlackout )
    
  bufferSize <- compArgs$get('bufferSize')
  buffer <- vector( mode='double', length=bufferSize )

  # Compute the function
  function(input_data) {
    # Default and check
    peak_matrix <- matrix()
    if ( length( input_data ) > 0 ) {
      # filter
      filteredData <- NPO:::filterForSignalType( variables, buffer, input_data )
      # find
      fdata <- filteredData$filt_data_detect
      dw = diff(fdata);
      sdw = sign(dw);
      dsdw = diff(sdw);
      C = which( dsdw>0 | dsdw<0 ) + 1;
      peak_matrix <- NULL # default
      C <- C[ which( C > abs(min(blackout_mask)) | C < (length(C)-max(blackout_mask)) ) ]
      if ( length(C) > 0 ) {
        idx <- which( sapply( C, function(x) {NPO:::isLocalPeak_mask(x,blackout_mask,fdata)}) )
        idx <- C[ unlist( idx )]
        # Return the waveforms as a matrix.
        peak_matrix <- as.matrix( sapply( idx, function(x) { NPO:::shiftThePeak_mask(x,waveform_mask,fdata)} ) )
        # Add time of each event and return as a data frame.
        # attr( input_data, 't0' ) gives the time of the first sample
        # attr( input_data, 'dt' ) gives the time in microseconds between each sample
        T <- attr(input_data,'t0') + (idx-1)*attr(input_data,'dt')
        # The waveforms must be converted to strings to do this ...
        attr(peak_matrix, 'T' ) <- T
        attr( peak_matrix, 'counter' ) <- attr( input_data, 'counter' )
      }
    }
    return(peak_matrix)  # This variable is passed as the input to the next "bucket".
  }
}

