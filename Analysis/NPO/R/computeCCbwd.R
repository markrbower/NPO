computeCCbwd <- function( CW, cc_threshold, ed_threshold, prev_peak_matrix, peak_matrix, compArgs ) {
  #
#  print( "computeCCbwd" )
  cm <- compArgs$get( 'computation_mask' )
  
  # Find those times in 'prev_peak_matrix' that connect to 'peak_matrix'
  prev_peaks <- NULL
  time <- attr( peak_matrix, 'T' )
  if ( is.null(prev_peak_matrix) ) {
    compute_idx <- seq( 1, length(time) )
  } else {
    T0 <- time[1]
    prev_time <- attr( prev_peak_matrix, 'T' )
    idx <- which( prev_time > ( T0 - 2*CW ) )
    time <- c( prev_time[idx], time )
    prev_peaks <- prev_peak_matrix[,idx]
    # Set the start of the analysis indices
    compute_idx <- which( time > (T0 - CW) )
  }
  voltage <- cbind( prev_peaks, peak_matrix )
  
  if ( ncol(voltage) > 0 ) {
    # Remember that voltage is 32 rows by the number of peaks ...
    # Compute CC
    # Compute IDX: "to" < "from"  (T"target" < T"source" )
    # Remember, all "forward" CC (to times after the "target" will be computed when "source" becomes the "target" )
    IDX_target_list <- lapply( time[compute_idx], function(t) which( time > (t-CW) & time < (t) ) ) # The "bwd" part
    LL <- lengths(IDX_target_list)
    IDX_source_list <- lapply( seq(1,length(LL)), function(x) rep(x,times=LL[x]) )
    # Unroll
    IDX_target <- unlist( IDX_target_list )
    IDX_source <- unlist( IDX_source_list )
    # Compute  
    cc_all <- mapply( function(idx_target,idx_source) cor( voltage[cm,idx_target],  voltage[cm,idx_source]), IDX_target, IDX_source )
    er_all <- mapply( function(idx_target,idx_source) sum( voltage[cm,idx_target] * voltage[cm,idx_target] ) /
                                                      sum( voltage[cm,idx_source] * voltage[cm,idx_source] ), IDX_target, IDX_source )
    # Create the return data frame
    V <- lapply( seq(1,length(time)), function(x) paste0( round(voltage[,x],1),collapse=',') )
    # Unroll
    counter=rep(attr(peak_matrix,'counter'),times=length(IDX_target))
    Tsource=time[IDX_source]
    tV <- t(V)
    WVsource=tV[,unlist(IDX_source_list)]
    Ttarget=time[IDX_target]
    WVtarget=tV[,unlist(IDX_target_list)]
    weight=unlist(cc_all)
    # Keep the valid values
    keepIdx <- which( cc_all > cc_threshold & er_all < ed_threshold & er_all > (1/ed_threshold) )
    counter <- counter[keepIdx]
    Tsource <- round( Tsource[keepIdx] ) # This is a weird problem of text-to-numeric at large values.
    WVsource <- WVsource[keepIdx]
    Ttarget <- round( Ttarget[keepIdx] ) # This is a weird problem of text-to-numeric at large values.
    WVtarget <- WVtarget[keepIdx]
    weight <- weight[keepIdx]
    CC <- data.frame( counter=counter, Tsource=Tsource, Ttarget=Ttarget, weight=weight )
    CC$WVsource <- WVsource
    CC$WVtarget <- WVtarget
  
  #  return( result )
    attr( CC, 'timeStart' ) <- min( time )
    attr( CC, 'timeStop'  ) <- max( time )
    return( CC )
  } else {
    return( NULL )
  }
}


