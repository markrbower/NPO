computeCCbwd <- function( CW, cc_threshold, ed_threshold, prev_peak_matrix, peak_matrix ) {
  #
  print( "computeCCbwd" )
  
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

  # Remember that voltage is 32 rows by the number of peaks ...
  # Compute CC
  # Compute IDX: "to" < "from"
  IDX_to_list <- lapply( time[compute_idx], function(t) which( time > (t-CW) & time < (t) ) ) # The "bwd" part
  LL <- lengths(IDX_to_list)
  IDX_from_list <- lapply( compute_idx, function(x) rep(x,times=LL[x]) )
  # Unroll
  IDX_to <- unlist( IDX_to_list )
  IDX_from <- unlist( IDX_from_list )
  # Compute  
  cc_all <- mapply( function(idx_to,idx_from) cor( voltage[,idx_to], voltage[,idx_from]), IDX_to, IDX_from )
  er_all <- mapply( function(idx_to,idx_from) sum( voltage[,idx_to] * voltage[,idx_to] ) / sum( voltage[,idx_from] * voltage[,idx_from] ), IDX_to, IDX_from )

  # Create the return data frame
  V <- lapply( seq(1,length(time)), function(x) paste0(voltage[,x],collapse=',') )
  # Unroll
  counter=rep(attr(peak_matrix,'counter'),times=length(IDX_to))
  Tsource=rep(time,times=lengths(IDX_from_list))
  WVsource=rep(V,times=lengths(IDX_from_list))
  Ttarget=time[IDX_to]
  WVtarget=rep(V,times=lengths(IDX_to_list))
  weight=unlist(cc_all)
  # Keep the valid values
  keepIdx <- which( cc_all > cc_threshold & er_all < ed_threshold & er_all > (1/ed_threshold) )
  counter <- counter[keepIdx]
  Tsource <- Tsource[keepIdx]
  WVsource <- WVsource[keepIdx]
  Ttarget <- Ttarget[keepIdx]
  WVtarget <- WVtarget[keepIdx]
  weight <- weight[keepIdx]
  CC <- data.frame( counter=counter, Tsource=Tsource, Ttarget=Ttarget, weight=weight )
  CC$WVsource <- WVsource
  CC$WVtarget <- WVtarget

#  return( result )
  attr( CC, 'timeStart' ) <- min( time )
  attr( CC, 'timeStop'  ) <- max( time )
  return( CC )  
}


