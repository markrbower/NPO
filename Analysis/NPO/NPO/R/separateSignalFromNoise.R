separateSignalFromNoise <- function( output, rateThreshold ) {
  counts <- table( table( output$clusterid ) )

#  count_vec[ as.numeric(names(counts)) ] <- counts
#  t <- seq(1,length(count_vec))
#  df <- tibble(t = t, y = count_vec)
#  # Fit an exponential
#  fit <- nls(y ~ SSasymp(t, yf, y0, log_alpha), data = df)
#  c <- coef( fit )
#  yf <- c['yf']
#  y0 <- c['y0']
#  alpha <- exp( c['log_alpha'])
#  y_ <- yf + (y0-yf)*exp(-alpha*t)

  # Scale the firing rate so that the highest-rate cluster fires at 1 Hz
  duration <- max( output$time ) - min( output$time )
  nbrOfMembers <- as.numeric( names( counts) )
  scaleFactor <- duration / max(nbrOfMembers)
  threshold <- 0.01 * duration / scaleFactor  # Firing rate in Hz
  
  idx_signal <- which( nbrOfMembers > threshold )
  signalClusterNames <- which( table(output$clusterid) %in% names(counts)[idx_signal] )
  return( signalClusterNames )
}

