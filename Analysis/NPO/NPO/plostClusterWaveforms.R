plotClusterWaveforms <- function( output, data, clusterid ) {
  # After timeVoltage2tibble.R
  #
  # Get the 'output' variable by running:
  #   load(file='output.RData')
  library( ggplot2 )
  library( reshape2 )
  library( plyr )
  
  idx <- which( output$clusterid == clusterid )
  volt <- data$voltage[idx]

  df <- data.frame()
  N <- nrow( volt )
  for ( idx in seq(1,N) ) {
    df <- rbind( df, data.frame( g=rep(idx,7), x=seq(1,7), y=volt[idx,] ) )
  }

  #plot
  tib %>% ggplot( aes(x=x, y=y, group=g)) + geom_line()
}

