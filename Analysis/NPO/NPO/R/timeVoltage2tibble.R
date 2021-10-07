timeVoltage2tibble <- function( data ) {
  # Struggling with how to represent vector data in dataframes.
  # Storing vectors as lists is compact (see 'overlayLinePlots'),
  # but it is hard to work with the data from there.
  #
  # Plot with: tib %>% ggplot( aes(x=x, y=y, group=g)) + geom_line()
  
  df <- data.frame()
  N <- nrow( data )
  for ( idx in seq(1,N) ) {
    df <- rbind( df, data.frame( g=rep(idx,7), x=seq(1,7), y=unlist(data$voltage[idx]) ) )
  }
  return( tibble(df) )
}



