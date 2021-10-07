mergeSmallClusters <- function( output, cc_threshold, ed_threshold ) {
  table_output <- sort( table( output$clusterid ), decreasing=TRUE )
  clusterids <- as.numeric( names( table_output ) )
  idx <- which( output$clusterid == clusterids[1] )
  averages <- colMeans( matrix( as.numeric( unlist( strsplit( output$waveform[idx], "," ))),byrow=TRUE,ncol=7))
  
  book = data.frame( rank=1, clusterid=clusterids[1], time=seq(1,7), avg=averages)

  # book[which(book$clusterid==1),'avg']

  for ( id in clusterids[-1] ) {
    idx <- which( output$clusterid == id )
    avg <- colMeans( matrix( as.numeric( unlist( strsplit( output$waveform[idx], "," ))),byrow=TRUE,ncol=7))
    # Metrics for existing, big clusters
    averages <- matrix( book$avg, ncol=7 )
    cc <- apply( averages, 1, function(x) { cor( avg, x ) } )
    er <- apply( averages, 1, function(x) { sum(x*x) / sum(avg*avg) } )
    # Comparison across vector ...
    logical_vector <- cc > cc_threshold & er > ed_threshold & er < 1/ed_threshold
    if ( any( logical_vector ) ) {
      rank_of_winner <- which( logical_vector )
      output$clusterid[idx] <- unique( book$clusterid[book$rank==rank_of_winner] )
    } else {
      # Make a new "big cluster" entry in the book
      book <- rbind( book, data.frame( clusterid=max(book$clusterid)+1, time=seq(1,7), avg=avg))
      # Modify output to reflect the new information
      
    }
  }
  # Modify clusterid numbers in 'output' here?
  
  
  return( output )
}

