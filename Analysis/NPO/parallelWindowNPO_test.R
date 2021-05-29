parallelWindowNPO_test <- function( L, output ) {
  table_clusterid <- sort( table( output$clusterid ), decreasing=TRUE )
  
  # Are all waveforms in a cluster from the same source?
  idx_big_clusters <- which( table_clusterid > 5 )
  for ( clusterid in names(table_clusterid[idx_big_clusters]) ) {
    clusterid <- as.numeric( clusterid )
    idx <- which( output$clusterid == clusterid )
    
    table_cluster <- table(output$clusterid[idx])
    predicted <- table_cluster[which(names(table_cluster)==clusterid)]
    table_actual <- table( L[idx] )
    actual <- table_actual[1]
    
    pct <- 100.0 * predicted[1] / actual[1]

    print( paste0( clusterid, ": ", unname( pct ) ) )
    
    plotClusterWaveforms(output,clusterid)
  }

  # Do any waveforms from a cluster get assigned to low-count clusters or NA?
  for ( src in seq(1,3) ) {
    clusterids <- output$clusterid[ which( L == src ) ]
    print( paste0( src, ": " ) )
    print( table(clusterids ) )
  }
  
  
  
  
  
  
  
  
  
}  
#    cat( crayon::bgGreen( crayon::red( "Not all caategories were found.\n" ) ) )
