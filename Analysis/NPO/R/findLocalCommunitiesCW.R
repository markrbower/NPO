findLocalCommunitiesCW <- function( output=NULL, CW, CC, message, times, waveforms ) {
  # Final data structure (called output):
  # timestamp   clusterid   incident   weights
  #
  # For each time, find the community.
  
  # Create the full graph
  #
  # The structure of CC:
  # message#, Tsource, Ttarget, weight 
  library(igraph)
  library(Matrix)
  
  # One difference with the existing solution is that CC are already computed.
  # Therefore, do I need to store the waveform in the vertex attrributes?

  # Normalize weights
  CC$weight <- (1 + CC$weight)/2
  
  # Create the full graph
  uniqueSource <- unique( as.numeric(CC$Tsource) )
  uniqueTarget <- unique( as.numeric(CC$Ttarget) )
  uniqueTimes <- sort( union( uniqueSource, uniqueTarget ) )
  
  grph <- igraph::graph.data.frame(CC[c('Tsource','Ttarget','weight')], directed=FALSE)

  # Process the sub-graphs
  for ( tCN in message ) {
    wv <- paste0( waveforms[[ which( unlist(times)==tCN) ]], collapse=',' )
    sub_grph <- igraph::induced_subgraph(grph, ( as.numeric(V(grph)$name)>=(tCN-CW) & as.numeric(V(grph)$name)<=(tCN+CW)),impl="auto")
    #    	compute communities
    sub_cliques <- cluster_louvain( sub_grph, weight=igraph::get.edge.attribute(sub_grph, 'weight' ) )
    # sub_cliques_i <- cluster_infomap( sub_grph, e.weights=igraph::get.edge.attribute(grph, 'weight' ) )
    # sub_cliques_w <- cluster_walktrap( sub_grph, weights=igraph::get.edge.attribute(grph, 'weight' ) 
    # sub_cliques_f <- cluster_fast_greedy( sub_grph, weights=igraph::get.edge.attribute(grph, 'weight' ), membership = TRUE )
    sub_cliques_membership <- membership( sub_cliques )
    clusterid <- sub_cliques_membership[as.numeric(names(sub_cliques_membership))==tCN]
    if ( length(clusterid) > 0 ) {
      member_idx <- which( sub_cliques_membership == clusterid )
      grph_clique <- igraph::induced_subgraph(sub_grph, member_idx )
      
      # needed: incident, weights
      edz <- igraph:::incident( grph_clique, topigraph:::IDv(grph_clique, tCN) )
      enz <- igraph:::ends( grph_clique, edz )
      weights <- edz$weight
      #    keep_idx <- which( as.numeric(mat[,2]) == tCN )
      str_incident <- paste0( as.numeric(setdiff( as.numeric(union( enz[,1], enz[,2] )), tCN )), collapse=",")
      str_weights <- paste0( weights, collapse=',' )
    } else {
      
      # Is tCN even in the graph?
      tCN_idx <- topigraph:::IDv(sub_grph, tCN)
      if ( length( unlist(tCN_idx) ) > 0 ) {
        edz <- igraph:::incident( sub_grph, tCN_idx )
        enz <- igraph:::ends( sub_grph, edz )
        weights <- edz$weight
        #    keep_idx <- which( as.numeric(mat[,2]) == tCN )
        str_incident <- paste0( as.numeric(setdiff( as.numeric(union( enz[,1], enz[,2] )), tCN )), collapse=",")
        str_weights <- paste0( weights, collapse=',' )
      } else {
        str_incident <- NA
        str_weights <- NA
      }
    }
    if ( is.null(output) ) {
      output <- data.frame( time=tCN, clusterid=NA, incident=str_incident, weights=str_weights, waveform=wv )
    } else {
      output <- rbind( output, data.frame( time=tCN, clusterid=NA, incident=str_incident, weights=str_weights , waveform=wv ) )
    }
  }
  return( output )
}

