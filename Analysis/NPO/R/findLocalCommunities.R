findLocalCommunitiesCW <- function( CW, CC, TIMES ) {
  # Final data structure (called output):
  # timestamp   voltageVector   clusterid   incident   weights    votes
  #
  # For each time, find the community.
  
  # Create the full graph
  #
  # The structure of CC:
  # group, Tsource, Ttarget, weight 
  library(Matrix)
  
  # One difference with the existing solution is that CC are already computed.
  # Therefore, do I need to store the waveform in the vertex attrributes?
  
  # Create the full graph
  uniqueSource <- unique( CC$Tsource )
  uniqueTarget <- unique( CC$Ttarget )
  uniqueTimes <- sort( union( uniqueSource, uniqueTarget ) )
  
  grph <- igraph::graph.data.frame(CC[c('Tsource','Ttarget','weight')], directed=FALSE)

  # Process the sub-graphs
  for ( idx in seq(1,nrow(CC)) ) {
    # Loop through the individual timestamps and find the subgraph
    sub_grph <- igraph::induced_subgraph(grph, ( as.numeric(V(grph)$name)>=(tCN-CW) & as.numeric(V(grph)$name)<=(tCN+CW)),impl="auto")
    #    	compute communities
    sub_cliques <- cluster_louvain( sub_grph, weight=igraph::get.edge.attribute(sub_grph, 'weight' ) )
    # sub_cliques_i <- cluster_infomap( sub_grph, e.weights=igraph::get.edge.attribute(grph, 'weight' ) )
    # sub_cliques_w <- cluster_walktrap( sub_grph, weights=igraph::get.edge.attribute(grph, 'weight' ) 
    # sub_cliques_f <- cluster_fast_greedy( sub_grph, weights=igraph::get.edge.attribute(grph, 'weight' ), membership = TRUE )
    sub_cliques_membership <- membership( sub_cliques )
    clusterid <- sub_cliques_membership[as.numeric(names(sub_cliques_membership))==tCN]
    member_idx <- which( sub_cliques_membership == clusterid )
    grph_clique <- igraph::induced_subgraph(sub_grph, member_idx )
    
    # Determine the majority of incident memberships already assigned.
    
    
    # needed: incident, weights
    edz <- incident( grph_clique, IDv(grph_clique, tCN) )
    enz <- ends( grph_clique, edz )
    weights <- edz$weight
    mat <- cbind( enz, weights )
    keep_idx <- which( as.numeric(mat[,2]) == tCN )
    str_incident <- paste0( mat[keep_idx,1], collapse=',' )
    str_weights <- paste0( round(as.numeric(mat[keep_idx,3]),4), collapse=',' )
  }

}

