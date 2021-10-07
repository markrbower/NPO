findLocalCommunities <- function( CW, CC ) {
  # Final data structure (called output):
  # The structure of CC:
  library(Matrix)

  # One difference with the existing solution is that CC are already computed.
  # Therefore, do I need to store the waveform in the vertex attrributes?
  
  # Create the full graph
  uniqueSource <- unique( CC$Tsource )
  uniqueTarget <- unique( CC$Ttarget )
  uniqueTimes <- sort( union( uniqueSource, uniqueTarget ) )
  grph <- igraph::graph.data.frame(CC[c('Tsource','Ttarget','weight')], directed=FALSE)

  # Process the sub-graphs
  N <- length(uniqueSource)
  output <- data.frame(time=vector(mode='integer',length=N),waveform='',str_incident='',str_weights='',clusterID=NA )
  for ( idx in seq(1,length(uniqueSource) ) ) {
    tCN <- round(as.numeric(uniqueSource[idx]))
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
    edz <- incident( grph_clique, topigraph::IDv(grph_clique, tCN) )
    enz <- ends( grph_clique, edz )
    weights <- edz$weight
    mat <- cbind( enz, weights )
    keep_idx <- which( as.numeric(mat[,1]) == tCN )
    str_incident <- paste0( mat[keep_idx,2], collapse=',' )
    str_weights <- paste0( round(as.numeric(mat[keep_idx,3]),4), collapse=',' )
    output$time[idx] <- tCN
    cc_idx <- which( CC$Tsource == tCN )
    output$waveform[idx] <- CC$WVsource[cc_idx[1]]
    output$incident[idx] <- str_incident
    output$weights[idx] <- str_weights
#    output$clusterID[idx] <- clusterid # REMEMBER: This function doesn't assign clusterid's!!!!
  }
  return(output)
}

