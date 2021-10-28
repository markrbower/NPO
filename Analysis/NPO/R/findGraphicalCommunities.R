findGraphicalCommunities <- function( CW, CC, compArgs ) {
  # Final data structure (called output):
  # The structure of CC:
  library(Matrix)

  # One difference with the existing solution is that CC are already computed.
  # Therefore, do I need to store the waveform in the vertex attrributes?
  cm <- compArgs$get( 'computation_mask' )
  
  # Create the full graph
  tryCatch({
    grph <- igraph::graph.data.frame(CC[c('Tsource','Ttarget','weight')], directed=FALSE)
  },error=function(cond) {
    print( CC[c('Tsource','Ttarget','weight')] )
    return()
  })
  uniqueSource <- unique( CC$Tsource )
  uniqueTarget <- unique( CC$Ttarget )
  uniqueTimes <- sort( union( uniqueSource, uniqueTarget ) )

  # Process the sub-graphs
  # The variable "output" will be the data structure sent to the "databaseInsertBuffer",
  # which means the column names need to match the database fields.
  N <- length(uniqueTarget)
  output <- data.frame(subject='',channel='',clusterid=vector(mode='integer',length=N),seizureUsed=vector(mode='integer',length=N),
                       peak=vector(mode='integer',length=N),energy=vector(mode='integer',length=N),
                       waveform='',incident='',weights='',UUID='')
  # Set constants
  output$subject <- compArgs$get('subject')
  output$channel <- compArgs$get('channel')
  output$clusterid <- 0
  case <- compArgs$get('case')
  output$UUID <- case$UUID
  # get the case
  case <- compArgs$findClass('metadataInformer')$get('case')
  output$seizureUsed <- as.numeric( unlist( case['centerTime'] ) )

  for ( idx in seq(1,length(uniqueTarget) ) ) {
    tryCatch({
      tCN <- as.numeric(uniqueTarget[idx])
      # Loop through the individual timestamps and find the subgraph
      sub_idx <- (as.numeric(igraph::V(grph)$name)>=(tCN-CW) & as.numeric(igraph::V(grph)$name)<=(tCN+CW))
      if ( length(which(sub_idx)) > 0 ) {
        sub_grph <- igraph::induced_subgraph(grph, sub_idx, impl="auto")
        #    	compute communities
        sub_cliques <- igraph::cluster_louvain( sub_grph, weight=igraph::get.edge.attribute(sub_grph, 'weight' ) )
        # sub_cliques_i <- cluster_infomap( sub_grph, e.weights=igraph::get.edge.attribute(grph, 'weight' ) )
        # sub_cliques_w <- cluster_walktrap( sub_grph, weights=igraph::get.edge.attribute(grph, 'weight' ) 
        # sub_cliques_f <- cluster_fast_greedy( sub_grph, weights=igraph::get.edge.attribute(grph, 'weight' ), membership = TRUE )
        sub_cliques_membership <- igraph::membership( sub_cliques )
        clusterid <- sub_cliques_membership[as.numeric(names(sub_cliques_membership))==tCN]
        member_idx <- which( sub_cliques_membership == clusterid )
        grph_clique <- igraph::induced_subgraph(sub_grph, member_idx )
        # Determine the majority of incident memberships already assigned.
        
        # needed: incident, weights
        edz <- igraph::incident( grph_clique, topigraph::IDv(grph_clique, tCN) )
        enz <- igraph::ends( grph_clique, edz )
        weights <- edz$weight
        mat <- cbind( enz, weights )
        keep_idx <- which( as.numeric(mat[,2]) == tCN )
        str_incident <- paste0( mat[keep_idx,1], collapse=',' )
        str_weights <- paste0( round(as.numeric(mat[keep_idx,3]),4), collapse=',' )
      } else {
        print( "empty subgraph" )
        return()
      }
    },error=function(cond){
      print(cond)
      return()
    })
    
    # Build entry for the output dataframe
    output$time[idx] <- tCN
    cc_idx <- which( CC$Ttarget == tCN )
    waveform_str <- CC$WVtarget[cc_idx[1]]
    output$waveform[idx] <- waveform_str
    waveform <- as.numeric( unlist( stringr::str_split(waveform_str,',') ) )
    abs_waveform <- abs( waveform[cm] )
    peak_idx <- which( abs_waveform == max( abs_waveform ) )
    output$peak[idx] <- abs_waveform[ peak_idx[1] ]
    output$energy[idx] <- sqrt( sum( waveform * waveform ) )
    output$incident[idx] <- str_incident
    output$weights[idx] <- str_weights
#    output$clusterid[idx] <- clusterid # REMEMBER: This function doesn't assign clusterid's!!!!
  }
  return(output)
}

