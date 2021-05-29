P2M <- function( conn, case, signalType, P_table, M_table, context ) {
#  library( RMySQL )
#  library( stringr )
#  library( network )
#  library( sna )
#  library( GGally )
#  library( ggnetwork )
#  library( dplyr )
#  library( igraph )
  # Need to add 'timestamp' to the request for which event to use.
  # Maybe use the 'progress' table? You could mark a channel+seizure with '2' when done with this step and so on.
  
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/databaseInsertBuffer.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/IDv.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/computeNetworkParameters.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/summaryNetworkPlot.R')
  
  fields <- list("subject","channel","clusterid","seizureUsed","minT","maxT","duration","rate","waveform","energy","diameter","edge_density","degree", "hub_score", "mean_distance", "transitivity", "count" )
  dib <- databaseInsertBuffer( conn, M_table, fields, 20, 'clusterid' )

  #	select all unique clusterids
  query <- paste0( 'select distinct clusterid from ', P_table, ' where subject=', case$subject, ' and channel=\'', case$channel, '\' and seizureUsed=', case$seizureUsed, ';' )
  rs <- dbGetQuery( conn, query )
  clusterids <- rs$clusterid

  cnt <- 0
  # foreach ...
  for ( clusterid in clusterids ) {
    # Protection step: Make sure this clusterid does not already exist in the M-table.
#    query <- paste0( 'select clusterid from ', M_table, ' where subject=', case$subject, ' and channel=\'', case$channel, '\' and seizureUsed=', case$seizureUsed,' and clusterid=', clusterid, ';' )
#    rs_test <- dbGetQuery( conn, query_test )
#    if ( length(rs_test$clusterid) == 0 ) {
      cnt <- cnt + 1
      print( paste0( 'Cluster ', cnt, ' of ', length(clusterids) ) )
#      if ( cnt >= 51 ) {

      query <- paste0( 'select time,waveform,incident,weights from ', P_table, ' where subject=', case$subject, ' and channel=\'', case$channel, '\' and seizureUsed=', case$seizureUsed, ' and clusterid=', clusterid, ';' )
      rs <- dbGetQuery( conn, query )
      
      if ( length(rs$time) > 1 ) {
        # Make a new graph
        grph <- graph(edges=NULL,n=NULL,directed=FALSE)
        grph <- add_vertices( grph, length(rs$time), name=rs$time ) %>% set_vertex_attr( "waveform", value=rs$waveform )

        # Use 'incident' and 'weights' to make the connections.
        edgeList <- vector( mode='integer' )
        weightList <- vector( mode='double' )
        for ( vidx in seq(1,length(rs$time) ) ) {
          # Make the dataframe
          df <- data.frame( incident=csv2vec( rs$incident[vidx] ), weights=csv2vec( rs$weights[vidx] ) )
          
          # Remove any edges that connect to a vertex not in the group.
          drop <- which( is.element( df$incident, setdiff( df$incident, rs$time ) ) )
          if ( length(drop) > 0 ) {
            df <- df[-drop,]
          }        
          tmpEdgeList <- vector( mode='integer', length=2*length(df$incident) )
          if ( length(df$incident) > 0 ) {
            idx <- seq( from=1, to=2*length(df$incident), by=2 )
            tmpEdgeList[idx] <- IDv( grph, rs$time[vidx] )
            tmpEdgeList[idx+1] <- IDv( grph, df$incident )
            edgeList <- append( edgeList, tmpEdgeList )
              
            # Find the incident weights.
            weightList <- append( weightList, df$weights )          
          }
        }
        if ( length(rs$time) > 0 ) {
          grph <- add_edges( grph, edgeList ) %>% set_edge_attr( 'weight', value=weightList )
        }
        grph <- simplify( grph, remove.multiple = TRUE, remove.loops = TRUE )
        if ( length(grph) > 0 ) {
          #	compute graph properties
          fixed_parms <- list(subject=case$subject,channel=case$channel,clusterid=clusterid,seizureUsed=case$seizureUsed)
          parms <- computeNetworkParameters( grph )
          all_parms <- append( fixed_parms, parms )
            
          #	store graph properties to M table with subject, channel, seizureUsed, clusterid
          dib$insert( all_parms )
          
          # Generate a summary sheet for this clique
          p <- summaryNetworkPlot( case$subject, case$channel, case$seizureUsed, signalType, clusterid, parms, grph, context )
          filename <- print( paste0( "~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Presentation/Figures/summaryPtable_",case$subject,'_',case$channel,'_',case$seizureUsed,'_',clusterid,'.pdf' ) )
          pdf( file=filename, paper="letter" )
          print( p )          
          dev.off()
        }
      }
      
#      }
  } # End clusterids
  print( 'flushing' )
  dib$flush()
}
