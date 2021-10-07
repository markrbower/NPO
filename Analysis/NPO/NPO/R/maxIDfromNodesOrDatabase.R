maxIDfromNodesOrrDatabase( nodes, compArgs ) {
  
  if ( all( is.na( nodes$clusterid ) ) ) {
    maxID <- 0
  } else {
    maxID <- as.numeric(max( nodes$clusterid, na.rm=TRUE ))
  }
  maxID <- maxID + 1
  nodes$clusterid = as.numeric(maxID)  #
  
}
