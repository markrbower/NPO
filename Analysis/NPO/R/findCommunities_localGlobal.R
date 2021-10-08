findCommunities_localGlobal <- function( CC, CW, compArgs ) {
  library(future)
  # A combined function call intended for use with "parallelWindowNPO.R"
  #
  # I cannot pass "nodes" into this because I do not know the state of nodes, anymore.
  # From previous nodes
  #
  # Originally, findLocalCommunitiesCW used "message" to identify its contained times.
  # Running with futures, the message variable will be out of sync with this computation.
  # CC contains those times, but it also contains others.
  # User 'parameters' to store the first and last times in the associated message.
  #
  
  nodes <- NULL
  if ( !is.null(CC) & ncol(CC) > 0 ) {
    nodes <- NPO:::findGraphicalCommunities( CW, CC, compArgs )
    
    # This computation only finds results for "message"
    nodes <- NPO:::assignClusterids( nodes, compArgs ) # from subsequent nodes
  }
  return( nodes )
}

