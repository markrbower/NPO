findCommunities_localGlobal <- function( CC, CW ) {
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
  
  nodes <- NPO:::findLocalCommunities( CW, CC )
  
  # This computation only finds results for "message"
  nodes <- NPO:::findGlobalCommunities( nodes ) # from subsequent nodes
}

