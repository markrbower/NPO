findCommunities <- function( CC_all, CC, CW, time, voltage ) {
  library(future)
  # A combined function call intended for use with "parallelWindowNPO.R"
  #
  # I cannot pass "output" into this because I do not know the state of output, anymore.
  # From previous nodes
  #
  # Originally, findLocalCommunitiesCW used "message" to identify its contained times.
  # Running with futures, the message variable will be out of sync with this computation.
  # CC contains those times, but it also contains others.
  # User 'parameters' to store the first and last times in the associated message.
  #
  
  output <- NPO:::findLocalCommunitiesCW( CW, CC, time, voltage )
  
  # This computation only finds results for "message"
  output <- NPO:::findGlobalCommunities( output, CC ) # from subsequent nodes
  
}

