computeCommunities_closure <- function( compArgs ) {
  # In theory, this function has access to a variable stored in "algo" called "data".
  #
  # Handle parameters: What parameter info is needed for this task?
  CW <- compArgs$get( 'correlationWindow' )
  compArgs <- compArgs

  # Compute the function
  function(CC) {
    if ( nrow(CC) > 0 ) {
      df <- findCommunities_localGlobal( CC, CW, compArgs )
    }
    return( df )
  }
}

