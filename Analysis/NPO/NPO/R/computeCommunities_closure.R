computeCommunities_closure <- function( compArgs ) {
  # In theory, this function has access to a variable stored in "algo" called "data".
  #
  # Handle parameters: What parameter info is needed for this task?
  CW <- compArgs$get( 'correlationWindow' )

  # Compute the function
  function(CC) {
    findCommunities_localGlobal( CC, CW )
  }
}

