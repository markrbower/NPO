parallelWindowNPO <- function() {
  set.seed(1)
  
  # Create data
  v <- matrix( nrow=4, ncol=7 )
  v[1,] <- c( 0, 0, 20, -30, -50, 0, 0 )
  v[2,] <- -1 * c( 0, 5, 20, -30, -50, -20, 0 )
  v[3,] <- c( 0, 10, 50, -120, -100, -80, -40 )
  v[4,] <- c( 0, 0, 0, 0, 0, 0, 0 )
  
  threshold <- 0.8
  
  NC <- 4 # number of categories
  N <- 10000 # total number of peaks
  T <- cumsum(rpois(N,lambda=5)+4) # blackout of 4
  L <- sample( rep( c(1,2,2,3,3,3,4,4,4,4), times=N/10 ), N, replace=FALSE )
  D <- t( sapply( 1:N, function(i) list( v[L[i],] + rnorm(n=7,mean=0,sd=5)) ) )
  
  # Make a dataframe of the time and data
  Dl <- list()
  for ( j in seq(1,N) ) { Dl <- append( Dl, D[j])}
  data <- as.data.frame( cbind( time=T, voltage=Dl ) )
  
  MW <- 10000 # Message Window: Each window should get 100 samples, so 10 windows total
  CW <- 2000  # Correlation Window
  
  # Compute message/map breaks
  messages <- list()
  t <- T[1]
  while ( t <= T[N] ) {
    idx <- which( T >= t & T < (t+MW) )
    messages <- append( messages, list(T[idx]) )
    t <- t + MW
  }
    
  # Loop on messages
  cntMessage <- 0
  if ( length(messages) > 0 ) {
    iter_messages <- itertools::ihasNext( iterators::iter( messages ) )
    message_pre <- list()
    if ( itertools::hasNext(iter_messages) ) {
      message <- iterators::nextElem( iter_messages )
      message <- as.numeric( unlist( message ) )
  
      # Mark, PLEASE read your own documentation and don't comment-out this section!    
      # Prime the CC and output structures
      dataIdx <- unlist( sapply( message, function(x) which(data$time==x)) )
      CC <- NPO:::computeCC( CW, cntMessage, message, data$voltage[dataIdx], data )
      output <- NULL
      
      while ( itertools::hasNext(iter_messages) ) {
        cntMessage <- cntMessage + 1
        message_post <- iterators::nextElem( iter_messages )
        message_post <- as.numeric( unlist( message_post ) )
        
        # Computes CC on "message_post"
        dataIdx <- unlist( sapply( message_post, function(x) which(data$time==x)) )
        CC <-  rbind( CC, NPO:::computeCC( CW, cntMessage, message_post, data$voltage[dataIdx], data ) )

        # Compute the neighborhood: The entire dataset or for each CW?!
        # Set it up both ways, but I believe the correct answer is "for each CW".
        #
        # Final data structure (called output):
        # timestamp   voltageVector   clusterid   incident   weights    votes
        # Computes output on "message"
        output <- rbind( output, NPO:::findLocalCommunitiesCW( output, CW, threshold, CC, unlist(message) ) ) # from previous nodes
        # This computation only finds results for "message"
        output <- NPO:::findGlobalCommunities( output, message ) # from subsequent nodes
        
        # Persist and drop earlier message
        # For now, 'output' will hold all results and will not be trimmed.
        # 'CC', however, will be trimmed.
        idx <- which( CC$Tsource %in% unlist(message_pre) )
        CC <- CC[-idx,]
        
        # Shift the messages
        message_pre <- message
        message <- message_post
        
      } # if there is a next message
    } # while messages hasNext
  } # length(messages)>0 
  
  # Test
  #
  # Final data structure (called output):
  #
  #   timestamp   clusterid   vector
  #
  # All examples in a given category should have the same clusterid.
  all_categories <- table( output$clusterid )
  all_sorted <- sort( all_categories )
  all_names <- as.numeric(names(all_sorted)) # Should be new name for original
  
  # Test
  valid <- TRUE
  # Test: NC categories.
  if ( length(all_categories) != NC ) {
    cat( crayon::bgGreen( crayon::red( "Not all caategories were found.\n" ) ) )
    valid <- FALSE
  }
  # Test: No category should be empty
  if ( length(which(all_categories>0)) != NC ) {
    cat( crayon::bgGreen( crayon::red( "Some caategories were empty.\n" ) ) )
    valid <- FALSE
  }
  
  for ( category in seq(1,4)) {
    idx <- which( output$clusterid == category )
    this_categories <- table( output$clusterid[idx] )
    this_sorted <- sort( this_categories )
    # Test: all should be in the max category
    Noutliers <- sum( this_sorted[2:NC] )
    if ( Noutliers > 0 ) {
      cat( crayon::bgGreen( crayon::red( "Not all examples were grouped together.\n" ) ) )
      valid <- FALSE
    }
  }
  
  if ( valid == TRUE ) {
    cat( crayon::bgGreen( crayon::green( "Success!\n" ) ) )
  }

}
