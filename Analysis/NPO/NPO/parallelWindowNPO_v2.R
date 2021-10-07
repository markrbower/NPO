parallelWindowNPO <- function() {
  # Built from parallelWindowNPO_v1.R
  # v2.0
  # June 10, 2021
  # Mark R. Bower
  # Yale University
  library(future)
  plan(multisession,workers=6) # "multisession" is portable, "multicore" is not
  
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Analysis/NPO/parallelWindowNPO_test.R')
  
  set.seed(1)
  
  # Create data
  v <- matrix( nrow=4, ncol=7 )
  v[1,] <- c( 0, 5, 20, -30, -50, 0, 0 )
  v[2,] <- -1 * c( 0, 5, 20, -30, -50, -20, 0 )
  v[3,] <- c( 0, 10, 50, -120, -100, -80, -40 )
  v[4,] <- c( 0, 0, 0, 0, 0, 0, 0 )
  
  # Model parameters
  SD = 5
  DRIFT_MAG = 2.0
  DRIFT_CNT = 200
  
  # Algorithm parameters (6 of them)
  bandpassFilterFrequency_High <- 6.0E3 # These aren't used here,
  bandpassFilterFrequency_Low  <- 0.6E3 # but are included for completeness.
  cc_threshold <- 0.85
  ed_threshold <- 0.5
  CW <- 2000  # Correlation Window duration
  firingRateThreshold <- 0.01 # In Hz

  MW <- 20000 # Message Window: Each window should get 100 samples, so 10 windows total
  
  NC <- 4 # number of categories
  N <- 50000 # total number of peaks
  T <- cumsum(rpois(N,lambda=5)+4) # blackout of 4
  L <- sample( rep( c(1,2,2,3,3,3,4,4,4,4), times=N/10 ), N, replace=FALSE )
  D <- matrix( unlist( sapply( 1:N, function(i) list( v[L[i],] + rnorm(n=7,mean=0,sd=SD)) ) ), ncol=7, byrow=TRUE )

  # Add drift to cluster2
  idx2 <- which( L == 2 )
  length2 <- length(idx2)
  begin_drift <- idx2[ round(length2/2) ]
  changed_idx <- which( idx2 >= begin_drift)
  drift_idx <- changed_idx[1:DRIFT_CNT]
  drifted_idx <- setdiff( changed_idx, drift_idx )
  idx_start <- drift_idx[1]
  idx_end <- drift_idx[DRIFT_CNT]
  D[idx2[drift_idx],] <- ( ( (DRIFT_MAG-1.0) / DRIFT_CNT ) * seq(1,DRIFT_CNT) + 1 ) * D[idx2[drift_idx],]
  D[idx2[drifted_idx],] <- DRIFT_MAG * D[idx2[drifted_idx],]
  
  # Make a dataframe of the time and data
  Dl <- list()
  for ( j in seq(1,N) ) { Dl <- append( Dl, list(D[j,]))}
  data <- as.data.frame( cbind( time=T, voltage=Dl ) )
  
  # Compute message/map breaks
  messages <- list()
  t <- T[1]
  while ( t <= T[N] ) {
    idx <- which( T >= t & T < (t+MW) )
    messages <- append( messages, list(T[idx]) )
    t <- t + MW
  }
  
  # Loop on messages IN PARALLEL
  
#  cl<-parallel::makeCluster(8,outfile="",setup_strategy = "sequential")
#  doParallel::registerDoParallel(cl)
  
  
  # Start implementing Rob's plan here ....
  
  # The "f()" function refers to computation of CCs.
  # The "g()" function refers to computation of "local" and "global" communities
  #               using the precedeing and following CC data
  # number of tasks staged before waiting 
  
  f_futs <- list() 
  flimit<-10 
  glimit<-5
  
  # Set up message iterator
  cntMessage <- 0
  message_pre <- list()
  iter_messages <- itertools::ihasNext( iterators::iter( messages ) )
  if ( itertools::hasNext(iter_messages) ) {
    message <- iterators::nextElem( iter_messages )
    message <- as.numeric( unlist( message ) )
    
    output <- NULL
    
    cat("preloading f's\n")
    while ( itertools::hasNext(iter_messages) & cntMessage < flimit ) {
      cntMessage <- cntMessage + 1
      print( paste0( cntMessage, " of ", length(messages) ) )
      message_post <- iterators::nextElem( iter_messages )
      message_post <- as.numeric( unlist( message_post ) )
      
      # Computes CC on "message" into "message_post"
      dataIdxMsg <- unlist( sapply( message, function(x) which(data$time==x)) )
      voltage <- data$voltage[dataIdxMsg]
      dataIdxBoth <- unlist( sapply( union(message,message_post), function(x) which(data$time==x)) )

# LINEAR vs. PARALLEL
      f_futs[[cntMessage]] <- future( NPO:::computeCCfwd( CW, cc_threshold, ed_threshold, cntMessage-1, message,
                                                          voltage, data$time[dataIdxBoth], data$voltage[dataIdxBoth] ) ) # , wait=TRUE
#      f_futs[[cntMessage]] <- NPO:::computeCCfwd( CW, cc_threshold, ed_threshold, cntMessage-1, message,
#                                                          voltage, data$time[dataIdxBoth], data$voltage[dataIdxBoth] ) # , wait=TRUE
      
      # "message" is only used to compute CC, which only looks forward, so I don't need a "message_post"
      message <- message_post
    }
    
    g_futs=list()
    
    # for collecting results
    gs=list()
    
    CC_pre <- list()
# LINEAR vs. PARALLEL
    CC <- value(f_futs[[1]])
#    CC <- f_futs[[1]]
    for ( cntGraph in seq( 1, length(messages) ) ) { # Make sure each g() gets started
      ###
      if ( itertools::hasNext(iter_messages) ) {
        # The CC part
        # Add another CC to the end to make sure one is always available for the graph computation.
        cntMessage <- cntMessage + 1
        print( paste0( cntMessage, " of ", length(messages) ) )
        message_post <- iterators::nextElem( iter_messages )
        message_post <- as.numeric( unlist( message_post ) )
        
        # Computes CC on "message" into "message_post"
        dataIdxMsg <- unlist( sapply( message, function(x) which(data$time==x)) )
        voltage <- data$voltage[dataIdxMsg]
        dataIdxBoth <- unlist( sapply( union(message,message_post), function(x) which(data$time==x)) )
        
# LINEAR vs. PARALLEL
        f_futs[[cntMessage]] <- future( NPO:::computeCCfwd( CW, cc_threshold, ed_threshold, cntMessage-1, message,
                                                            voltage, data$time[dataIdxBoth], data$voltage[dataIdxBoth] ) ) # , wait=TRUE
#        f_futs[[cntMessage]] <- NPO:::computeCCfwd( CW, cc_threshold, ed_threshold, cntMessage-1, message,
#                                                            voltage, data$time[dataIdxBoth], data$voltage[dataIdxBoth] ) # , wait=TRUE
        # "message" is only used to compute CC, which only looks forward, so I don't need a "message_post"
        message <- message_post
      }
      ###
      # The graph part.
      # CC results can now only be read when the vauls are obtained    
      # ... with this step waiting for a Future
      # NOTE: Each "f_futs[[...]]" is a self-contained CC structure with "pre", "", and "next" entries.
      if ( cntGraph < length(messages) ) {
# LINEAR vs. PARALLEL
        CC_strx <- value(f_futs[[cntGraph]])
#        CC_strx <- f_futs[[cntGraph]]
        CC_all <- rbind( CC_strx$CC_pre, CC_strx$CC, CC_strx$CC_next )
        CC <- CC_strx$CC
      } else {
        CC_all <- list()
      }

      # Compute this index for the graph operation
      messageTimeStart <- attr( CC_strx, 'timeStart' )
      messageTimeStop  <- attr( CC_strx, 'timeStop' )
      dataIdxGraph <- which(data$time>=messageTimeStart & data$time<=messageTimeStop)
      
      #
      # WHY?
      #
      # Why do I need to know ANY data$time OR data$voltage, given that I already have the CC values?!
      #
      # The only place they are used is to identify events by time to get the associated waveform to store in the output.
      #

# LINEAR vs. PARALLEL
      g_futs[[cntGraph]]<-future( NPO:::findCommunities( CC_all, CC, CW, unlist(data$time[dataIdxGraph]), data$voltage[dataIdxGraph] ) )
#      g_futs[[cntGraph]]<-NPO:::findCommunities( CC_all, CC, CW, unlist(data$time[dataIdxGraph]), data$voltage[dataIdxGraph] )
      
      # collect lowest remaining g  
      # This "the brake" that preveents too many g functions from loading up.
      if (cntGraph > glimit) { # glimit g()'s are running, so wait on the most latent "g" to finish.
# LINEAR vs. PARALLEL
        gs[[cntGraph-glimit]]=value(g_futs[[cntGraph-glimit]])
#        gs[[cntGraph-glimit]]=g_futs[[cntGraph-glimit]]
        cat("got g", cntGraph-glimit, "\n")
      }
    }
  }
  
  # At this point, all of the CC's are computed and waiting, all g's have been started, and "glimit" g's are still running.
  for (i in seq( (length(messages)-glimit+1), length(messages) ) ) {
# LINEAR vs. PARALLEL
    gs[[i]]<-value(g_futs[[i]])
#    gs[[i]]<-g_futs[[i]]
    cat("got g", i, "\n")	
  }
  
  # gs should now contain a list of "output" structures.

#  output <- NPO:::mergeSmallClusters( output, cc_threshold, ed_threshold )
  
  parallelWindowNPO_test( L, output )
  
  save(file="parallelWindowNPO_final.RData", output, L )
  
#  signalClusterNames <- separateSignalFromNoise( output, firingRateThreshold )
  
  
}
