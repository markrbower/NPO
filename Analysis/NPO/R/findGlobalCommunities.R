findGlobalCommunities <- function( output, message ) {
  # Connect communities by votes.
  #
  # Usage:
  # output <- NPO:::findGlobalCommunities( output ) # from subsequent nodes
  # data.frame( time, clusterid=clusterid, incident=str_incident, weights=str_weights )
  #
  # Two passes: 1st. look only at the past and only assign if:
  #               - there are no previous votes
  #               - all previous votes agree
  #               - the number of subsequent votes aren't sufficient to alter
  #             2nd. look at past+present and assign to majority

  # Round 1
  save(file="parallelWindowNPO.RData",output)
  
  message_idx <- sapply( message, function(x) which(output$time==x))
  for ( idx in message_idx ) {
    incident <- as.numeric(unlist(strsplit(output[idx,'incident'],",")))
    if ( length(incident) > 0 ) {
      pre <- which( incident < as.numeric(output$time[idx]) )
      if ( length(pre) > 0 ) { # if length==0, leave clusterid as is
        incident_pre <- incident[pre]
        incident_idx <- sapply( incident_pre, function(x) which(output$time==x))
        clusterids <- sapply( incident_idx, function(x) as.numeric(output[x,'clusterid']) ) 
        votes <- sort( table( clusterids, useNA='no' ), decreasing = TRUE )
        if ( length(votes) == 1 ) { # all previous votes agree
          output[idx,'clusterid'] = as.numeric(names(votes)[1])        
        } else if ( length(votes) > 1 ) { # can remaining votes overcome the difference?
          remaining_votes <- length(incident) - length(pre)
          vote_difference <- votes[1] - votes[2]
          if ( remaining_votes < vote_difference ) {
            output[idx,'clusterid'] = as.numeric(names(votes)[1])        
          } else { # yes
            output[idx,'clusterid'] = NA  # Decide this in Round 2
          }
        } else { # all votes were 'NA'
          output[idx,'clusterid'] = NA  # Decide this in Round 2
        }
      } else { # length(pre)==0
        if ( all( is.na( output$clusterid ) ) ) {
          maxID <- 0
        } else {
          maxID <- as.numeric(max( output$clusterid, na.rm=TRUE ))
        }
        maxID <- maxID + 1
        output[idx,'clusterid'] = as.numeric(maxID)  #
      }
    } else {
      if ( all( is.na( output$clusterid ) ) ) {
        maxID <- 0
      } else {
        maxID <- as.numeric(max( output$clusterid, na.rm=TRUE ))
      }
      maxID <- maxID + 1
      output[idx,'clusterid'] = as.numeric(maxID)  #
    }
  } # message_idx
  save(file="parallelWindowNPO_2.RData",output)
  
  # Round 2
  undecided_idx <- which( is.na( output[message_idx,'clusterid']) )
  for ( idx in undecided_idx ) {
    incident <- as.numeric(unlist(strsplit(output[idx,'incident'],",")))
    incident_idx <- unlist( sapply( incident, function(x) which(output$time==x)) )
    if ( all(!is.na(incident), na.rm=TRUE) & length(incident) > 0 ) {
      #print( incident_idx )
      clusterids <- sapply( incident_idx, function(x) as.numeric(output[x,'clusterid']) ) 
      votes <- sort( table( clusterids, useNA='no' ), decreasing = TRUE )
      if ( length(votes) > 0 ) { # If isolated, leave clusterid as "NA"
        output[idx,'clusterid'] = as.numeric(names(votes)[1])
      }
    }
  } # undecided_idx
  return( output )
}


