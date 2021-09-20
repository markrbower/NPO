findGlobalCommunities <- function( nodes ) {
  # Connect communities by votes.
  #
  # nodes: dataframe containing the list of nodes
  # CC: dataframe describing connections between nodes.
  #
  # Two passes: 1st. look only at the past and only assign if:
  #               - there are no previous votes
  #               - all previous votes agree
  #               - the number of subsequent votes aren't sufficient to alter
  #             2nd. look at past+present and assign to majority

  # Round 1
  for ( idx in seq(1,nrow(nodes) ) ) {
    incident <- as.numeric(unlist(strsplit(nodes$incident[idx],",")))
    if ( length(incident) > 0 ) {
      pre <- which( incident < as.numeric(nodes$time[idx]) )
      if ( length(pre) > 0 ) { # if length==0, leave clusterid as is
        incident_pre <- incident[pre]
        incident_idx <- sapply( incident_pre, function(x) which(nodes$time==x))
        clusterids <- nodes$clusterid[incident_idx] 
        votes <- sort( table( clusterids, useNA='no' ), decreasing = TRUE )
        if ( length(votes) == 1 ) { # all previous votes agree
          nodes$clusterid = as.numeric(names(votes)[1])        
        } else if ( length(votes) > 1 ) { # can remaining votes overcome the difference?
          remaining_votes <- length(incident) - length(pre)
          vote_difference <- votes[1] - votes[2]
          if ( remaining_votes < vote_difference ) {
            nodese$clusterid = as.numeric(names(votes)[1])        
          }
        }
      } else { # length(pre)==0
        nodes$clusterid <- maxIDfromNodesOrDatabase( nodes ) + 1
      }
    } else { # This node is isolated
      nodes$clusterid <- maxIDfromNodesOrDatabase( nodes ) + 1
    }
  } # message_idx
#  save(file="parallelWindowNPO_2.RData",nodes)
  
  # Round 2
  undecided_idx <- which( is.na( nodes$clusterid) )
  for ( idx in undecided_idx ) {
    incident <- as.numeric(unlist(strsplit(nodes$incident,",")))
    incident_idx <- unlist( sapply( incident, function(x) which(nodes$time==x)) )
    if ( all(!is.na(incident), na.rm=TRUE) & length(incident) > 0 ) {
      #print( incident_idx )
      clusterids <- nodes$clusterid[incident_idx]
      votes <- sort( table( clusterids, useNA='no' ), decreasing = TRUE )
      if ( length(votes) > 0 ) { # If isolated, leave clusterid as "NA"
        nodes$clusterid = as.numeric(names(votes)[1])
      }
    }
  } # undecided_idx
  return( nodes )
}


