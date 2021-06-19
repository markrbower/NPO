computeCCfwd <- function( CW, cc_threshold, ed_threshold, counter, time, voltage, allTime, allVoltage ) {
  # Assumee "time" and "voltage" contain a list

  IDX_list <- lapply( time, function(t) which( allTime > (t) & allTime <= (t+CW) ) ) # The "fwd" part
  
  VLTG_list <- mapply( function(idx,v) rep( v, times=length(idx)), IDX_list, voltage )
  
  IDX <- unlist( IDX_list )
  VLTG <- matrix( unlist(VLTG_list), ncol=7, byrow=TRUE )

  cc_all <- mapply( function(y,z) sapply( y, function(x) { cor(z, unlist(allVoltage[x])) } ), IDX, split(VLTG,row(VLTG)) )
  er_all <- mapply( function(y,z) sapply( y, function(x) { sum(z*z) / sum(unlist(allVoltage[x])*unlist(allVoltage[x])) } ), IDX, split(VLTG,row(VLTG)) )

  # Only keep CC > threshold  
  CC <- data.frame( counter=rep(counter,times=sum(lengths(IDX_list))),Tsource=rep(time,times=lengths(IDX_list)),Ttarget=unlist(allTime[IDX]), weight=unlist(cc_all) )
  
  # Filter, based on CC and energy
  keepIdx <- which( cc_all > cc_threshold & unlist(er_all) > ed_threshold & unlist(er_all) < 1/ed_threshold )
  CC <- CC[keepIdx,]
  
  # Create CC_next
  # Keep those elements whose target is beyond the largest value in "time"
  keepIdx <- which( CC$Ttarget > max(time) )
  CC_next <- CC[keepIdx,]
  CC_next$counter <- CC_next$counter + 1
  tmp <- CC_next$Ttarget
  CC_next$Ttarget <- CC_next$Tsource
  CC_next$Tsource <- tmp
  CC_next <- CC_next[ with(CC_next,order(Tsource,Ttarget)), ]

  # Pack
  result <- list( CC=CC, CC_next=CC_next )
  attr( result, 'timeStart' ) <- min( time )
  attr( result, 'timeStop'  ) <- max( time )
  
  return( result )
}


