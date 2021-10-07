computeCC <- function( CW, counter, time, voltage, data ) {
  # Assumee "time" and "voltage" contain a list
  IDX <- lapply( time, function(t) which( data$time > (t) & data$time <= (t+CW) & data$time!=t ) )
  cc <- mapply( function(y,z) sapply( y, function(x) { cor(z, unlist(data$voltage[x])) } ), IDX, voltage )
  output <- data.frame( counter=rep(counter,times=sum(lengths(IDX))), Tsource=rep( time, times=lengths(IDX)), Ttarget=unlist( data$time[ unlist(IDX)] ), weight=unlist(cc) )
}


