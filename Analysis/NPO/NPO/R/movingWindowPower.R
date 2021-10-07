movingWindowPower <- function() {
  library(topsecret)
  library(meftools)
  # Set up the filename
  
  # Load the data file iterator
  topsecret::init_here()
  mef_filename <- here("mef2/CSC33.mef")
  vault <- topsecret::get_secret_vault()
  password <- secret::get_secret("MEF_password", key = secret::local_key(), vault = vault)
  info <- meftools::mef_info( c( mef_filename,
                                 secret::get_secret("MEF_password",
                                                    key = secret::local_key(), vault = vault) ) )
  samplingPeriod <- 1 / info$header$sampling_frequency
  
  iter_cont <- meftools::MEFcont( mef_filename, topsecret::get("MEF_password"))
  # Iterate windows
  result <- data.frame( power <- NULL, times <- NULL )
  while (   iter_cont$hasNext() ) {
    iter_data <- iter_cont$nextElem()
    while ( iter_data$hasNext() ) {
      voltage <- iter_data$nextElem()
#      voltage <- ts( data, frequency = 32000 )

      # Break up the data
      breaks <- ceiling( length(voltage) / 4 )
      endPoints <- c( seq( from=1, to=length(voltage)-breaks/2, by=breaks ), length(voltage)+1 )
      L <- length(endPoints)
      ep <- data.frame( start=endPoints[1:L-1], stop=endPoints[2:L]-1 )

      for ( row in 1:nrow(ep) ) {
        start <- ep[row,'start']
        stop  <- ep[row,'stop']
        # Compute power
        x <- voltage - mean(voltage[start:stop])
        P <- sqrt(mean(x^2))
        T <- attr( voltage, 't0' ) + samplingPeriod * 1E6 * (start-1)
               
        # Store into vector
        result <- rbind( result, data.frame(power=P,times=T))
      }
    }

  }  
  return( result )  
}
