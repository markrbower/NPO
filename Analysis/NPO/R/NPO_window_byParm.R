NPO_window_byParm <- function( parms ) {
  #' @export
  
  print( "In NPO_window_byParm" )

  context <- SingletonInR$new()
  
  correlationWindow = parms$duration
  CCthreshold = parms$threshold
  
  L <- length( correlationWindow )
  
  hostname <- context$value$hostname
  if ( nchar(hostname) == 0 ) {
    hostname <- 'localhost'
  }
  password <- context$value$password
  if ( nchar(password) == 0 ) {
    password <- ''
  }
  
  for ( idx in seq(1,L) ) {
    NPO_window_testbed(dbName='testbed_results', path='/home/rstudio/r-docker-tutorial/2019_11_19_NetworkParameterOutlier/NPO/Data/Halo_data_from_Roni', taskName='preprocessing', institution='Yale', lab='NSME', experiment='Halo_test', subject='11', signalType='AP', centerTime=0, iterationType='directory', hostname=hostname, password=password, range=c(-1000,1000), correlationWindow=correlationWindow[idx], CCthreshold=CCthreshold[idx] )
  }
  
}


