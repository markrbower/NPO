NPO_testbed <- function(...) {
  library(NPO)
  # Run by a docker container
  # Start a network and MySQL container named "mysql-server", first.
  #' @export
  
  args <- list(...)
  
  # Files converted from .ncs by "convertHaloData.R"
  library( DBI )
  library( RMySQL )

  # Find data
  #print( "Looking for files" )
  mefFiles <- list.files("/data/Halo_data_from_Roni",pattern="mef",full.names=TRUE)
  L <- length( mefFiles )
  print( paste0( L, " files to process." ) )
  
  #print( "Setup" )
  NPO::setupResultsDatabase( args )

  # Set parameter grid (window duration, CC threshold,  )
#  durations <- c(5E5,1E6,5E6)
  durations <- c(5E4,10E4,50E4,100E4) # for software test
#  thresholds <- c( 0.5, 0.7, 0.9 )
  thresholds <- c( 0.7, 0.8, 0.9 ) # for software test
  parms <- expand.grid( threshold=thresholds, duration=durations )
  L <- nrow(parms)

  # Loops
#  apply( parms, 1, function(x) NPO_window_byParm(x) )
#  print( "Going into NPO_window_byParm" )
#  NPO_window_byParm( parms ) # for software test

  correlationWindow = parms$duration
  CCthreshold = parms$threshold

  hostname <- NPO:::parseArg( args, 'hostname' )
  if ( length(hostname) == 0 ) {
    hostname <- 'localhost'
  }
  dbName <- NPO:::parseArg( args, 'dbName' )
  if ( length(dbName) == 0 ) {
    hostname <- 'mysql'
  }
  db_user <- NPO:::parseArg( args, 'db_user' )
  if ( length(db_user) == 0 ) {
    db_user <- 'root'
  }
  password <- NPO:::parseArg( args, 'password' )
  if ( length(password) == 0 ) {
    password <- ''
  }
  
  #print( L )
  for ( idx in seq(1,L) ) {
    NPO_window_testbed(dbName='testbed_results', path='/data/Halo_data_from_Roni', taskName='preprocessing', institution='Yale', lab='NSME', experiment='Halo_test', subject='11', signalType='AP', centerTime=0, iterationType='directory', hostname=hostname, dbName=dbName, db_user=db_user, password=password, range=c(-1000,1000), correlationWindow=correlationWindow[idx], CCthreshold=CCthreshold[idx] )
  }
  
}

