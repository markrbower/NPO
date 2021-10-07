setupResultsDatabase <- function( variables ) {
  #' @export
  
  library( DBI )
  library( RMySQL )
  
  hostname <- NPO:::parseArg( variables, 'hostname' )
  if ( length(hostname) == 0 ) {
    hostname <- 'localhost'
  }
  dbName <- NPO:::parseArg( variables, 'dbName' )
  if ( length(dbName) == 0 ) {
    hostname <- 'mysql'
  }
  db_user <- NPO:::parseArg( variables, 'db_user' )
  if ( length(db_user) == 0 ) {
    db_user <- 'root'
  }
  password <- NPO:::parseArg( variables, 'password' )
  if ( length(password) == 0 ) {
    password <- ''
  }
  print( paste0( "hostname: ", hostname ) )
  print( paste0( "password: ", password ) )

  conn <- DBI::dbConnect( RMySQL::MySQL(), user=db_user, password=password, host=hostname, dbName=dbName)

  query <- paste0( "CREATE DATABASE IF NOT EXISTS testbed_results;" )
  DBI::dbGetQuery( conn, query )
  
  query <- paste0( "USE testbed_results;" )
  DBI::dbGetQuery( conn, query )
  
  query <- paste0( "CREATE TABLE IF NOT EXISTS testbed_halo (subject varchar(32), channel varchar(32), cw int, CCthreshold float, user float, sys float, elapsed float);" )
  DBI::dbGetQuery( conn, query )
  
  DBI::dbDisconnect( conn )

  NPO:::createBaseNPOtables( dbName="testbed_results", hostname=hostname, password=password )

#  populateBehaviorTablesForTestbed( dbName="testbed_results", hostname=hostname, password=password )

}

