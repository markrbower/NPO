createTablesForNPO <- function( conn, variables ) {
  # progress, P, M, and C tables.
  # Format: <experiment>_<subject>_<seizureTime>_<table_type>

  #print( paste0( "createTables: variables: correlationWindow: ", variables$correlationWindow ) )
  #print( paste0( "createTables: variables: CCthreshold: ", variables$CCthreshold ) )
  prefix <- NPO:::tableNamingLogic( variables )

  progress <- paste0( prefix, '_progress' )
  P <- paste0( prefix, '_P' )
  M <- paste0( prefix, '_M' )
  C <- paste0( prefix, '_C' )

  # Make sure that these tables are not already created.
  # progress
  if ( !(NPO:::sqlTableExists( conn, progress ) ) ) {
    query <- paste0( 'create table ', progress, ' like progress;' )
    print( query )
    DBI::dbSendQuery( conn, query )
  }

  # P
  if ( !(NPO:::sqlTableExists( conn, P ) ) ) {
    query <- paste0( 'create table ', P, ' like P;' )    
    DBI::dbSendQuery( conn, query )
  }
  
  # M
  if ( !(NPO:::sqlTableExists( conn, M ) ) ) {
    query <- paste0( 'create table ', M, ' like M;' )    
    DBI::dbSendQuery( conn, query )
  }
  
  # C
  if ( !(NPO:::sqlTableExists( conn, C ) ) ) {
    query <- paste0( 'create table ', C, ' like C;' )    
    DBI::dbSendQuery( conn, query )
  }
  
  table_names <- c( progress=progress, P=P, M=M, C=C )
  return( table_names )
}
