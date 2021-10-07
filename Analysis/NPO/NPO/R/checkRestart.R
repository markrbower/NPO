checkRestart <- function( args, conn, table_names, context ) {

  if ( str_length( parseArg( args, '--restart') ) > 0 ) {
    # Restart for ALL channels for this subject and centerTime
    
    # Progress
    progress_table <- table_names['progress']
    query <- paste0( "update ", progress_table, " set done=0 where subject=\'", context$subject, "\';" )
    DBI::dbSendQuery( conn, query )
    
    # P
    signal_table <- table_names['P']
    query <- paste0( "truncate ", signal_table )
    DBI::dbSendQuery( conn, query )
    
    # delete graph file
    filenames <- list.files( path=".", pattern=paste0( context$subject, "[[:alnum:]_.]+", "_graph.xml" ) )
    for ( filename in filenames ) 
      #Delete file if it exists
      file.remove( filename )
  }    
}
