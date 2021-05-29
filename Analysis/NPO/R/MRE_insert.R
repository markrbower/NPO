MRE_insert <- function( db_user, password, host, dbname ) {
  #' @export
  
  conn <- DBI::dbConnect( RMySQL::MySQL(),
                          user=db_user,
                          password=password,
                          host=host,
                          dbname='sys')

  query <- paste0( "CREATE DATABASE IF NOT EXISTS MRE_test;" )
  DBI::dbGetQuery( conn, query )
  
  query <- paste0( "USE MRE_test;" )
  DBI::dbGetQuery( conn, query )
  
  query <- paste0( "CREATE TABLE IF NOT EXISTS test (value INT);" )
  DBI::dbGetQuery( conn, query )
  
  query <- paste0( "TRUNCATE test;" )
  DBI::dbGetQuery( conn, query )
  
  query <- paste0( "insert into test (value) values (5);")
  DBI::dbGetQuery( conn, query )
  
  DBI::dbDisconnect( conn )
}
