findTheLatestTimestampProcessed <- function( compArgs ) {
  dbp <- compArgs$findClass('databaseProvider')
  conn <- dbp$connect();
  #print( "Got connection")
  query <- paste0('select max(time) as T from ', compArgs$get('P'), ' where ')
  query <- paste0( query, 'subject=\'', compArgs$get('subject'), '\' and ')
  query <- paste0( query, 'channel=\'', tools::file_path_sans_ext(compArgs$get('channel')), '\' and ')
  query <- paste0( query, 'seizureUsed=', compArgs$get('centerTime'), ';')
  #print( query )
  tryCatch({
    rs <- DBI::dbGetQuery( conn, query )
  }, error=function(cond) {
    print( "Error in dbGetQuery" )
    print(cond)
  })
  #print( "Recordset obtained")
  if ( nrow(rs) == 0 ) {
    #print( "nrow = 0")
    Tstored <- -1
  } else if ( nrow(rs) == 1 ) {
    #print( "nrow = 1")
    if ( is.na(rs$T) ) {
      #print( "is na")
      Tstored <- -1
    } else {
      #print( "is number")
      Tstored <- rs$T
    }
  } else {
    print( 'ERROR! Too many rows found in createGraphAndBuffer...')
  }
  #print( "Out")
  DBI::dbDisconnect( conn )
  return( Tstored )
}
