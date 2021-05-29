getSeizureTimes <- function( conn, context ) {
  #
  # 
  #
  #' @export

  query <-paste0( "select event_start from epochs where subject=", context$subject, " and label='seizure';" )
  times <- dbGetQuery( conn, query )
  
  
}
