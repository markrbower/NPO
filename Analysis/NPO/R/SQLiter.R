SQLiter <- function( db, query ) {
  #' @export
  #' 
  #
  library( iterators )
  library( itertools )
  i <- 1
    
  resultset <- DBI::dbGetQuery( db, query )

  it <- iterators::iter( resultset, by="row" )

  nextEl <- function() {
    n <- iterators::nextElem(it)
  }

  obj <- list(nextElem=nextEl)
  class(obj) <- c('SQLiter', 'abstractiter', 'iter')
  returnable <- ihasNext(obj)
  attr( returnable, "size" ) <- nrow( resultset )
  return( returnable )
}
