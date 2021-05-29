DIRiter <- function( dirPath, subject, time0 ) {
  # Arguments:
  # db    : MySQL database connection
  # query : "select ..." - it is assumed that this will always be a "select" query.
  #
  library( iterators )
  library( itertools )
  i <- 1
    
  lists <- list( channel=list.files( path=dirPath, pattern="*.mef" ) )
  # For an SQLiter, the resultset contains three fields: subject, channel and timestamp. How do I match that here?
  # The simplest answer is that I have to supply the subject and seizure fields, which must be replicated for each value.
  N <- length(lists$channel )
  lists$subject <- rep( subject, N )
  lists$centerTime <- rep( time0, N )
  
  df <- as.data.frame( lists )

  it <- iter( df, by="row" )

  nextEl <- function() {
    n <- nextElem(it)
  }
  
  obj <- list(nextElem=nextEl)
  class(obj) <- c('DIRiter', 'abstractiter', 'iter')
  returnable <- ihasNext(obj)
  attr( returnable, "size" ) <- nrow( df )
  return( returnable )
}
