testSequentialParallelProcessing <- function() {
  # Implement a two-step process in parallel
  # where the second part must wait for the completion of two preceding steps
  library(parallel)
  library(doParallel)
  library( foreach )
  
  cl<-parallel::makeCluster(4,outfile="",setup_strategy = "sequential")
  doParallel::registerDoParallel(cl)
  
  prev <- list()
  
  tasks <- seq(1,10)
  
  result <- list()
  
  ptm <- proc.time()
  
  for (task in tasks) {
    current <- list(task)
    
    # This is the sequential part: it depends on "prev"
    p <- mcparallel( {prev} )
    q <- mcparallel( {current} )
    r <- mccollect( list(p,q), wait=TRUE )
    print( paste0( "Start: ", r[[1]], ", ", r[[2]], " :End" ) )
    
    # This is the parallel part. It will write to a database, so results can arrive in any order
    pp <- mcparallel( append( result, paste0( "Start: ", r[[1]], ", ", r[[2]], " :End" ) ) )
    # Even though I don’t care about order here, saying “wait=FALSE” causes the function to hang, eventually
    result <- mccollect( pp, wait=TRUE )
    
    # Slide
    prev <- current
  }
  print(   proc.time() - ptm )
  print( unname( unlist(result) ) )
  print( "Done")
  
  stopCluster( cl )
}
