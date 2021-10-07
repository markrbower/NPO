testSequentialParallelProcessing_notParallel <- function() {

  f<-function(t) {j=0;for (k in seq(1,1E7)) j <- j+k; t*2}
  g<-function(a,b) {j=0;for (k in seq(1,1E8)) j <- j+k; a+b}

  gs <- list()
  
  n <- 20
  
  a <- f(1)  
  for (i in seq(2,n)) {
    cat("doing", i, "\n")
    # make sure g(i)'s dependencies are ready
    b<-f(i)
    cat("got fs",i,i+1,"\n")
    # submit g
    gs[[i-1]]<-g(a,b)
    # step forward
    a <- b
  }
  
}
