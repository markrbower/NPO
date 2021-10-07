testSequentialParallelProcessing_rob <- function() {
  library(future)
  plan(multicore)
  
  f<-function(t) {j=0;for (k in seq(1,1E7)) j <- j+k; t*2}
  g<-function(a,b) {j=0;for (k in seq(1,1E8)) j <- j+k; a+b}
  
  # total jobs
  n<-20
  
  # number of tasks staged before waiting 
  flimit<-10 
  glimit<-5
  
  cat("preloading fs\n")
  f_futs=lapply(seq(1,flimit), function(x) {future({f(x)})})
  g_futs=list()
  
  next_f=flimit+1
  # for collecting results
  gs=list()
  
  for (i in seq(1,n-1)) {
    cat("doing", i, "\n")
    # make sure g(i)'s dependencies are ready
    a<-value(f_futs[[i]])
    b<-value(f_futs[[i+1]])
    # done with ith f value, discard now
    f_futs[[i]]=0 # NULL would shorten the list, I think
    cat("got fs",i,i+1,"\n")
    # submit g
    g_futs[[i]]<-future(g(a,b))
    # collect lowest remaining g  
    if (i > glimit) {
      gs[[i-glimit]]=value(g_futs[[i-glimit]])
      cat("got g", i-glimit, gs[[i-glimit]], "\n")
    }
    # start another f, if necessary
    cat( "next_f: ", next_f, "    n: ", n )
    if (next_f <= n) {
      f_futs[[next_f]]<-future(f(next_f))
      next_f<-next_f+1
    }
  }
  
  # gather remaining gs
  for (i in seq(n-glimit, n-1)) {
    gs[[i]]<-value(g_futs[[i]])
    cat("got g", i, gs[[i]], "\n")	
  }
}
