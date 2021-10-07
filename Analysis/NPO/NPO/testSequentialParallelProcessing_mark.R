testSequentialParallelProcessing_mark <- function() {
  library(future)
  plan(multisession,workers=8) # "multisession" is portable, "multicore" is not
  
  f<-function(t) {j=0;for (k in seq(1,1E7)) j <- j+k; t*2}
  g<-function(a,b) {j=0;for (k in seq(1,1E8)) j <- j+k; a+b}
  
  # total jobs
  n<-500
  
  # number of tasks staged before waiting 
  flimit<-10 
  glimit<-5
  f_offset <- 0
  
  #cat("preloading fs\n")
  f_futs=lapply(seq(1,flimit), function(x) {future({f(x)})})
  g_futs=list()
  
  next_f=flimit+1
  # for collecting results
  gs=list()
  
  for (i in seq(1,n-1)) {
    cat("doing", i, "  ")
    # make sure g(i)'s dependencies are ready
    a<-value(f_futs[[i-f_offset]])
    b<-value(f_futs[[i+1-f_offset]])
    # done with ith f value, discard now
    # NULL would shorten the list, I think [Rob Bjornsen]
    # But, setting a list element to NULL shifts all other addresses
    # so we need an offset [Mark]
    f_futs[[i-f_offset]]=0 
#    f_offset <- f_offset + 1
    
    #cat("got fs",i,i+1,"\n")
    # submit g
    g_futs[[i]]<-future(g(a,b))
    # collect lowest remaining g  
    if (i > glimit) {
      gs[[i-glimit]]=value(g_futs[[i-glimit]])
      #cat("got g", i-glimit, gs[[i-glimit]], "\n")
    }
    # start another f, if necessary
    #cat( "next_f: ", next_f, "    n: ", n, "\n" )
    if (next_f <= n) {
      #cat( "computing f# ", next_f, "\n" )
      f_futs[[next_f-f_offset]]<-future(f(next_f))
      next_f<-next_f+1
    }
  }
  
  # gather remaining gs
  for (i in seq(n-glimit, n-1)) {
    gs[[i]]<-value(g_futs[[i]])
    #cat("got g", i, gs[[i]], "\n")	
  }
}
