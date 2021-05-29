peakFinder <- function( parameters, correlationWindow, state ) {
  #print( "In peakFinder")
  #print( "state")
  #print( state)

  cc_th <- parameters$CCthreshold
  ed_th <- parameters$EDthreshold
  
  #print( paste0( 'findPeaks: cc_th=', cc_th, ' and ed_th=', ed_th ) )
  
  skipSize <- 1
  width <- 20
  mid_idx <- seq((width-3),(width+5))
  
  mask <- seq(from=-width,to=width,by=1)
  local_width <- 40
  
  eventsList <- list()
  it <- iter( eventsList )
#  if (!is.null(it$hasNext)) return(it)
  cache <- NULL
  has_next <- NA
  
  #print( "Midway")
  
  findPeaksIn <- function( filteredData ) {
    # Timestamp of the first value
    s0 <- filteredData$s0 
    t0 <- filteredData$t0
    dt <- filteredData$dt
    info <- filteredData$info
    
    filt_data_detect <- filteredData$filt_data_detect
    filt_data_keep <- filteredData$filt_data_keep
    eventsList <<- list()  
    
    # Find candidate peaks
    dw = diff(filt_data_detect);
    sdw = sign(dw);
    dsdw = diff(sdw);
    C = which( dsdw>0 | dsdw<0 ) + 1;
    N = length(C);
    
    bad = which( C < (abs(min(mask))+1) | C > ((length(filt_data_detect)-max(mask))-1) );
    if ( length(bad) > 0 ) {
      C <- C[-bad];
    }
  
    # Find peaks
  #  print( paste0( "3.2 ", attr( grph, 'masterID' ) ) )
    print( paste0( "Checking ", length(C), " peaks." ) )
    peakIdxCnt <- 0
    iterC <- ihasNext( iterators::iter( C ) )
    # print( "Starting the loop" )
    while( hasNext( iterC ) ) {
      c_ <- nextElem(iterC)
      if ( isLocalPeak_IIS(filt_data_detect, c_, local_width) ) {
        # print( "Peak" )
        localPeak <- findLocalPeak_IIS( filt_data_detect, c_, local_width )
        if ( localPeak>=(local_width-2) & localPeak<=(local_width+4) ) { # IIS
          # Shift to the actual peak
#          localPeak <- findLocalPeak_IIS( filt_data_keep, c_, local_width )
          c_ <- c_ + localPeak - local_width - 1
#          print( c_ )
          mask_idx <- c_+mask
          if ( min(mask_idx)>0 & max(mask_idx)<=length(filt_data_keep) ) {
            tryCatch({
              peakIdxCnt = peakIdxCnt + 1;
              wvfrm = filt_data_keep[c_+mask];
              bad <- which( is.na( wvfrm ) )
        
              # Add to graph
        #      str = paste0( wvfrm, collapse=',' );
              utc = NPO:::uUTC_from_samp( (c_*skipSize)+s0, info$ToC, info$header); # re-instate downsampling factor
              utc = round( utc );
              utc_string <- toString(utc)
              # print( "utc computed" )
              if ( !( utc_string %in% names(eventsList) ) ) { # ASSUMPTION: values are increasing!
                # print( "valid" )
                # Store this node
                #event_ <- topigraph::anEvent( utc, wvfrm )
                wvfrm <- list(wvfrm)
                #attr(wvfrm,'utc') <- utc_string
                names(wvfrm) <- utc_string
                eventsList <<- append( eventsList, wvfrm )
              }
            },error=function(e) {
              print(e)
            })
          }
        } # found the local peak
      } # is a local peak
    } # next candidate
    it <<- iterators::iter( names(eventsList) )
    # print( "eventsList iter created")
    return(1)
  }
  
  # These two functions put an iterator on the eventsList.
  nextEl <- function() {
#    print( "nextEl" )
    if ( !hasNx() )
      stop('StopIteration', call.=FALSE)
    has_next <<- NA
    wvfrm <- list(eventsList[[cache]])
    names(wvfrm) <- cache
    wvfrm
  }

  hasNx <- function() {
#    print( "hasNx" )
    if (!is.na(has_next)) return(has_next)
    tryCatch({
      cache <<- nextElem(it)
      has_next <<- TRUE
      },
      error=function(e) {
        if (identical(conditionMessage(e), 'StopIteration')) {
          has_next <<- FALSE
          } else {
            stop(e)
          }
        })
    has_next    
  }
  
  clr <- function() {
    rm( eventsList )
  }

  #print( "Done")
  # I would like this to be an iterable, but I'm confused how to do that.
  obj <- list( nextElem=nextEl, hasNext=hasNx, findPeaksIn=findPeaksIn, clr=clr)
  class(obj) <- c( 'ihasNext', 'abstractiter', 'iter', 'peakFinder' )
  obj
}
