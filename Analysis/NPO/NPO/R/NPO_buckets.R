NPO_buckets <- function( compArgs, algorithm_NPO ) {
  # The design priniciple is that this code should work for any analysis:
  # simply replace the algorithm given in line 36: "algo <- algorithm( algorithm_NPO, compArgs )"
  #
  # Example: script_runNPObuckets.R
  #
  setOptions()
#  cl<-parallel::makeCluster( 4, outfile='', setup_strategy = 'sequential');
#  doParallel::registerDoParallel(cl)
  topconnect::clearAllDBcons()
  compArgs <- checkRestartAndPassword( compArgs )
  bufferSizePower <- 21
  compArgs$findClass('analysisInformer')$add( list(bufferSize=2^bufferSizePower) )
  correlationWindow <- compArgs$get('correlationWindow')
  cases <- topconnect::caseIter( compArgs )
  fileIter <- NPO:::DIRiter( compArgs$get('path'), compArgs$get('subject'), compArgs$get('centerTime') )
  while (hasNext(fileIter)) {
    filename <- file.path( compArgs$get('path'), nextElem(fileIter)$channel, fsep=.Platform$file.sep )
    compArgs <- topconnect::appendFileMetadata( compArgs, filename ) # 'info' should be added to 'compArgs' here
    algo <- buckets::algorithm( algorithm_NPO, compArgs )
    #    foreach::foreach(case = cases) %dopar% { # have the ability to do files in parallel as well as run futures (below)
    while ( hasNext(cases) ) {
      case <- nextElem( cases )
      if ( topconnect::currentProcessedLevel( compArgs, case, 0 ) ) {
        timeConstraints <- checkTimeConstraints( compArgs$get('info'), case )
        iter_conts <- meftools::MEFcont( filename, 'erlichda', compArgs$get('bufferSize'), window=timeConstraints, info=compArgs$get('info') )
        counter <- 0
        while ( hasNext( iter_conts ) ) { # across contiguous blocks
          iter_data <- nextElem( iter_conts )
          while ( hasNext( iter_data ) ) {
            data <- nextElem( iter_data )
            counter <- counter + 1
            attr( data, 'counter' ) <- counter
            compArgs$findClass('metadataInformer')$set( "counter", counter)
            t0 <- as.numeric( attr( data, 't0' ) )
            t1 <- as.numeric( attr( data, 't1' ) )
            Tstored <- NPO:::findTheLatestTimestampProcessed( compArgs )
            if ( t1 > (Tstored - 2*correlationWindow) ) { # more to do in this data block
              algo$run( data )
            } else {
              print( paste0( 'Skipping ', Tstored ) )
            }
            rm( data )
          } # next chunk of data
        } # next continuous series of chunks
        topconnect::markAsProcessed( compArgs, case, 1 )
      } # current_processed_level
    } # next case
  } # next file
}