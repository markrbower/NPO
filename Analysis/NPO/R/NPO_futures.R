NPO_futures <- function( argComp, algorithm ) {
  #
  # argComp example generation:
  #   argComp <- RFactories::argumentComposite()
  #   dp <- RFactories::databaseProvider(user="markrbower",vault_user='markrbower',vault_key='NV_password',host='localhost',dbname='NV')
  #   argComp$add( dp )
  #   pi <- RFactories::parameterInformer()
  #   pi$loadParameters( dp )  #  The parameterInformer requires a databaseProvidere to load parameters from the database.
  #   argComp$add( pi )
  #   argComp$add( RFactories::fileProvider(path='NPO/Analysis/NPO/tests/testData/Halo/11',iterationType='directory',pattern="*.mef") )
  #   argComp$add( RFactories::analysisInformer() )
  #
  # algorithm must have:
  #   See June 25,2021 MacJourrnal entry under "NPO/What else ..."
  #

  setOptions()
  cl<-parallel::makeCluster(8,outfile='',setup_strategy = 'sequential');
  doParallel::registerDoParallel(cl)
  topconnect::clearAllDBcons()
  argComp <- checkRestartPassword( argComp )
  cases <- topconnect::caseIter( argComp )
  fileIter <- NPO:::DIRiter( argComp$path, argComp$subject, argComp$centerTime )
  while (hasNext(fileIter)) {
    filename <- file.path( ac$get('path'), ac$get('channel'), fsep=.Platform$file.sep )
    argComp <- topconnect::appendFileMetadata( filename, argComp )
    info <- meftools::mef_info( c(filename, argComp$findClass("metadataInformer")$getFilePassword()) )
    foreach::foreach(case = cases) %dopar% { # have the ability to do files in parallel as well as run futures (below)d
      if ( topconnect::current_processed_level() ) {
        checkpointStatus()
        mefBuf <- meftools::mefBuffer( power=21 )
        # Processing the file starts here.
        iter_conts <- meftools::MEFcont( filename, 'erlichda', bufferSize, window=timeConstraints, info=info )
        while ( hasNext( iter_conts ) & segmentCount<maxSegmentCount ) { # across contiguous blocks
          iter_data <- nextElem( iter_conts )
          while ( hasNext( iter_data ) & segmentCount<maxSegmentCount ) {
            mefBuf$fill( nextElem( iter_data ) )
            segmentCount <- segmentCount + 1
            t0 <- as.numeric( attr( data, 't0' ) )
            t1 <- as.numeric( attr( data, 't1' ) )
            if ( t1 > (Tstored - 2*correlationWindow) ) { # more to do in this data block
              mefBuf$filter()
              pf <- NPO:::peakFinder( variables, correlationWindow, state )
              pf$findPeaksIn( mefBuf$get() )
              while ( pf$hasNext() ) {
                algoirthm( pf$nextElem() )
              }
              pf$clr()
              rm( pf )
            } else {
              print( paste0( 'Skipping ', Tstored ) )
            }
          } # next chunk of data
        } # next continuous series of chunks
        topconnect::markAsProcessed( dbName, progress_table, fdata$subject, fdata$channel, suid, case$centerTime, 1, hostname=hostname, password=password )
      } # current_processed_level
    } # next case
  } # next file
  
}
