MEFthenAnalysisLoopOnDirectory_v2 <- function( variables, dbName, table_names ) {
  # Beginning of the NPO algorithm.
  #
  # Opens MEF files and maintains "mef_info" across multiple cases.
  # Sets parameters for calling "createGraph...".
  #
  # If the context argument includes "seizureTimes", then only compute results
  # for a window of time "computeWindowHours" around each seizure, not for the entire file.
  #
  library(doParallel)
  library(foreach)
  library( SingletonsInR )
  library( topconnect )
  
  source('NPO/Analysis/NPO/R/DIRiter.R')

  options(stringsAsFactors = FALSE);
  
#  print('Starting')
  dirPath <- variables$path
#  print( dirPath )
  subject <- variables$subject
  seizureUsed <- variables$centerTime

  hourWindow <- 24
  progress_table <- table_names['progress']
  signal_table <- table_names['P']
  #print( paste0( "MEFthen: signal_table: ", signal_table))
  cluster_table <- table_names['C']
  #print( variables)
  if ( 'correlationWindow' %in% names(variables) ) {
    correlationWindow <- variables$correlationWindow
  }
  if ( 'CCthreshold' %in% names(variables) ) {
    CCthreshold <- variables$CCthreshold
  }

  cl<-parallel::makeCluster(8,outfile="",setup_strategy = "sequential")
  doParallel::registerDoParallel(cl)

  # Iterate over each valid seizure. If none, analyze the whole file.
    hostname <- variables$hostname
    if ( length(hostname) == 0 ) {
      hostname <- 'localhost'
    }
    #print( hostname )
    password <- variables$password
    if ( length(password) == 0 ) {
      password <- ''
    }
    #print( password )
    db_user <- variables$db_user
    if ( length(db_user) == 0 ) {
      db_user <- 'root'
    }
    conn <- topconnect::db( db_user=db_user, project=dbName, host=hostname, password=password )
  query <- paste0("select * from tasks where subject=\'",variables$subject,"\' and taskName='validSeizure' order by centerTime;")
  taskRecordset <- DBI::dbGetQuery( conn, query )
  if ( nrow( taskRecordset) == 0 ) { # analyze the entire data file
    query <- paste0("select * from tasks where subject=\'",variables$subject,"\' order by centerTime;")
    taskRecordset <- DBI::dbGetQuery( conn, query )
  }
  DBI::dbDisconnect( conn )

  # Iterate over each data file
  #print( variables$path )
  fileIter <- NPO:::DIRiter( variables$path, variables$subject, variables$centerTime )
  while ( itertools::hasNext(fileIter) ) {
    fdata <- iterators::nextElem( fileIter )
    filename <- file.path( variables$path, fdata$channel, fsep=.Platform$file.sep )
    vault <- topsecret::get_secret_vault()
    password_key <- paste0( dbName, '_password' )
    info <- meftools::mef_info( c(filename,secret::get_secret(password_key,key=secret::local_key(),vault=vault)) )
    suid <- info$header$session_unique_ID
    #print( "Got info" )
    
    #print( nrow( taskRecordset) )
    cases <- topconnect::RSiter( taskRecordset )
    
    foreach::foreach(case=cases ) %dopar% { # a case is a named list: subject, channel and event_start
#    while ( itertools::hasNext(cases) ) { # a case is a named list: subject, channel and event_start
#      case <- iterators::nextElem( cases )
      library(topconnect)
      library(signal)
      library(itertools)

#      conn_local <- topconnect::db( dbName )
      #print( case$parameters )
      case <- NPO:::expandStringsToFields( case, "parameters", ":::", "::" )
      #print( "Checking out processed level")
      if ( topconnect::currentProcessedLevel( dbName, progress_table, fdata$subject, fdata$channel, suid, case$centerTime, hostname=hostname, password=password )==0 ) {
        #print( "Loading filter parameters" )
        Flow  <- variables$filter_detect_lowF  / ( info$header$sampling_frequency / 2 )
        Fhigh <- variables$filter_detect_highF / ( info$header$sampling_frequency / 2 )
        variables$parms_filter_detect <- butter( 3, c(Flow,Fhigh), 'pass' )
        Flow  <- variables$filter_keep_lowF  / ( info$header$sampling_frequency / 2 )
        Fhigh <- variables$filter_keep_highF / ( info$header$sampling_frequency / 2 )
        variables$parms_filter_keep <- butter( 3, c(Flow,Fhigh), 'pass' )
        #
        timeConstraints <- vector()
        #print( paste0( "case: ", case ) )
        #print( case$analysisStart )
        #print( case$analysisStop )
        if ( is.null(case$analysisStart) ) {
          print( "Failed")
          timeConstraints['start']     <- info$header$recording_start_time + 1.01E6
        } else {
          timeConstraints['start']     <- case$analysisStart
        }
        if ( is.null(case$analysisStop) ) {
          print( "Failed")
          timeConstraints['stop']      <- info$header$recording_end_time - 1.01E6
        } else {
          timeConstraints['stop']      <- case$analysisStop
        }
        timeConstraints['usWindow']  <- timeConstraints['stop'] - timeConstraints['start']
        timeConstraints['isValid']   <- 1
        
        #print( paste0( "start: ", timeConstraints['start'], "      stop: ", timeConstraints['stop'] ) )
              
        tryCatch({
          T <- system.time(
            #print( "createGraph" ),
            NPO:::createGraphAndBufferThenFilterAndSendToPeakFinder( dbName, variables, filename, case$subject, suid, fdata$channel, case$centerTime, signal_table, cluster_table, timeConstraints, info, correlationWindow, CCthreshold, testbedFlag=FALSE ) )

#              createGraphAndBufferThenFilterAndSendToPeakFinder( dbName, variables, filename, variables, case$subject, suid, fdata$channel, case$centerTime, signal_table, cluster_table, timeConstraints, info, correlationWindow, CCthreshold, testbedFlag )


        }, error=function(cond) {
              print( "MEFthen error condition")
              message( cond )
        })
        
        #print( paste0( "MEFthenAnalysis...", password ) )
        topconnect::markAsProcessed( dbName, progress_table, fdata$subject, fdata$channel, suid, case$centerTime, 1, hostname=hostname, password=password )
      }
      #DBI::dbDisconnect( conn_local )
    }
  }

}
