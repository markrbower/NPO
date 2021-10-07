MEFthenAnalysisLoopOnDirectory_testbed <- function( variables, dbName, table_names, testbedFlag=TRUE ) {
  # Beginning of the NPO algorithm.
  # Need to create all database tables for this seizure:
  # progress, P, M and C.
  #
  # If the context argument includes "seizureTimes", then only compute results
  # for a window of time "computeWindowHours" around each seizure, not for the entire file.
  library(doParallel)
  library(foreach)
  library( topconnect )
  
  options(stringsAsFactors = FALSE);
  
  #print('Starting')
  dirPath <- variables$path
  #print( dirPath )
  subject <- variables$subject
  seizureUsed <- variables$centerTime

  hourWindow <- 24
  progress_table <- table_names['progress']
  signal_table <- table_names['P']
  cluster_table <- table_names['C']
#  correlationWindow <- 5*1E6
  
  #print( paste0( "MEFthen..."))
  #print( "variables")
  #print( variables)

  parameter_fields <- str_split( variables$parameters, ':::' )
  values_list <- unlist( lapply( parameter_fields, function(x) str_split(x,'::')) )
  cnt <- 1
  L <- length(values_list)
  while ( cnt < L ) {
    if ( values_list[cnt] == 'correlationWindow' ) {
      correlationWindow <- as.numeric(values_list[cnt+1])
    }
    if ( values_list[cnt] == 'CCthreshold' ) {
      CCthreshold <- as.numeric(values_list[cnt+1])
    }
    cnt <- cnt + 2
  }
  
#  print( correlationWindow )


#  cl<-parallel::makeCluster(8,outfile="",setup_strategy = "sequential")
#  doParallel::registerDoParallel(cl)

  # Iterate over each valid seizure. If none, analyze the whole file.
  db_user <- variables$db_user
  hostname <- variables$hostname
  dbName <- variables$dbName
  password <- variables$password
  #print( db_user )
  #print( hostname )
  #print( dbName )
  #print( password )

  conn <- DBI::dbConnect( RMySQL::MySQL(), user=db_user, password=password, host=hostname, dbname=dbName )
  #print( "Connected" )
  query <- paste0("select * from tasks where subject=\'",variables$subject,"\' and taskName='validSeizure' order by centerTime DESC limit 20;")
  #print( "Querying" )
  taskRecordset <- DBI::dbGetQuery( conn, query )
  #print( "Queried" )
  if ( nrow( taskRecordset) == 0 ) { # analyze the entire data file
    #print( "entire")
    query <- paste0("select * from tasks where subject=\'",variables$subject,"\' order by centerTime DESC limit 20;")
    taskRecordset <- DBI::dbGetQuery( conn, query )
  }
  DBI::dbDisconnect( conn )
  #print( "Disconnected" )

  # Iterate over each data file
  fileIter <- NPO:::DIRiter( variables$path, variables$subject, variables$centerTime )
  while ( itertools::hasNext(fileIter) ) {
    #print( "Iterating" )
    fdata <- iterators::nextElem( fileIter )
    filename <- file.path( variables$path, fdata$channel, fsep=.Platform$file.sep )
    MEFpassword <- ''
    info <- meftools::mef_info( c(filename,MEFpassword) )
    suid <- info$header$session_unique_ID

    #print( "RSiter" )
    cases <- topconnect::RSiter( taskRecordset )
    
    foreach::foreach(case=cases ) %dopar% { # a case is a named list: subject, channel and event_start
#    while ( itertools::hasNext(cases) ) { # a case is a named list: subject, channel and event_start
#      case <- iterators::nextElem( cases )
      library(topconnect)
      library(signal)
      library(itertools)

#      conn_local <- topconnect::db( dbName )
      case <- NPO:::expandStringsToFields( case, "parameters", ":::", "::" )
      print( case )
      
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
        if ( is.null(case$anaylsisStart) ) {
          timeConstraints['start']     <- info$header$recording_start_time + 1.01E6
        } else {
          timeConstraints['start']     <- case$analysisStart
        }
        if ( is.null(case$analysisStop) ) {
          timeConstraints['stop']      <- info$header$recording_end_time - 1.01E6
        } else {
          timeConstraints['stop']      <- case$analysisStop
        }
        timeConstraints['usWindow']  <- timeConstraints['stop'] - timeConstraints['start']
        timeConstraints['isValid']   <- 1
              
        tryCatch({
          #print( dbName )
          #print( signal_table )
          #print( cluster_table )
          #print( correlationWindow )
          #print( CCthreshold )
          T <- system.time(
            NPO:::createGraphAndBufferThenFilterAndSendToPeakFinder( dbName, variables, filename, case$subject, suid, fdata$channel, case$centerTime, signal_table, cluster_table, timeConstraints, info, correlationWindow, CCthreshold, testbedFlag ) )

#              createGraphAndBufferThenFilterAndSendToPeakFinder( dbName, parameters, filename, context, case$subject, suid, fdata$channel, case$centerTime, signal_table, cluster_table, timeConstraints, info, correlationWindow, CCthreshold, testbedFlag )

          if ( testbedFlag == TRUE ) {
            # Store the parms and times to the database.
            conn <- DBI::dbConnect( RMySQL::MySQL(), user=db_user, password=password, host=hostname, dbname=dbName)
            query <- paste0( "insert into testbed_halo (subject,channel,cw,cc_threshold,user,sys,elapsed) values " )
            query <- paste0( query, "(\'", fdata$subject, "\',\'", fdata$channel, "\'," )
            query <- paste0( query,correlationWindow,",",CCthreshold,",",T[1],",",T[2],",",T[3],");")
            DBI::dbGetQuery( conn, query )
            DBI::dbDisconnect( conn )
          }
              
        }, error=function(cond) {
              message( cond )
        })
              
        topconnect::markAsProcessed( dbName, progress_table, fdata$subject, fdata$channel, suid, case$centerTime, 1, hostname=hostname, password=password )
      }
      #DBI::dbDisconnect( conn_local )
    }
  }

}
