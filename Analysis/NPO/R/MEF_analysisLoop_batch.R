MEF_analysisLoop_batch <- function( conn, password, table_names, subject, seizureUsed ) {
  # Beginning of the NOGB algorithm.
  # Need to create all database tables for this seizure:
  # progress, P, M and C.
  
  library( RMySQL )
  library( doParallel )
  library( foreach )

  source('~/Dropbox/Documents/Concepts/2018_07_27_meftools/Analysis/meftools/R/SQLiter.R')

#  print('Starting')

  hourWindow <- 24
  progress_table <- table_names['progress']
  signal_table <- table_names['P']
  cluster_table <- table_names['C']
  correlationWindow <- 1*60*1E6

#  cl<-makeCluster(4,outfile="")
#  registerDoParallel(cl)
  
#  query <- 'select distinct subject,channel,event_start from seizures where subject=1255 and (channel=\'NVC1001_25_005_01\' or channel=\'NVC1001_25_005_02\') order by event_start,channel,subject;'
  query <- paste0( 'select distinct subject,channel,event_start as timestamp from seizures where subject=', subject,' and event_start=', seizureUsed, ' order by event_start,channel,subject;' )
  cases <- SQLiter( conn, query )
#  dbDisconnect( conn )

#  info <- list( filename='0_0_0') # prime this variable
  
#  foreach (case=cases ) %dopar% { # a case is a named list: subject, channel and event_start
  while ( hasNext(cases) ) { # a case is a named list: subject, channel and event_start
    case <- nextElem( cases )
    print( case )

    library( RMySQL )
    
    source('~/Dropbox/Documents/Concepts/2018_07_27_meftools/Analysis/meftools/R/mef_info.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/db.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/filenameFromCase.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/computeTimeConstraints.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/getNVsubjectFromFilename.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/currentProcessedLevel.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/identificationAlgorithm.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/identificationAlgorithm_simple.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/markProcessedLevel.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/home.R')

    filename <- filenameFromCase( home(), case )
    if ( file.exists(filename) ) {
      tryCatch(
        {
          conn_local <- db()
          # print( paste0( "Starting processing", filename ) )
          if ( currentProcessedLevel( conn_local, progress_table, case, case$timestamp, 1 )==0 ) {
            # print( "Loading mef info" )
            info <- mef_info( c(filename,password) )
            # print( "Done loading mef info" )
            timeConstraints <- computeTimeConstraints( case$timestamp, info, hourWindow )
            if ( timeConstraints['isValid'] ) {
              # print( "Starting identificationAlgo" )
              identificationAlgorithm( conn_local, filename, password, case$subject, case$channel, case$timestamp, signal_table, cluster_table, timeConstraints, info, correlationWindow )
    #          identificationAlgorithm_simple( conn_local, filename, password, case$subject, case$channel, case$timestamp, signal_table, cluster_table, timeConstraints, info, correlationWindow )
              markProcessedLevel( conn_local, progress_table, case, case$timestamp, 1 )
            }
          }
          dbDisconnect( conn_local )
          rm( info )
          gc()
        },error=function(e)
        {
          print( paste0(e) )
          dbDisconnect( conn_local )
          rm( info )
          gc()
        }
      )
    } else {
      print( paste0( "Filename not found: ", filename ) )
    }
    # print( paste0( "Done processing", filename ) )
  }

}
