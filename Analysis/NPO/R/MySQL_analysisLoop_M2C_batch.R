MySQL_analysisLoop_M2C_batch <- function( conn, password, table_names, subject, seizureTime ) {
  library( RMySQL )
  library( doParallel )
  library( foreach )

  source('~/Dropbox/Documents/Concepts/2018_07_27_meftools/Analysis/meftools/R/SQLiter.R')

#  callingFunctionName <- as.character( match.call()[[1]] )
#  print( callingFunctionName )

  hourWindow <- 24
  progress_table <- table_names['progress']
  peaks_table <- table_names['P']
  members_table <- table_names['M']
  cluster_table <- table_names['C']
  correlationWindow <- 5*60*1E6
  
#  cl<-makeCluster(9,outfile="")
#  registerDoParallel(cl)
  
#  db <- dbConnect( MySQL(), user='root', password=password, dbname='markdb_projectKoala', host='localhost' );
#  db <- dbConnect( MySQL(), user='db00087c', password='mrb1yale12', dbname='db00087c', host='spinup-mysql-01.c9ukc6s0rmbg.us-east-1.rds.amazonaws.com' );

  query <- paste0( 'select distinct subject,channel,seizureUsed from ', members_table, ' where subject=1255 order by seizureUsed,channel,subject;' )
  print( query )
  cases <- SQLiter( conn, query )
#  dbDisconnect( db )

  basedir <- '/Volumes/Oyen_1TB/RawData/NV_Human/'
#  basedir <- '/home/bm662/Data/NeuroVista/'
  
#  info <- list( filename='0_0_0') # prime this variable
  
#  foreach (case=cases ) %dopar% { # a case is a named list: subject, channel and event_start
  while ( hasNext(cases) ) { # a case is a named list: subject, channel and event_start
    case <- nextElem( cases )
    #print( case )

    library( RMySQL )
    
    source('~/Dropbox/Documents/Concepts/2018_07_27_meftools/Analysis/meftools/R/mef_info.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/filenameFromCase.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/computeTimeConstraints.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/getNVsubjectFromFilename.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/currentProcessedLevel.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/markProcessedLevel.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/M2C.R')
    
    tryCatch({
#      db <- dbConnect( MySQL(), user='root', password=password, dbname='markdb_projectKoala', host='localhost' );
      if ( currentProcessedLevel( conn, progress_table, case, 2 ) == 2 ) {
        case$latency <- correlationWindow
        M2C( conn, case, peaks_table, members_table, cluster_table, 2*correlationWindow )
        markProcessedLevel( conn, progress_table, case, 3 )
      }
#      dbDisconnect( db )
    },error=function(e) {
      print( paste0(e) )
      dbDisconnect( conn )
#        rm( info )
    }
    )
  }

}
