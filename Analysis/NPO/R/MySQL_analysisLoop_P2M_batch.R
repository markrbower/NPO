MySQL_analysisLoop_P2M_batch <- function( parameters, dbName, table_names, context ) {
#  library( RMySQL )
#  library( doParallel )
#  library( foreach )

  source('~/Dropbox/Documents/Concepts/2020_08_01_topconnect/topconnect/Analysis/topconnect/R/SQLiter.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Analysis/NPO/R/P2M.R')

  hostname <- context$hostname
  
#  print('Starting')
  dirPath <- context$path
  subject <- context$subject
  seizureUsed <- context$centerTime

  hourWindow <- 24
  progress_table <- table_names['progress']
  signal_table <- table_names['P']
  members_table <- table_names['M']
  correlationWindow <- parameters$correlationWindow

#  cl<-makeCluster(9,outfile="")
#  registerDoParallel(cl)
#  db <- dbConnect( MySQL(), user='root', password=password, dbname='markdb_projectKoala', host='localhost' );
#  db <- dbConnect( MySQL(), user='db00087c', password='mrb1yale12', dbname='db00087c', host='spinup-mysql-01.c9ukc6s0rmbg.us-east-1.rds.amazonaws.com' );

  if ( exists( 'cases.RData', ) ) {
    load( 'cases.RData' )
  } else {
    signal_table <- paste0( signal_table, '_safe' )
    query <- paste0( 'select distinct subject,channel,seizureUsed from ', signal_table, ' where subject=\'', subject,'\' order by seizureUsed,channel,subject;' )
    print( query )
    cases <- SQLiter( conn, query )
    save( cases, file='cases.RData' )
  }

  ####    XXXXXXXX  There should be a "path" variable containing this info XXXX
  basedir <- context$path
#  basedir <- '/Volumes/Oyen_1TB/RawData/NV_Human/'
#  basedir <- '/home/bm662/Data/NeuroVista/'
  
#  info <- list( filename='0_0_0') # prime this variable
  
#  foreach (case=cases ) %dopar% { # a case is a named list: subject, channel and event_start
  while ( hasNext(cases) ) { # a case is a named list: subject, channel and event_start
    case <- nextElem( cases )
    #print( case )

    library( RMySQL )

    tryCatch({
      conn_local <- topconnect::db( dbName );
      if ( currentProcessedLevel( conn_local, progress_table, case, context$centerTime ) == 1 ) {
        P2M( conn_local, case, parameters$signalType, signal_table, members_table, context )
        markProcessedLevel( conn_local, progress_table, case, 2 )
      }
      dbDisconnect( conn_local )
    },error=function(e) {
      print( paste0(e) )
      dbDisconnect( conn_local )
#        rm( info )
    }
    )
  }

}
