createGraphAndBufferThenFilterAndSendToPeakFinder <- function( dbName, variables, filename, subject, session, channel, seizureUsed, signal_table, cluster_table, timeConstraints, info, correlationWindow, CCthreshold=NULL, testbedFlag=FALSE ) {
  
  #print( "In createGraph...")
  # Computes detections for a window of size 'timeConstraints' around a single seizure
  library( topconnect )
  library( topigraph )
  library( meftools )
  library( DBI )

  options(stringsAsFactors = FALSE);
  
#  CCthreshold <- 0.5 # IIS
#  CCthreshold <- 0.95
#  EDthreshold <- 1.5
  if ( testbedFlag==TRUE ) {
    maxSegmentCount <- 10
  } else {
    maxSegmentCount <- 9E9
  }
  
  if ( is.null(CCthreshold) ) {
    CCthreshold <- variables$CCthreshold
  }
  EDthreshold <- variables$EDthreshold
  
  # parameters  
  latency <- correlationWindow
  blackout <- variables$blackout
  
  if ( 'hostname' %in% names(variables) ) {
    hostname <- variables$hostname
  } else {
    hostname <- 'localhost'
  }
  if ( 'db_user' %in% names(variables) ) {
    db_user <- variables$db_user
  } else {
    db_user <- 'root'
  }
  if ( 'password' %in% names(variables) ) {
    password <- variables$password
  } else {
    password <- ''
  }
  if ( 'dbName' %in% names(variables) ) {
    dbName <- variables$dbName
  } else {
    dbName <- 'mysql'
  }
  #print(hostname)
  #print(db_user)
  #print(password)
  #print(dbName)

  # Set up the graph for this window.
  masterID <- 0
  
  # Initialize the update string ...
  vineCount <- 1
  updateLimit <- variables$database_update_limit  # Check each update to make sure you aren't duplicating entries.
#  updateLimit <- 2
#  updateLimit <- 10
  fields <- c("subject","UUID","channel","seizureUsed","time","waveform","clusterid","peak", "energy", "incident", "weights" )
  #print( "DIB")
  #print( dbName )
  dib <- topconnect::databaseInsertBuffer( dbName, signal_table, fields, updateLimit, 'clusterid', host=hostname, password=password )
  options( scipen=999 )
  graph_filename <- paste0( subject, '_', tools::file_path_sans_ext(channel), '_', seizureUsed, '_graph.xml' )
  
  state <- list( dbName=dbName, latency=latency, variablesNotSet=0, CCthreshold=CCthreshold, EDthreshold=EDthreshold, masterID=masterID, subject=subject, session=variables$UUID, channel=tools::file_path_sans_ext(channel), seizureUsed=seizureUsed, signal_table=signal_table, cluster_table=cluster_table, findCliques=0, blackout=blackout  )

  #print( "GIB")
    gib <- topigraph::graphInsertBuffer( variables, correlationWindow, CCthreshold, EDthreshold, graph_filename, blackout, dib, state, dbName, P_table_=signal_table, host=hostname, db_user_=db_user, password=password )

  #print( "DIB and GIB created" )
  # Create the buffer
  # Buffer, filter and downsample
  # - create buffer
  bufferSizePower <- 21
  bufferSize <- 2^bufferSizePower
  buffer <- vector( mode='double', length=bufferSize )
  
  # Processing the file starts here.
  #print( paste0( "timeConstraints: ", timeConstraints['start'], " ", timeConstraints['stop'] ) )
  iter_conts <- meftools::MEFcont( filename, 'erlichda', bufferSize, window=timeConstraints, info=info )
  #print('MEF iter created')

  # Are results from this file already computed? If so, what is the last time stored (Tstored)?
  # Skip data up to Tstored-CW.

  #print( "db")
  #print(hostname)
  #print(db_user)
  #print(password)
  #print(dbName)
  
  conn <- topconnect::db( user=db_user, dbname=dbName, host=hostname, password=password )
  #print( "Got connection")
  query <- paste0('select max(time) as T from ', signal_table, ' where ')
  query <- paste0( query, 'subject=\'', subject, '\' and ')
  query <- paste0( query, 'channel=\'', tools::file_path_sans_ext(channel), '\' and ')
  query <- paste0( query, 'seizureUsed=', seizureUsed, ';')
  #print( query )
  tryCatch({
    rs <- DBI::dbGetQuery( conn, query )
  }, error=function(cond) {
    print( "Error in dbGetQuery" )
    print(cond)
  })
  #print( "Recordset obtained")
  if ( nrow(rs) == 0 ) {
    #print( "nrow = 0")
    Tstored <- -1
  } else if ( nrow(rs) == 1 ) {
    #print( "nrow = 1")
    if ( is.na(rs$T) ) {
      #print( "is na")
      Tstored <- -1
    } else {
      #print( "is number")
      Tstored <- rs$T
    }
  } else {
    print( 'ERROR! Too many rows found in createGraphAndBuffer...')
  }
  #print( "Out")
  DBI::dbDisconnect( conn )

  #print( "Begin processing")
  segmentCount <- 0
  #print( maxSegmentCount )
  while ( hasNext( iter_conts ) & segmentCount<maxSegmentCount ) { # across contiguous blocks
    #print( "Next data")
    iter_data <- nextElem( iter_conts )
    #print( "iter_data")
    while ( hasNext( iter_data ) & segmentCount<maxSegmentCount ) {
      data <- nextElem( iter_data )
      #print( "data")
      segmentCount <- segmentCount + 1
      t0 <- as.numeric( attr( data, 't0' ) )
      t1 <- as.numeric( attr( data, 't1' ) )
      #print( paste0( "Actual data times: ", t0, " to ", t1 ) )
      #print( "times")
      #      if ( profileCounter < 5 ) {
      #print( "Checking")
        if ( t1 > (Tstored - 2*correlationWindow) ) { # more to do in this data block
#          profileCounter <- profileCounter + 1
          #print( "filter data")
          filteredData <- NPO:::filterForSignalType( variables, buffer, data )
          #print( "find peaks")
          pf <- NPO:::peakFinder( variables, correlationWindow, state )
          #print( "PF created")
          pf$findPeaksIn( filteredData )
          #print( "found")
          peak_count <- 0
          while ( pf$hasNext() ) {
            #print( "insert")
            peak_count <- peak_count + 1
            #print( peak_count )
            gib$insert( pf$nextElem() )
          }
          #print( "done")
          rm( filteredData )
          pf$clr()
          rm( pf )
          #print( "Done with if")
        } else {
          #print( paste0( 'Skipping ', Tstored ) )
        }
      
      rm( data )
    }
  } # end block


#  gib$persistGraph( state )
  tryCatch(
    {
      #print( "flush")
      dib$flush()
    },
    error=function(cond) {
      print( "Error on dib flush.")
      print( cond )
    }    
  )
}
