persistBehavioralEvents <- function() {
  # create table epochs ( subject varchar(256), session varchar(256), start bigint(20) unsigned, stop bigint(20) unsigned, label varchar(256) );
  #
  # Inputs:
  #   files - list of filenames containing CSV data to be stored into the 'epochs' table.
  #
  # The CSV file must have the format: start_time, stop_time, label
  
  # Change to target "...hypnogram.txt" and "...seizures.csv" in /Volumes/Data/NV/NV_Human_Sleep
  
  codes <- c('?','NREM3','NREM2','NREM1','REM','WAKE') 
  
  patient_ids <-  list(
    '23_002'
#    '23_003',
#    '23_004',
#    '23_007',
#    '24_002',
#    '24_004',
#    '24_005'
#    '25_001',
#    '25_002',
#    '25_003',
#    '25_005'
  )
  
  conn <- topconnect::db("NV")
  fields <- c('subject','session','start','stop','label')
#  dib <- topconnect::databaseInsertBuffer( conn, table='epochs', fields, limit=100 )
  dib <- topconnect::databaseInsertBuffer( dbName='NV', dbTable='epochs', fields=fields, limit=100, updates=NULL, dbuser='root', host='localhost', password='' )

  basedir <- here( "Data" )
  base = paste0( basedir, "/classes_")

  patient_iter <- itertools::ihasNext( patient_ids )
  while( itertools::hasNext( patient_iter ) ) {
    id <- iterators::nextElem( patient_iter )
    print( paste0( id ) )
    
    # Get the SUID
#    mef_filename <- paste0("/Users/markrbower/Documents/Data/NV/NVC1001_",id,"_2/NVC1001_",id,"_01.mef")
    mef_filename <- paste0("/Volumes/Data/NV/NVC1001_",id,"_2/NVC1001_",id,"_01.mef")
    vault <- topsecret::get_secret_vault()
    password_key <- paste0( 'NSME_halo_password' )
    info <- meftools::mef_info( c(mef_filename,secret::get_secret(password_key,key=secret::local_key(),vault=vault)) )
    suid <- info$header$session_unique_ID

    basedir <- "/Volumes/Data/NV/NV_Human_Sleep/"
    # Get the sleep times (*.txt)
    fname <- paste0( 'NVC1001_',id,'_hypnogram.txt' )
    filename <- paste0( basedir, fname)
    data <- read.delim( file=filename, header=FALSE, sep="\t" )
    data$time <- as.numeric( data$V1 )
    data$code <- codes[ as.numeric( data$V2 ) ]
    
    for ( row in seq(1,nrow(data) ) ) {
      # store each line using a dbIterator
      dib$insert( list(subject=id, session=info$header$session_unique_ID, start=data$time[row], stop=data$time[row]+30E6, label=data$code[row]) )
    }
    
    
    # Get the seizure times (*.csv)
    fname <- paste0( 'NVC1001_',id,'_seizures.csv' )
    filename <- paste0( basedir, fname)
    data <- read.csv( file=filename, header=FALSE, stringsAsFactors = FALSE )
    data$V1 <- as.numeric( data$V1 )
    data$V2 <- as.numeric( data$V2 )
    
    for ( row in seq(1,nrow(data) ) ) {
      # store each line using a dbIterator
      dib$insert( list(subject=id, session=info$header$session_unique_ID, start=data$V1[row], stop=data$V2[row], label=data[row,3]) )
    }
  }
  dib$flush()
  toptools::clearAllDBcons()
}
