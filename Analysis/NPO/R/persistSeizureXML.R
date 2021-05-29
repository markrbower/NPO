persistSeizureXML <- function( dbConn, dbTable, fields, xmlFile, subject, password ) {
  # Assumes that the data are written: start, stop, label
  #
  # Usage:
  # persistSeizureXML( db, 'epochs', list('subject','label','start','stop'), '/Users/markrbower/Documents/Data/NV/NVC1001_23_002_2', "12 23 13 25 13 14 12 15" )
  
  # The 'filename' is assumed to describe a representative file. It is 'representative" in that the actual filename does not
  # matter; only the times matter. If the discontinuities differ for each filenmae, then you will need a new database table.

  case <- data.frame( subject=subject, channel_name=paste0("NVC1001_",subject,"_01.mef") )
  #  basedir <- '/Volumes/Oyen_1TB/RawData/NV_Human/'
  basedir <- paste0( '/Users/markrbower/Documents/Data/NV/NVC1001_', case$subject, '_2' )
  mef_filename <- filenameFromCase( basedir, case )
  vault <- topsecret::get_secret_vault()
  password_key <- paste0( 'NSME_halo_password' )
  info <- meftools::mef_info( c(mef_filename,secret::get_secret(password_key,key=secret::local_key(),vault=vault)) )
  suid <- info$header$session_unique_ID

  dat <- read_xml( paste0("/Users/markrbower/Documents/Data/NV/NVC1001_",subject,"_2/annotations.xml") )
  nodes<-xml_find_all(dat, ".//annotation")
  nodes<-nodes[xml_attr(nodes, "description")=="Seizure_CCS"]
  starts <- xml_attr(nodes, "startOffsetUsecs")
  stops  <- xml_attr(nodes, "endOffsetUsecs")
  data <- data.frame( starts=starts, stops=stops )

  dib <- topconnect::databaseInsertBuffer( dbConn, dbTable, fields, 100 )  
  
  lapply( seq(from=1,to=nrow(data)), function(x) {dib$insert( c(subject=subject, session=suid, label="seizure", start=data$start[x], stop=data$stop[x] ) )} )
  dib$flush()
}
