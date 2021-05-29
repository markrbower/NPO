generateExampleSeizurePlots <- function( subjectID ) {
  case <- data.frame( subject=subjectID, channel_name=paste0("NVC1001_",subjectID,"_01.mef") )
  #  basedir <- '/Volumes/Oyen_1TB/RawData/NV_Human/'
  basedir <- paste0( '/Users/markrbower/Documents/Data/NV/NVC1001_', case$subject, '_2' )
  mef_filename <- filenameFromCase( basedir, case )
  vault <- topsecret::get_secret_vault()
  password_key <- paste0( 'NSME_halo_password' )
  info <- meftools::mef_info( c(mef_filename,secret::get_secret(password_key,key=secret::local_key(),vault=vault)) )

  # database  
  conn <- topconnect::db( "NSME_halo" )
  T0 <- info$header$recording_start_time

  # Find all seizures.
  query <- paste0("select start,stop from epochs where subject=\'",subjectID,"\' and label='seizure';")
  conn <- topconnect::db( "NSME_halo" )
  seizureTimes <- DBI::dbGetQuery( conn, query )
  
  # Load the filenmae "info"
  case <- data.frame( subject=subjectID, channel_name=paste0("NVC1001_",subjectID,"_01.mef") )
  #  basedir <- '/Volumes/Oyen_1TB/RawData/NV_Human/'
  basedir <- paste0( '/Users/markrbower/Documents/Data/NV/NVC1001_', case$subject, '_2' )
  mef_filename <- filenameFromCase( basedir, case )
  vault <- topsecret::get_secret_vault()
  password_key <- paste0( 'NSME_halo_password' )
  info <- meftools::mef_info( c(mef_filename,secret::get_secret(password_key,key=secret::local_key(),vault=vault)) )

  SEC <- 1E6
  for ( index in 1:nrow(seizureTimes) ) {
    print( index )
    seizure <- seizureTimes[index,] + T0
    # Load data from -60 sec to +120 sec
    win_start <- seizure$start - 60*SEC
    win_stop <- seizure$start + 120*SEC
    if ( meftools::dataAreContinuous( info, win_start, win_stop ) ) {
      data <- meftools::getMEFwindow( mef_filename, win_start, win_stop, info )
      p <- ggplot( data=data.frame(time=seq(1,length(data)),y=data), aes(x=time, y=y) ) + geom_line()
      for ( chNbr in seq(2,16) ) {
        #       
        case <- data.frame( subject=subjectID, channel_name=paste0("NVC1001_",subjectID,"_",formatC(chNbr,width=2,flag="0"),".mef") )
        #  basedir <- '/Volumes/Oyen_1TB/RawData/NV_Human/'
        basedir <- paste0( '/Users/markrbower/Documents/Data/NV/NVC1001_',subjectID, '_2' )
        mef_filename <- filenameFromCase( basedir, case )
        
        data <- meftools::getMEFwindow( mef_filename, win_start, win_stop, info )
        p <- p + geom_line( data=data.frame(time=seq(1,length(data)),y=data+(500*(chNbr-1))), aes(x=time, y=y) )
      }
      set_plot_dimensions(1,12)
      out_filename <- paste0( here("NPO/Presentation/Figures/seizurePlots/"), subjectID, "_", formatC(index,width=3,flag="0"), ".pdf" )
      pdf( out_filename )
      print( p )
      dev.off()
    }
  }  
  
  
  
}
