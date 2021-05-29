NPO_window_grid_script <- function( parms ) {
  duration = parms[1]
  threshold = parms[2]
  
  NPO_window(dbName='Halo', path='/Users/markrbower/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Data/Halo_data_from_Roni', taskName='preprocessing', institution='Yale', lab='NSME', experiment='Halo_test', subject='11', signalType='AP', centerTime=0, iterationType='directory', range=c(-1000,1000), duration=duration, threshold=threshold )

  
    
}


