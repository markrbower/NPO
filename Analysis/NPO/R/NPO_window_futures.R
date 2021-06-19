NPO_window_futures <- function(...) {
  # June 6, 2021
  # Mark R. Bower
  # Yale University
  #
  #' Run the Network Parameter Outlier (NPO) algorithm using futures to promote parallel processing.
  #' Based on 'NPO_window.R" and "testSequentialParallelProcessing_rob.R"
  #' 
  #' @export
  #' @examples
  #' \dontrun{
  #'   First time the function is called:
  #'   NPO_window_futures(dbName='NSME_halo',path='NPO/Analysis/NPO/tests/testData/Halo/11',data='rodentMSO',institution='Yale',lab='NSME',experiment='Halo10sec_10x',subject=11,signalType='AP',centerTime=0,iterationType='directory',range=c(-3000,2000), '--restart' )
  #'
  #' ... or ...
  #' 
  #'   NPO_window_futures(dbName='NV', path='/Users/markrbower/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/mef2', data='NV_23_002', institution='Yale', lab='NSME', experiment='baseline', subject='23_002', signalType='IIS', centerTime=0, iterationType='directory', range=c(-3000,2000), '--restart' )
  #' 
  #'   NPO_window_futures(service='NV', path='/Users/markrbower/Documents/Data/NV/NVC1001_24_005_2', taskName='preprocessing', institution='Yale', lab='NSME', experiment='NeuroVista', subject='24_005', signalType='IIS', centerTime=0, iterationType='directory', range=c(-3000,2000), '--restart' )
  #' 
  #'   Subsequent calls will re-run the most previous command using:
  #'   NPO_window_futures()
  #' }
    
  args <- list(...)
  
  options(warn=-1)
  options(stringsAsFactors = FALSE);
  
  variables <- args2vars( args )
  
  # There are 5 steps to the NPO algorithm: find peaks, compute CC, build graphs, find local membership, find global membership
  
  
  
  
  
  
}


