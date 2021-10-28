NPO_use_buckets <- function( compArgs_base, algorithm_NPO ) {
  # The design priniciple is that this code should work for any analysis:
  # simply replace the algorithm given in line 36: "algo <- algorithm( algorithm_NPO, compArgs )"
  #
  # Example: script_runNPObuckets.R
  #
  #' @export
  library(foreach)
  library(future)
  setOptions()
  nbrWorkers <- parallel::detectCores() * 2
  print( paste0( "Number of workers: ", nbrWorkers ) )
  plan(multisession,workers=nbrWorkers) # "multisession" is portable, "multicore" is not
  topconnect::clearAllDBcons()
  compArgs_base <- checkRestartAndPassword( compArgs_base )
  bufferSizePower <- 22
  compArgs_base$findClass('analysisInformer')$add( list(bufferSize=2^bufferSizePower) )
  correlationWindow <- compArgs_base$get('correlationWindow')
  fileProvider <- compArgs_base$findClass( 'fileProvider' )
#  fileList <- fileProvider$listFiles()
#  foreach (filename = fileList) %dopar% {
  while ( fileProvider$hasNext() ) {
#  fileIter <- NPO:::DIRiter( compArgs_base$get('path'), compArgs_base$get('subject'), compArgs_base$get('centerTime') )
#  while (hasNext(fileIter)) {
    compArgs_file <- compArgs_base
#    filename <- file.path( compArgs_base$get('path'), nextElem(fileIter)$channel, fsep=.Platform$file.sep )
    filename <- fileProvider$nextElem()
    print( filename )
    compArgs_file <- topconnect::appendFileMetadata( compArgs_file, filename ) # 'info' should be added to 'compArgs' here
    cases <- topconnect::caseIter( compArgs_file )
#    foreach::foreach(case = cases) %dopar% { # have the ability to do files in parallel as well as run futures (below)
    while ( cases$hasNext() ) {
      case <- cases$nextElem()
      compArgs <- compArgs_file
      algo <- buckets::algorithm( algorithm_NPO, compArgs )
      algo$initialize( algo )
      compArgs$findClass('metadataInformer')$set( "case", case )
      if ( topconnect::currentProcessedLevel( compArgs, case, 0 ) ) {
        timeConstraints <- checkTimeConstraints( compArgs$get('info'), case )
        iter_conts <- meftools::MEFcont( filename, 'erlichda', compArgs$get('bufferSize'), window=timeConstraints, info=compArgs$get('info') )
        bucket_source <- buckets::bucket_source( iter_conts, algo, 5 )
        bucket_source$begin()
        print( "bucket_source$begin() done!")
        algo$flush()
        topconnect::markAsProcessed( compArgs, case, 1 )
      } # current_processed_level
    } # next case
  } # next file
}