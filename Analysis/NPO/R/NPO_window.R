NPO_window <- function(...) {
  # Network Properties Outlier
  # v1. October 1, 2019
  # v1. June 10, 2021
  # Mark R. Bower
  # Yale University
  #
  #' Run the Network Parameter Outlier (NPO) algorithm.
  #' 
  #' @export
  #' @examples
  #' \dontrun{
  #'  dbName='NV', path='/Users/markrbower/Documents/Data/NV/NVC1001_24_005_2', taskName='preprocessing', institution='Yale', lab='NSME', experiment='NeuroVista', subject='24_005', signalType='IIS', centerTime=0, iterationType='directory', range=c(-3000,2000), hostname='localhost', db_user='root', password=''
  #' }

  args <- list(...)

  setwd( here() )

  options(warn=-1)
  options(stringsAsFactors = FALSE);

  topconnect::clearAllDBcons()
  
  # Structure the input arguments for the analysis
  arguments <- argumentComposite()
  
  arguments$add( fileProvider(path=args$path,k) )
  arguments$add( databaseProvider(user=args$user,vault_user=args$vault_user,vault_key=args$vault_key,host=args$host,dbname=args$dbname) )
  arguments$add( parameterInformer() )
  arguments$add( analysisInformer() )
  arguments$loadParameters()
  
  # Create database tables
  table_names <- NPO:::createTablesForNPO( conn, variables )

  NPO:::checkRestart( args, conn, table_names, variables )
  NPO:::checkMEFpassword( variables ) # Make sure that the secret_vault has the correct password
  DBI::dbDisconnect( conn )

  NPO:::MEFthenAnalysisLoopOnDirectory_v2( araguments )
  
}
