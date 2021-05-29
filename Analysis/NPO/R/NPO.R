# Network Properties Outlier
# October 1, 2019
# Mark R. Bower
# Yale University
#
# create table tasks (nodename varchar(128),path varchar(256),data varchar(64),institution varchar(64),lab varchar(32),experiment varchar(32),subject int(11),signaltype varchar(32),iterationtype varchar(32),label varchar(512) not null,centerTime bigint,done boolean,created timestamp default current_timestamp, modified timestamp default current_timestamp on update current_timestamp, primary key (label) );
#
NPO <- function(...) {
  #' Run the Network Parameter Outlier (NPO) algorithm.
  #' 
  #' @export
  #' @examples
  #' \dontrun{
  #'   First time the function is called:
  #'   NPO(dbName='NSME_halo',path='NPO/Analysis/NPO/tests/testData/Halo/11',data='rodentMSO',institution='Yale',lab='NSME',experiment='Halo10sec_10x',subject=11,signalType='AP',centerTime=0,iterationType='directory',range=c(-3000,2000), '--restart' )
  #'
  #'   Subsequent calls:
  #'   NPO()
  #' }
  
  print( "In NPO" )
  
  args <- list(...)
  
  setwd( here() )

  options(warn=-1)
  options(stringsAsFactors = FALSE);

  topconnect::clearAllDBcons()
  args <- list(...)

  dbName <- parseArg( args, 'dbName' )
  conn <- topconnect::db( db_user="root", project="NSME_halo" )
  
  # Check that inputs are valid: have associated seizures, behavior and data.
  query <-paste0( "select * from behavior where subject= and ")
  
  # Evaluate the "tasks" table.
  print( 'Getting the context' )
  context <- topconnect::getContextFromTaskTable( conn, args )
  
  # Load the seizure times
  seizureTimes <- getSeizureTimes( context )
  context$seizureTimes <- seizureTimes
  context$relativeWindowSizeInHours <- c( -24, 48 )
  
  print( 'Getting parameters' )
  parameters <- loadParameters( context )
  
  # Create database tables
  print( 'Creating tables' )
  table_names <- createTablesForNPO( conn, context )
  print( 'Tables created' )
  
  # I don't understand what the purpose of this function is. About the only useful thing is checking for a 'restart', which can be done on it's own.
  #  analysisPlan <- useArgsAndContextToPlan( conn, args, context )
  checkRestart( args, conn, table_names, context )
  checkMEFpassword( context ) # Make sure that the secret_vault has the correct password
  
  # Find the channel names and fill the progress table
  # Find peaks
  #    MEF_analysisLoop_batch( conn, 'erlichda', table_names, subject, seizureTime )
  # MEF_analysisLoop_directory( dbName, table_names, context )
  MEFthenAnalysisLoopOnDirectory( parameters, dbName, table_names, context )
  
  # Find communities
  # MySQL_analysisLoop_P2M_batch( conn, '', table_names, subject, seizureTime )
  MySQL_analysisLoop_P2M_batch( conn, parameters, dbName, table_names, context )
  
  # Find clusters
  # MySQL_analysisLoop_M2C_batch( conn, '', table_names, subject, seizureTime )
  

}
