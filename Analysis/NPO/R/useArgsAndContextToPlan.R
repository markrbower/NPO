useArgsAndContextToPlan <- function( conn, args, context ) {
#  ⁃	restart only if the ‘—restart’ flag is supplied
#  ⁃	if you run with no inputs, continue (don’t restart) the most recent task based on modified.
#  ⁃	This requires the key-pair (username and service) be included in the Tasks table
#  ⁃	if you run with a known label, continue that task
#  ⁃	if you run with an unknown key-pair, ask for the password and assume a completely new run
#  ⁃	if you run with just a known key-pair, assume this is a completely new run
#
# That means that 'analysisPlan' can take on the following values: 
#  - continueTheMostRecent: Load the most recent task in the "tasks" table and return info.
#  - continueGivenTask: Load the task information for the given task label and return info.
#  - createNewTaskAndRun: Prompt the user for required task information, create entry and return needed info.
#  - runKnownTaskInItsCurrentState: 
# The plans are enacted by changes to database tables.
# Should it also include some variables returned to the program?
#
# The function returns those variables needed to run a task: username_experiment_subject_path_service_parameters
# These values are the same as the consituents of a label: username_experiment_subject_path_service_parameters
  
  library( secret )
  
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/knownArg.R')
  
  # If restart, update progress and P tables.
  checkRestart( args, conn, table_names, context )
  
  # get system information
  si <- Sys.info()
  
  # Check for username, service, label.
  argNames <- names(args)
  if ( 'username' %in% argNames & 'service' %in% argNames ) {
    username <- args[['username']]
    service <- aergs[['service']]
    query <- paste0( 'select count(*) as count from tasks where username=\'', username, '\' and service=\'', service, '\';' )
    rs <- dbGetQuery( conn, query )
    if ( rs$count != 1 ) {
      username <- ''
      service <- ''
    }
  }
  if ( 'label' %in% argNames ) {
    label <- args[['label']]
    query <- paste0( 'select count(*) as count from tasks where username=\'', label, '\';' )
    rs <- dbGetQuery( conn, query )
    if ( rs$count != 1 ) {
      label <- ''
    }
  }
  
  if ( length(args) == 0 ) {
    # continue the most recent
    query <- paste0( 'SELECT * FROM tasks ORDER BY modified DESC LIMIT  1;')
    rs <- dbGetQuery( conn, query )
    new_context <- contextFromTaskTableRecordSet( rs )
    label <- labelFromContext( new_context )
  } else if ( nchar(label) > 0 ) {
    # continue that task
    query <- paste0( 'SELECT * FROM tasks where label=\'', label, '\';')
    rs <- dbGetQuery( conn, query )
    new_context <- contextFromTaskTableRecordSet( rs )
    label <- labelFromContext( new_context )
  } else if ( nchar(username) > 0 & nchar(service) > 0 ) {
    # The project is the service. Get the label from the tasks table.
    

    
  } else {
    # get a password
    
    # parse args
    
  }

  return( label ) # 'label' is the analysis plan
}
