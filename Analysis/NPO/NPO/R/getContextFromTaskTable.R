getContextFromTaskTable <- function( conn, args ) {
#  create table tasks (username varchar(128), institution varchar(64), lab varchar(32), experiment varchar(32),
#                      subject varchar(32), path varchar(256), service varchar(128), data varchar(64), iterationtype varchar(32),
#                      parameters text,
#                      nodename varchar(128), label varchar(512) not null, done boolean,
#                      created timestamp default current_timestamp, modified timestamp default current_timestamp on update current_timestamp,
#                      primary key (label));  
#  
#   NPO(path='/Users/markrbower/Dropbox/Documents/Data',data='rodent-MSO',institution='Yale',labl='NSME',subject='Halo11',signaltype='AP',iterationtype='directory')  
#  
  library( stringr )
  library( crayon )

  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/parseArg.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/labelFromContext.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/notDefined.R')
  
  fixedFields <- c('username','nodename','path','data','institution','lab','experiment','subject','iterationType','label','service','dbName','password','')
  
  #  1.	Load arguments into a data structure called context.
  print( 'Getting context' )
  context <- list()
  context$username <- parseArg( args, 'username' )
  if ( nchar(context$username) == 0 ) {
    context$username <- unname( Sys.info()['user'] )
  }
  context$nodename <- unname( Sys.info()['nodename'] )
  context$path <- parseArg( args, 'path' )
  context$data <- parseArg( args, 'data' )
  context$institution <- parseArg( args, 'institution' )
  context$lab <- parseArg( args, 'lab' )
  context$experiment <- parseArg( args, 'experiment' )
  context$subject <- parseArg( args, 'subject' )
  context$iterationType <- parseArg( args, 'iterationType' )
  context$label <- parseArg( args, 'label' )
  context$service <- parseArg( args, 'service' )
  if ( nchar( context$service ) == 0 ) { # try 'dbName'
    context$service <- parseArg( args, 'dbName' )
  }
  # How to handle parameters?
  # Look at the names of the args and find outliers to fixed fields
  '%!in%' <- function(x,y)!('%in%'(x,y)) # https://stackoverflow.com/questions/5831794/opposite-of-in
  idx <- which( names(args) %!in% fixedFields )
  context$parameters <- paste( names(args[idx]),args[idx],sep="::",collapse=":::") #  f <- str_split( context$parameters, '::' )

  #  2.	If prior existence in database cannot be established, notify user the previous instance is being loaded, and fill in missing values.
  if ( notDefined( context$label ) ) { # the label was not supplied by the user
    if ( nchar(labelFromContext(context)) <= 7 ) { # the label cannot be constructed
      cat( bgGreen( white( "NOTICE: THE PREVIOUS RUN PARAMETERS ARE BEING USED.\n" ) ) )
      cat( bgGreen( white( "        IF YOU WANT TO UTILIZE A NEW SET OF PARAMETERS, PLEASE PROVIDE A NEW SET OF VALUES.\n") ) )
      query <- paste0( 'select * from tasks inner join (select label,max(modified) as most_recent from tasks group by label limit 1) ms on tasks.label=ms.label and modified=most_recent;')
      rs <- dbGetQuery( conn, query )
      if ( notDefined( context$username  ) ) { context$username <- rs$username }
      if ( notDefined( context$nodename  ) ) { context$nodename <- rs$nodename }
      if ( notDefined( context$path  ) ) { context$path <- rs$path }
      if ( notDefined( context$data  ) ) { context$data <- rs$data }
      if ( notDefined( context$institution  ) ) { context$institution <- rs$institution }
      if ( notDefined( context$lab  ) ) { context$lab <- rs$lab }
      if ( notDefined( context$experiment  ) ) { context$experiment <- rs$experiment }
      if ( notDefined( context$subject  ) ) { context$subject <- rs$subject }
      if ( notDefined( context$iterationType  ) ) { context$iterationType <- rs$iterationType }
      if ( notDefined( context$service  ) ) { context$service <- rs$service }
      if ( notDefined( context$parameters  ) ) { context$parameters <- rs$parameters }
    }
  }  
  context$label <- labelFromContext( context )
  
  # Unpack the parameters and make separate entries for each.
  parmString <- unlist( str_split( context$parameters, '::' ) )
  for ( ps in parmString ) {
    parts <- unlist(str_split( ps, '=' ) )
    str <- paste0( 'context[[\"', parts[1], '\"]] = \'', parts[2], '\'' )
    eval(parse(text=str))
  }
  
  #  3.	Determine whether this instance exists in the database.
  query <- paste0( 'select count(*) as count from tasks where label=\'', context$label, '\';' )
  rs <- dbGetQuery( conn, query )
  if ( rs$count == 0 ) { # This label does not exist in the database
    #  ⁃	If not, create the instance in the database
    query <- "insert into tasks (username,nodename,path,data,institution,lab,experiment,subject,iterationType,label,service,done) values "
    query <- paste0( query, "(\'", context$username, "\',\'", context$nodename, "\',\'", context$path, "\',\'", context$data, "\', \'", context$institution, "\',\'", context$lab, "\',\'", context$experiment, "\'," )
    query <- paste0( query, "\'", context$subject, "\',\'", context$iterationType, "\',\'", context$label, "\', " )
    query <- paste0( query, "\'", context$service, "\', 0 );" )
    rs <- dbGetQuery( conn, query )
  } else { # This label does exist in the database 
    #  ⁃	If so, update the database values. Perhaps updating the 'modified' value is the most important aspect of this step.
    query <- paste0( "update tasks set modified=now(), " )
    query <- paste0( query, "username=\'", context$username, "\', nodename=\'", context$nodename, "\', path=\'", context$path, "\', data=\'", context$data, "\', institution=\'", context$institution, "\', lab=\'", context$lab, "\', experiment=\'", context$experiment, "\'," )
    query <- paste0( query, "subject=\'", context$subject, "\',iterationType=\'",context$iterationType, "\',label=\'", context$label, "\', " )
    query <- paste0( query, "service=\'", context$service, "\',done=0 " )
    query <- paste0( query, "where label=\'", context$label, "\';" )
    print( query )
    rs <- dbGetQuery( conn, query )
  }
  
  #  4.	Return context
  return( context )
}
