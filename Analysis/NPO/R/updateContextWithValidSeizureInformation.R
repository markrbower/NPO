updateContextWithValidSeizureInformation <- function( conn, context ) {
  # Adds three fields to the "context" variable:
  # Check the database to see if the information contained in the context has seizure info.
  # If subject=<current_subject> and taskName='validSeizure' is not empty,
  # then valid seizure information has been entered.
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Analysis/NPO/R/persistBehavioralEvents.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Analysis/NPO/R/persistDiscontinuitiesIntoEpochs.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Analysis/NPO/R/findValidEpochs.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Analysis/NPO/R/findValidSeizures.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Analysis/NPO/R/tabulateBehavioralResult.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Analysis/NPO/R/persistValidSeizuresAsTasks.R')
  
  # Modify the epochs table
  query <- paste0( 'select count(*) as count from epochs where subject=\'',context$subject,'\' and label=\'WAKE\';' )
  rs <- DBI::dbGetQuery( conn, query )
  if ( rs$count == 0 ) { # compute and persist information about valid seizures
    persistBehavioralEvents()                       # populates the 'epochs' table - currently too focused
    persistDiscontinuitiesIntoEpochs( conn=conn, subject=context$subject, basedir=context$path ) # same as above
    result <- findValidEpochs( 'NV', '24_005', 5 )  # creates a dataframe within start/stop times
    findValidSeizures(result)                       # has options to include Recovery and/or REM
    save( result, file="result.RData")
  }
  # Modify the tasks table
  query <- paste0( 'select count(*) as count from tasks where subject=\'',context$subject,'\' and taskName=\'validSeizure\';' )
  rs <- DBI::dbGetQuery( conn, query )
  if ( rs$count == 0 ) { # compute and persist information about valid seizures
    if ( !exists( 'result' ) ) {
      load( file='result.RData' )
      zInfo <- tabulateBehavioralResult(result)       # several print statements about counts and time
      persistValidSeizuresAsTasks( conn, context, zInfo )            # need a function that persists
    }
  }
  
  # Load persisted info
  query <- paste0('select centerTime,parameters from tasks where subject=\'',context$subject,'\' and taskName=\'validSeizure\';' )
  rs <- DBI::dbGetQuery( conn, query )
  seizureTimes <- rs$centerTime
  context$seizureTimes <- seizureTimes

  parametersList <- rs$parameters
  parameters <- stringr::str_split( parametersList, ":::" )
  for ( parameter in parameters ) {
    parts <- stringr::str_split( parameter, "::" )
    context$relativeWindowSizeInHours <- rbind( context$relativeWindowSizeInHours, c( as.numeric(parts[[1]][2]), as.numeric(parts[[2]][2]) ) )
  }

  return( context )
}

