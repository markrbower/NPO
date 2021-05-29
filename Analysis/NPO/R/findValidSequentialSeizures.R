findValidSequentialSeizures <- function( subjectID, minDuration ) {
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/db.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/durationRequirement.R')
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/validSequentialSeizures.R')
  
  clearAllDBcons()

  conn <- db()
  D <- durationRequirement( conn, 'epochs', subjectID, minDuration )
  data <- D$data
  validIdx <- D$validIdx
  query <- paste0( 'select distinct event_start from seizures where subject=', subjectID, ';' )
  rs <- dbGetQuery( conn, query )
  zTimes <- sort( rs$event_start )
  
  validSzIdx <- validSequentialSeizures( data, zTimes, validIdx, minDuration )
  return( validSzIdx )
}
