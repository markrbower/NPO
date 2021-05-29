home <- function( project_name=NULL ) {
  # Returns the path to the home directory for a given project.
#  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/NPO/R/loadContext.R')
  
  context <- loadContext( project_name )
  
  return( context['home'] )
}
