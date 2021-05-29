tableNamingLogic <- function( variables ) {
  # Remove 'range'; it contains too many characters that cannot go into a MySQL table and it is more about displays, anyway.
  # This chunk is from 'loadParameters.R', if you want to make a function of it.
  options( scipen = 999 )
  
  #print( paste0( "tableNaming: variables: correlationWindow: ", variables$correlationWindow ) )
  #print( paste0( "tableNaming: variables: CCthreshold: ", variables$CCthreshold ) )
  prefix <- paste0( variables$experiment, '_', variables$subject, '_', variables$signalType, '_',
                    variables$centerTime, '_', variables$correlationWindow, '_', round(100*as.numeric(variables$CCthreshold)) )
  prefix <- str_replace_all( prefix, '::', '_' )
  prefix <- str_replace_all( prefix, '=', '_' )
  prefix <- str_replace_all( prefix, '-', 'MINUS' )
  #print( prefix )
}
