tabulateBehavioralResult <- function( result ) {
  # INPUT:
  # - result:   Data.Frame; e.g., result <- findValidEpochs( 'NV', '24_005', 2 )
  
  behavior <- result[['behavior']]
  recovery <- result[['recovery']]
  rem <- result[['rem']]
  
  base_pre <- which(is.na(behavior$base_stop)==TRUE)
  Er <- which(is.na(rem$Er_start)==TRUE | is.na(rem$Er_stop)==TRUE)
  Or <- which(is.na(rem$Or_start)==TRUE | is.na(rem$Or_stop)==TRUE)
  r <- union( Er, Or )
  both <- intersect( base_pre, r)
  behavior_valid <- which( complete.cases(behavior) )
  rem_valid <- which( complete.cases(rem) )
  cat( paste( "behavior_valid", length(behavior_valid), "rem_valid", length(rem_valid), "both", length(both), "base", length(base_pre)-length(both), "REM", length(r)-length(both), "\n", sep=":\t" ) )
  print( length( which(diff(behavior_valid) == 1) ) )
  
  V <- behavior_valid
  validSeizureInfo <- data.frame(seizureStart=behavior$seizure_start[V],analysisStart=behavior$Ei_start[V],analysisStop=recovery$Oi_stop[V])
  
  return( validSeizureInfo )
}
