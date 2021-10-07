expandStringsToFields <- function( case, fieldName, sep1=",", sep2=":" ) {
  str <- case[[fieldName]]
  parts <- stringr::str_split( str, sep1 )
  for ( part in parts[[1]] ) {
    p <- stringr::str_split( part, sep2 )
    fieldname <- p[[1]][1]
    fieldvalue <- p[[1]][2]
    if ( !is.na(as.numeric(fieldvalue)) ) {
      fieldvalue <- as.numeric(fieldvalue)
    }
    case[[fieldname]] <- fieldvalue
  }

  return( case )
}

