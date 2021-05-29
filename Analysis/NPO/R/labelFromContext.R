labelFromContext <- function( context ) {
  # username:::experiment:::subject:::path:::service_parameters
  label <- paste0( context$username, ":::", context$experiment, ":::", context$subject, ":::", context$path,":::", context$service, ":::", context$parameters )
  return( label )
}
