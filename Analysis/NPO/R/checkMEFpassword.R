checkMEFpassword <- function( context ) {
  library( secret )
  # Check that the current project has a valid MEF password
  password_key <- paste0( context$service, '_password' )
  vault <- topsecret::get_secret_vault()
  if ( !( password_key %in% secret::list_secrets(vault=vault)$secret)) {
    add_secret(name=password_key,value=readline("Enter MEF file password: "),users=Sys.info()['user'],vault=vault)
  }
}
