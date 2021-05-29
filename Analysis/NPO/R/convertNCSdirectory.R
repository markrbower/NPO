convertNCSdirectory <- function( rootDir ) {
  library( meftools )
  library( stringr )
  
  # Find all .ncs files with "rootDir" as a parent directory.
  filenames <- list.files(path=rootDir, pattern = "ncs$", recursive = TRUE)

  # Loop through them
  for ( ncs_filename in filenames ) {
    fullpath <- normalizePath( file.path( rootDir, ncs_filename, fsep = .Platform$file.sep) )
    mef_filename <- str_replace( fullpath, ".ncs$", ".mef")
    if ( file.exists(mef_filename) ) {
      file.remove( mef_filename )
    }
    meftools::ncs2mef( c(fullpath) )
  }
}
