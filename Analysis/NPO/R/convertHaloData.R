convertHaloData <- function() {
  #' @export
  library( meftools )
  library( here )
  
#  ncsFiles <- list.files(here("NPO/Data/Halo_data_from_Roni"),pattern=".ncs",full.names=TRUE)
  for ( file in ncsFiles ) {
    meftools::ncs2mef( file )
  }  
}

