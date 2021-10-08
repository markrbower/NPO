loadParameters <- function( context ) {
  # variables
  #   minimum_sampling_frequency
  #   correlationWindow
  #   blackout
  #   CCthreshold
  #   EDthreshold
  #   waveform_mask
  #   computation_mask
  #   database_update_limit
  #   filter_detect_lowF
  #   filter_detect_highF
  #   filter_keep_lowF
  #   filter_keep_highF
  #
  # var file
  library( signal )
  library( stringr )
  
  # parms <- c( AP_minimum_sampling_frequency=20000,AP_correlationWindow=1*1E6,AP_blackout=10E3,AP_CCthreshold=0.5,AP_EDthreshold=1.5,list(AP_waveform_mask=seq(-6,25)),AP_computation_mask=list(seq(3,15)),AP_database_update_limit=100,AP_filter_detect_lowF=600,AP_filter_detect_highF=6000,AP_filter_keep_lowF=600,AP_filter_keep_highF=6000,AP_signalType="AP",
  #             IIS_minimum_sampling_frequency=300,IIS_correlationWindow=60*1E6,IIS_blackout=100E3,IIS_CCthreshold=0.5,IIS_EDthreshold=1.5,IIS_waveform_mask=list(seq(-20,20)),IIS_computation_mask=list(seq(11,30)),IIS_database_update_limit=100,IIS_filter_detect_lowF=10,IIS_filter_detect_highF=40,IIS_filter_keep_lowF=20,IIS_filter_keep_highF=16000,IIS_signalType="IIS",
  #             NV_minimum_sampling_frequency=300,NV_correlationWindow=60*1E6,NV_blackout=100E3,NV_CCthreshold=0.5,NV_EDthreshold=1.5,NV_waveform_mask=list(seq(-20,20)),NV_computation_mask=list(seq(11,30)),NV_database_update_limit=100,NV_filter_detect_lowF=10,NV_filter_detect_highF=40,NV_filter_keep_lowF=1,NV_filter_keep_highF=100,NV_signalType="IIS"
  # )
#  # varfile <- system.file("extdata","parms.RData",package="NPO")
  # varfile <- "parms.RData"
  # save( parms, file=varfile )
  #
  #
##  varfile <- system.file("extdata/parms.RData",package="NPO")
  varfile <- "parms.RData"
  if (!file.exists(varfile)) {
    print( 'Please create a parameters file (parms.RData) as described in the loadParameters.R file' )
    return()
  } else {
    load(file=varfile)
  }

  # select which set of parms to use
  if ( context$signalType=="AP" ) { # AP
    parms <- c(minimum_sampling_frequency=unname(parms['AP_minimum_sampling_frequency']),correlationWindow=unname(parms['AP_correlationWindow']),blackout=unname(parms['AP_blackout']),CCthreshold=unname(parms['AP_CCthreshold']),EDthreshold=unname(parms['AP_EDthreshold']),waveform_mask=unname(parms['AP_waveform_mask']),computation_mask=unname(parms['AP_computation_mask']),database_update_limit=unname(parms['AP_database_update_limit']),filter_detect_lowF=unname(parms['AP_filter_detect_lowF']),filter_detect_highF=unname(parms['AP_filter_detect_highF']),filter_keep_lowF=unname(parms['AP_filter_keep_lowF']),filter_keep_highF=unname(parms['AP_filter_keep_highF']),signalType=unname(parms['AP_signalType']))
  } else if ( context$experiment!="NeuroVista" & context$signalType=="IIS" ) { # home
    parms <- c(minimum_sampling_frequency=unname(parms['IIS_minimum_sampling_frequency']),correlationWindow=unname(parms['IIS_correlationWindow']),blackout=unname(parms['IIS_blackout']),CCthreshold=unname(parms['IIS_CCthreshold']),EDthreshold=unname(parms['IIS_EDthreshold']),waveform_mask=unname(parms['IIS_waveform_mask']),computation_mask=unname(parms['IIS_computation_mask']),database_update_limit=unname(parms['IIS_database_update_limit']),filter_detect_lowF=unname(parms['IIS_filter_detect_lowF']),filter_detect_highF=unname(parms['IIS_filter_detect_highF']),filter_keep_lowF=unname(parms['IIS_filter_keep_lowF']),filter_keep_highF=unname(parms['IIS_filter_keep_highF']),signalType=unname(parms['IIS_signalType']))
  } else if ( context$experiment=="NeuroVista" & context$signalType=="IIS" ) { # home
    parms <- c(minimum_sampling_frequency=unname(parms['NV_minimum_sampling_frequency']),correlationWindow=unname(parms['NV_correlationWindow']),blackout=unname(parms['NV_blackout']),CCthreshold=unname(parms['NV_CCthreshold']),EDthreshold=unname(parms['NV_EDthreshold']),waveform_mask=unname(parms['NV_waveform_mask']),computation_mask=unname(parms['NV_computation_mask']),database_update_limit=unname(parms['NV_database_update_limit']),filter_detect_lowF=unname(parms['NV_filter_detect_lowF']),filter_detect_highF=unname(parms['NV_filter_detect_highF']),filter_keep_lowF=unname(parms['NV_filter_keep_lowF']),filter_keep_highF=unname(parms['NV_filter_keep_highF']),signalType=unname(parms['NV_signalType']))
  } else {
    print( 'Did not find the appropriate signal type')
  }
  

  return( parms )
}
