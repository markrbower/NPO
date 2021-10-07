script_plotSeizureHalo11 <- function() {
  library(signal)
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Analysis/NPO/R/movingWindowPower.R')
  result <- movingWindowPower()
  plot( result$power )
  plot( result$power[20:100] )
  result$times[20]
  result$times[100]
  topsecret::init_here()
  mef_filename <- here("mef2/CSC33.mef")
  vault <- topsecret::get_secret_vault()
  info <- meftools::mef_info( c(mef_filename,secret::get_secret("MEF_password",key=secret::local_key(),vault=vault)) )
  D <- meftools::getMEFwindow(mef_filename,5396964400,5476964400,info)
  # Filter and downsample by a factor of 10
  bf <- butter( 3, .01, 'low')
  y <- filtfilt( bf, D )
  yy <- y[seq(from=1,to=length(D),by=10)]
  # Look for the stim artifact
  plot(yy,type='l')
  plot(yy[87000:97000],type='l')
  # Look at an individual stim
  plot(yy[89000:90000],type='l') # Period is 100 samples at 312.5 usec per/sample = 31.25 msec -> 30 Hz
  
  # Look at the stim and the seizure
  plot(yy[85000:160000],type='l')
  
  # The seizure starts around 85+18 *1000 and goes to 85+70 *1000.
  # It has a duration of (70-18)=52 *1000 = 16.25 sec


}

