persistCSV <- function( dbConn, dbTable, fields, csvFile, subject ) {
  # Usage:
  # persistCSV( db, 'epochs', list('subject','label','start','stop'), '/Users/m063341/Dropbox/Documents/Concepts/2016_09_28_Project_Koala/Data/classes_23_003.csv', 1242 )
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/databaseInsertBuffer.R')
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/rows_along.R')
  
  dib <- databaseInsertBuffer( dbConn, dbTable, fields, 100 )  
  data <- read.csv(file=csvFile, header=F, sep=",", stringsAsFactors = F)
  lapply( seq(from=1,to=nrow(data)), function(x) {dib$insert( c(subject=subject, label=data[x,3], start=data[x,1], stop=data[x,2] ) )} )
  dib$flush()
}
