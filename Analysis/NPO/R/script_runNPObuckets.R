script_runNPObuckets <- function() {
#  options(error=recover)
  compArgs <- RFactories::argumentComposite()
  dbp <- RFactories::databaseProvider(user="root",vault_user='markrbower',vault_key='NV_password',host='localhost',dbname='NV')
#  dbp <- RFactories::databaseProvider(user="root",vault_user='markrbower',vault_key='NSME_halo_password',host='localhost',dbname='nsme_halo')
  compArgs$add( dbp )
#  compArgs$add( RFactories::fileProvider(path='NPO/Analysis/NPO/tests/testData/Halo/11',iterationType='directory',pattern="*.mef") )
#  compArgs$add( RFactories::fileProvider(path='NPO/Analysis/NPO/tests/testData/William/2016-03-03_10-47-59',iterationType='directory',pattern="*.mef") )
  compArgs$add( RFactories::fileProvider(path='/Volumes/Data/NV/NVC1001_23_002_2',iterationType='directory',pattern="*.mef") )
#  aInf <- RFactories::analysisInformer(experiment='Halo10sec_10x',subject='11',centerTime=0,pattern="*.mef",lab="RNCP")
  aInf <- RFactories::analysisInformer(experiment='NeuroVista',subject='23_002',centerTime=0,pattern="*.mef",lab="RNCP")
  compArgs$add( aInf )
  pInf <- RFactories::parameterInformer(signalType='IIS')
  pInf$loadParameters( dbp, aInf )  #  The parameterInformer requires a databaseProvidere to load parameters from the database.
  compArgs$add( pInf )
  
  
#  mysql> describe P;
#  +-------------+-------------+------+-----+-------------------+-------------------+
#    | Field       | Type        | Null | Key | Default           | Extra             |
#    +-------------+-------------+------+-----+-------------------+-------------------+
#    | subject     | varchar(32) | YES  | MUL | NULL              |                   |
#    | channel     | varchar(32) | YES  |     | NULL              |                   |
#    | time        | bigint      | YES  |     | NULL              |                   |
#    | waveform    | mediumtext  | YES  |     | NULL              |                   |
#    | cluster     | varchar(32) | YES  |     | NULL              |                   |
#    | clusterid   | int         | YES  |     | NULL              |                   |
#    | seizureUsed | bigint      | YES  |     | NULL              |                   |
#    | peak        | double      | YES  |     | NULL              |                   |
#    | energy      | double      | YES  |     | NULL              |                   |
#    | incident    | mediumtext  | YES  |     | NULL              |                   |
#    | weights     | mediumtext  | YES  |     | NULL              |                   |
#    | created_on  | datetime    | NO   |     | CURRENT_TIMESTAMP | DEFAULT_GENERATED |
#    | UUID        | varchar(36) | YES  |     | NULL              |                   |
#    +-------------+-------------+------+-----+-------------------+-------------------+
#    13 rows in set (0.02 sec)
  
  algorithm_NPO <- 
  "fields <<- c('subject','channel','time','waveform','clusterid','seizureUsed','peak','energy','incident','weights','UUID');
   updateFields <<- c('clusterid')
   dib <<- topconnect::databaseInsertBuffer('NV','NeuroVista_23_002_IIS_0_60000000_50_P', fields, 100, updates=updateFields, dbuser='root', host='localhost', password='' );
   bucket_L <<- buckets::bucket_last( accumulatorSize=0, NULL, dib );
   bucket_comm <<- buckets::bucket( accumulatorSize=0, NPO:::computeCommunities_closure( compArgs ), bucket_L, primeSize=5 );
   bucket_CC <<- buckets::bucket( accumulatorSize=0, NPO:::computeCC_closure( compArgs ), bucket_comm, primeSize=5 );
   bucket_F <<- buckets::bucket_first( accumulatorSize=0, NPO:::computePeaks_closure( compArgs ), bucket_CC, primeSize=5 )"
  
  NPO:::NPO_use_buckets( compArgs, algorithm_NPO )
}
