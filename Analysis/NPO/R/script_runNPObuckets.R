script_runNPObuckets <- function() {
  compArgs <- RFactories::argumentComposite()
  dp <- RFactories::databaseProvider(user="markrbower",vault_user='markrbower',vault_key='NSME_Halo_password',host='localhost',dbname='NSME_Halo')
  compArgs$add( dp )
#  compArgs$add( RFactories::fileProvider(path='NPO/Analysis/NPO/tests/testData/Halo/11',iterationType='directory',pattern="*.mef") )
  compArgs$add( RFactories::fileProvider(path='NPO/Analysis/NPO/tests/testData/William/2016-03-03_10-47-59',iterationType='directory',pattern="*.mef") )
    compArgs$add( RFactories::analysisInformer(experiment='Halo10sec_10x',subject='11',centerTime=0,pattern="*.mef") )
  pInf <- RFactories::parameterInformer(signalType='AP')
  pInf$loadParameters( dp )  #  The parameterInformer requires a databaseProvidere to load parameters from the database.
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
   dib <<- topconnect::databaseInsertBuffer('NSME_halo','Halo10sec_10x_11_AP_0_1000000_50_P', fields, 100, updates=updateFields, dbuser='root', host='localhost', password='' );
   bucket_output <<- buckets::bucket( accumulatorSize=0, NULL, dib );
   bucket_comm <<- buckets::bucket( accumulatorSize=0, NPO:::computeCommunities_closure( compArgs ), bucket_output, primeSize=5 );
   bucket_CC <<- buckets::bucket( accumulatorSize=0, NPO:::computeCC_closure( compArgs ), bucket_comm, primeSize=5 );
   bucket_input <<- buckets::bucket( accumulatorSize=0, NPO:::computePeaks_closure( compArgs ), bucket_CC, primeSize=5 )"
  
  NPO:::NPO_buckets( compArgs, algorithm_NPO )
}
