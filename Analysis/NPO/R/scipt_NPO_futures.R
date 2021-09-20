script_runNPObuckets <- function() {
  argComp <- RFactories::argumentComposite()
  dp <- RFactories::databaseProvider(user="markrbower",vault_user='markrbower',vault_key='NV_password',host='localhost',dbname='NV')
  argComp$add( dp )
  argComp$add( RFactories::fileProvider(path='NPO/Analysis/NPO/tests/testData/Halo/11',iterationType='directory',pattern="*.mef") )
  argComp$add( RFactories::analysisInformer(experiment='Halo10sec_10x',subject='11',centerTime=0,pattern="*.mef") )
  pInf <- RFactories::parameterInformer(signalType='AP')
  pInf$loadParameters( dp )  #  The parameterInformer requires a databaseProvidere to load parameters from the database.
  argComp$add( pInf )
  
  algorithm_NPO <- 
    "bucket_output <- buckets::bucket( accumulatorSize=0, NULL, databaseInsertBuffer( 'MRE_test', 'test', fields, 2 ) );
   computeCommunities_ <- computeCommunities_closure( argComp );
   bucket_comm <- buckets::bucket( accumulatorSize=0, computeCommunities, bucket_output );
   computeCC_ <- computeCC_closure( argComp );
   bucket_CC <- buckets::bucket( accumulatorSize=0, computeCC_, bucket_comm, prime=5 );
   findPeaks_ <- computePeaks_closure( argComp );
   bucket_input <- buckets::bucket( accumulatorSize=0, findPeaks_, bucket_CC )"
  
  NPO:::NPO_buckets( argComp, algorithm_NPO )
}
