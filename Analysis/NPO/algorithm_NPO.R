algorithm_NPO <- 
  "bucket_output <- buckets::bucket( accumulatorSize=0, NULL, databaseInsertBuffer( 'MRE_test', 'test', fields, 2 ) );
   computeCommunities_ <- computeCommunities_closure( argComp );
   bucket_comm <- buckets::bucket( accumulatorSize=0, computeCommunities, bucket_output );
   computeCC_ <- computeCC_closure( argComp );
   bucket_CC <- buckets::bucket( accumulatorSize=0, computeCC_, bucket_comm, prime=5 );
   findPeaks_ <- computePeaks_closure( argComp );
   bucket_input <- buckets::bucket( accumulatorSize=0, findPeaks_, bucket_CC )"

