algorithm_test <- 
  "bucket_output <- buckets::bucket( accumulatorSize=0, NULL, NULL );
   test_function <- function(x) {x + data};
   bucket_input <- buckets::bucket( accumulatorSize=0, test_function, bucket_output )"
