v <- matrix( nrow=4, ncol=7 )
v[1,] <- c( 0, 0, 20, -30, -50, 0, 0 )
v[2,] <- -1 * c( 0, 5, 20, -30, -50, -20, 0 )
v[3,] <- c( 0, 10, 50, -120, -100, -80, -40 )
v[4,] <- c( 0, 0, 0, 0, 0, 0, 0 )
NC <- 4 # number of categories
N <- 1000 # total number of peaks
T <- cumsum(rpois(N,lambda=10))
L <- sample( rep( c(1,2,2,3,3,3,4,4,4,4), times=N/10 ), N, replace=FALSE )
D <- sapply( 1:N, function(i) v[L[i],] + rnorm(n=7,mean=0,sd=5))
D <- t( sapply( 1:N, function(i) list( v[L[i],] + rnorm(n=7,mean=0,sd=5)) ) )
Dl <- list()
for ( j in seq(1,1000) ) { Dl <- append( Dl, D[j])}
data <- as.data.frame( cbind( time=as.character(T), voltage=Dl ) )


