VonNeumann <- function(n, p=1, graine)
{
  x <-  rep(graine,n*p+1)
  for(i in 2:(n*p+1))
  {
    numbers <- strsplit(format(x[i-1]^2,scientific=FALSE),'')[[1]]
    while(length(numbers)>4){ 
      numbers <- numbers[2:(length(numbers)-1)] 
    }
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1,0,-1))
  }
  x <- matrix(x[2:(n*p+1)],nrow=n,ncol=p)
  return(x)
}

MersenneTwister <- function(n, p=1, graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(x)
}

congruenceLin <- function(n, p=1, graine, a, b, m)
{
  x <- rep(graine,n*p+1)

  for(i in 2:(n*p+1))
  {
    x[i] <- (x[i-1]*a + b)%%m
  }

  x <- matrix(x[2:(n*p+1)],nrow=n,ncol=p)
  return(x)
}

StandardMinimal <- function(n, p=1, graine) {
  return(congruenceLin(n, p, graine, 16807, 0, 2^31-1))
}

Randu <- function(n, p=1, graine) {
  return(congruenceLin(n, p, graine, 65539, 0, 2^31))
}

