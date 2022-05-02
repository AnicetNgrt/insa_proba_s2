piProp <- function(bits)
{
  return(sum(bits)/length(bits))
}

vnStat <- function(bits)
{
  vn <- 1
  for (i in 1:(length(bits)-1))
  {
    if (bits[i] != bits[i+1])
    {
      vn <- vn + 1
    }
  }
  return(vn)
}

Runs <- function(x, nb)
{
  bits <- c()
  for(i in 1:length(x)) {
    bits <- c(bits, binary(x[i]))
  }
  bits <- bits[1:nb]
  
  n <- length(bits)
  
  pi <- piProp(bits)
  if (abs(pi-1/2) >= 2/sqrt(n))
    return(0.0)
  
  vn <- vnStat(bits)
  return(2*(1-pnorm(abs(vn-2*n*pi*(1-pi))/(2*sqrt(n)*pi*(1-pi)))))
}