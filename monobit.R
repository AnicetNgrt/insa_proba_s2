binary <- function(x)
{
  if((x<2^31)&(x>=0))
    return( as.integer(intToBits(as.integer(x))) )
  else{
    if((x<2^32)&(x>0))
      return( c(binary(x-2^31)[1:31], 1) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}

negateZeros <- function(bit)
{
  if(bit == 0)
    return(-1)
  else
    return(1)
}

surplus <- function(bits)
{
  s <- 0
  for(i in 1:length(bits))
  {
    s <- s + negateZeros(bits[i])
  }
  return(s)
}

Frequency <- function(seq, nb)
{
  bits <- c()
  for(i in 1:length(seq)) {
    bits <- c(bits, binary(seq[i]))
  }
  bits <- bits[1:nb]
  s <- surplus(bits)
  sobs <- abs(s)/sqrt(length(bits))
  return(2*(1-pnorm(sobs)))
}