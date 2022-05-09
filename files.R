FileMM1 <- function(lambda, mu,  D)
{
  proch_arrivee = rexp(lambda)
  en_attente = 0
  att_count = 0
  for (t in 1:D) {
    if (t > proch_arrivee) {
      if (att_count == 0) {
        en_attente = t+rexp(mu)
      }
      att_count = att_count + 1
    }
  }
}