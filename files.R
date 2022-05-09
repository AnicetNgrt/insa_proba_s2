FileMM1 <- function(lambda, mu,  D)
{
  proch_arrivee = rexp(1, lambda)
  en_attente = D+1
  att_count = 0
  arrivees = c()
  departs = c()
  counts = c()
  tseq = seq(1, D, by=0.001)
  for (t in tseq) {
    counts = append(counts, att_count)
    if (t >= proch_arrivee) {
      arrivees = append(arrivees, proch_arrivee)
      #print("===========")
      #cat("Arrivee ", t, "\n")
      if (att_count <= 0) {
        en_attente = t+rexp(1, mu)
        #cat("Arrivé mis en attente ", en_attente, "\n")
      }
      att_count = att_count + 1
      proch_arrivee = t+rexp(1, lambda)
      #cat("En attente ", att_count, "\n")
    }
    if (t >= en_attente) {
      departs = append(departs, en_attente)
      #print("===========")
      #cat("Départ ", t, "\n")
      att_count = att_count - 1
      if (att_count >= 1) {
        en_attente = t+rexp(1, mu)
        #cat("Prochain mis en attente ", en_attente, "\n")
      } else {
        en_attente = D+1
      }
      #cat("En attente ", att_count, "\n")
    }
  }
  
  return(list(departs=departs, arrivees=arrivees, counts=list(x=tseq, y=counts)))
}

TempsAtt <- function(arrivees, departs)
{
  tempsatt = c()
  for (i in 1:length(departs)) {
    tempsatt = append(tempsatt, departs[[i]]-arrivees[[i]])
  }
  return(tempsatt)
}

FileMM <- function(lambda, mu, n, D)
{
  proch_arrivee = rexp(1, lambda)
  en_attente = c()
  att_count = 0
  arrivees = c()
  departs = c()
  counts = c()
  tseq = seq(1, D, by=0.001)
  for (t in tseq) {
    counts = append(counts, att_count)
    if (t >= proch_arrivee) {
      arrivees = append(arrivees, proch_arrivee)
      
      if (length(en_attente) < n) {
        en_attente = append(en_attente, t+rexp(1, mu)) 
      }
      
      att_count = att_count + 1
      proch_arrivee = t+rexp(1, lambda)
    }
    
    for (fini in en_attente[t >= en_attente]) {
      departs = append(departs, fini)
      att_count = att_count - 1
    }
    
    en_attente = en_attente[!(t >= en_attente)]
    
    i = 0
    slots_pris = length(en_attente)
    slots = n-slots_pris
    while (i < slots && i < att_count-slots_pris) {
      en_attente = append(en_attente, t+rexp(1, mu))
      i = i+1
    }
  }
  
  return(list(departs=departs, arrivees=arrivees, counts=list(x=tseq, y=counts)))
}