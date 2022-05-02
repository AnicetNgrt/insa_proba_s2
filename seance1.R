library(randtoolbox)
source('generateurs.R')

seed <- 13173
Nsimu <- 100
Nrepet <- 1

# 1

vn <- VonNeumann(Nsimu,Nrepet,seed)
mt <- MersenneTwister(Nsimu,Nrepet,seed)
st <- StandardMinimal(Nsimu,Nrepet,seed)
rd <- Randu(Nsimu,Nrepet,seed) 

# 2.1

par(mfrow=c(1,4))
hist(mt[,1],xlab='',main='Mersenne Twister')
hist(vn[,1],xlab='',main='Von Neumann')
hist(st[,1],xlab='',main='Standard Minimal')
hist(rd[,1],xlab='',main='Randu')

# 2.2

vn <- VonNeumann(Nsimu,10,seed)
mt <- MersenneTwister(Nsimu,10,seed)
st <- StandardMinimal(Nsimu,10,seed)
rd <- Randu(Nsimu,10,seed) 

par(mfrow=c(1,4))
plot(mt[1:(Nsimu-1),1],mt[2:Nsimu,1],xlab='MT(i)', ylab='MT(i+1)', main='Mersenne Twister', xlim=c(0,1))
plot(vn[1:(Nsimu-1),1],vn[2:Nsimu,1],xlab='VN(i)', ylab='VN(i+1)', main='Von Neumann', xlim=c(0,1))
plot(st[1:(Nsimu-1),1],st[2:Nsimu,1],xlab='ST(i)', ylab='ST(i+1)', main='Standard Minimal', xlim=c(0,1))
plot(rd[1:(Nsimu-1),1],rd[2:Nsimu,1],xlab='RD(i)', ylab='RD(i+1)', main='Randu', xlim=c(0,1))

# 3

seeds <- sample.int(2^30, 100)

source('monobit.R')

pvalvn = rep(0,length(seeds))
pvalmt = rep(0,length(seeds))
pvalst = rep(0,length(seeds))
pvalrd = rep(0,length(seeds))
offvnct = 0
offmtct = 0
offstct = 0
offrdct = 0

for (i in 1:length(seeds)) {
  seed <- seeds[i] 
  vn <- VonNeumann(Nsimu,Nrepet,seed)
  mt <- MersenneTwister(Nsimu,Nrepet,seed)
  st <- StandardMinimal(Nsimu,Nrepet,seed)
  rd <- Randu(Nsimu,Nrepet,seed)
  
  pvalmt[i] <- Frequency(mt[,1], 100)
  pvalvn[i] <- Frequency(vn[,1], 100)
  pvalst[i] <- Frequency(st[,1], 100)
  pvalrd[i] <- Frequency(rd[,1], 100)
  
  if(pvalmt[i] < 0.1) offmtct <- offmtct + 1
  if(pvalvn[i] < 0.1) offvnct <- offvnct + 1
  if(pvalst[i] < 0.1) offstct <- offstct + 1
  if(pvalrd[i] < 0.1) offrdct <- offrdct + 1
}

par(mfrow=c(1,4))
hist(pvalmt,xlab='Frequency monobit',main='Mersenne Twister', xlim=c(0,1))
hist(pvalvn,xlab='Frequency monobit',main='Von Neumann', xlim=c(0,1))
hist(pvalst,xlab='Frequency monobit',main='Standard Minimal', xlim=c(0,1))
hist(pvalrd,xlab='Frequency monobit',main='Randu', xlim=c(0,1))
print("Mersenne Twister < 0.1 count")
print(offmtct)
print("Von Neumann < 0.1 count")
print(offvnct)
print("Standard Minimal < 0.1 count")
print(offstct)
print("Randu < 0.1 count")
print(offrdct)

# 4

source('runs.R')

pvalvn = rep(0,length(seeds))
pvalmt = rep(0,length(seeds))
pvalst = rep(0,length(seeds))
pvalrd = rep(0,length(seeds))
offvnct = 0
offmtct = 0
offstct = 0
offrdct = 0

for (i in 1:length(seeds)) {
  seed <- seeds[i] 
  vn <- VonNeumann(Nsimu,Nrepet,seed)
  mt <- MersenneTwister(Nsimu,Nrepet,seed)
  st <- StandardMinimal(Nsimu,Nrepet,seed)
  rd <- Randu(Nsimu,Nrepet,seed)
  
  pvalmt[i] <- Runs(mt[,1], 100)
  pvalvn[i] <- Runs(vn[,1], 100)
  pvalst[i] <- Runs(st[,1], 100)
  pvalrd[i] <- Runs(rd[,1], 100)
  
  if(pvalmt[i] < 0.1) offmtct <- offmtct + 1
  if(pvalvn[i] < 0.1) offvnct <- offvnct + 1
  if(pvalst[i] < 0.1) offstct <- offstct + 1
  if(pvalrd[i] < 0.1) offrdct <- offrdct + 1
}

par(mfrow=c(1,4))
hist(pvalmt,xlab='Runs p value',main='Mersenne Twister', xlim=c(0,1))
hist(pvalvn,xlab='Runs p value',main='Von Neumann', xlim=c(0,1))
hist(pvalst,xlab='Runs p value',main='Standard Minimal', xlim=c(0,1))
hist(pvalrd,xlab='Runs p value',main='Randu', xlim=c(0,1))
print("Mersenne Twister < 0.1 count")
print(offmtct)
print("Von Neumann < 0.1 count")
print(offvnct)
print("Standard Minimal < 0.1 count")
print(offstct)
print("Randu < 0.1 count")
print(offrdct)

# 5

pvalvn = rep(0,length(seeds))
pvalmt = rep(0,length(seeds))
pvalst = rep(0,length(seeds))
pvalrd = rep(0,length(seeds))
offvnct = 0
offmtct = 0
offstct = 0
offrdct = 0

for (i in 1:length(seeds)) {
  seed <- seeds[i] 
  vn <- VonNeumann(Nsimu,Nrepet,seed)
  mt <- MersenneTwister(Nsimu,Nrepet,seed)
  st <- StandardMinimal(Nsimu,Nrepet,seed)
  rd <- Randu(Nsimu,Nrepet,seed)
  
  pvalmt[i] <- order.test(mt[,1][1:80], d=4, echo=FALSE)$p.value
  pvalvn[i] <- order.test(vn[,1][1:80], d=4, echo=FALSE)$p.value
  pvalst[i] <- order.test(st[,1][1:80], d=4, echo=FALSE)$p.value
  pvalrd[i] <- order.test(rd[,1][1:80], d=4, echo=FALSE)$p.value
  
  if(pvalmt[i] < 0.1) offmtct <- offmtct + 1
  if(pvalvn[i] < 0.1) offvnct <- offvnct + 1
  if(pvalst[i] < 0.1) offstct <- offstct + 1
  if(pvalrd[i] < 0.1) offrdct <- offrdct + 1
}

par(mfrow=c(1,4))
hist(pvalmt,xlab='order.test p value',main='Mersenne Twister', xlim=c(0,1))
hist(pvalvn,xlab='order.test p value',main='Von Neumann', xlim=c(0,1))
hist(pvalst,xlab='order.test p value',main='Standard Minimal', xlim=c(0,1))
hist(pvalrd,xlab='order.test p value',main='Randu', xlim=c(0,1))
print("Mersenne Twister < 0.1 count")
print(offmtct)
print("Von Neumann < 0.1 count")
print(offvnct)
print("Standard Minimal < 0.1 count")
print(offstct)
print("Randu < 0.1 count")
print(offrdct)
