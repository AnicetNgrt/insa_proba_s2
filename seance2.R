library(randtoolbox)
source('files.R')

# Questions 6 et 7 et 8
par(mfrow=c(2,2))

# 10 clients arrivent par heure en moy et 20 clients partent par heure en moy pendant 12 heures
file <- FileMM1(10, 20, 12)
plot(file$counts$x, file$counts$y, type='l', xlab='t', ylab='count', main='En attente 10/20')
cat("Nb client moyen 10/20 ", mean(file$counts$y), "\n")
cat("Temps attente moyen 10/20 ", mean(TempsAtt(file$arrivees, file$departs)), "\n")

file <- FileMM1(14, 20, 12)
plot(file$counts$x, file$counts$y, type='l', xlab='t', ylab='count', main='En attente 14/20')
cat("Nb client moyen 14/20 ", mean(file$counts$y), "\n")
cat("Temps attente moyen 14/20 ", mean(TempsAtt(file$arrivees, file$departs)), "\n")

file <- FileMM1(20, 20, 12)
plot(file$counts$x, file$counts$y, type='l', xlab='t', ylab='count', main='En attente 20/20')
cat("Nb client moyen 20/20 ", mean(file$counts$y), "\n")
cat("Temps attente moyen 20/20 ", mean(TempsAtt(file$arrivees, file$departs)), "\n")

file <- FileMM1(30, 20, 12)
plot(file$counts$x, file$counts$y, type='l', xlab='t', ylab='count', main='En attente 30/20')
cat("Nb client moyen 30/20 ", mean(file$counts$y), "\n")
cat("Temps attente moyen 30/20 ", mean(TempsAtt(file$arrivees, file$departs)), "\n")

file <- FileMM(20, 20, 2, 12)
plot(file$counts$x, file$counts$y, type='l', xlab='t', ylab='count', main='En attente 20/20/2')
cat("Nb client moyen 20/20/2 ", mean(file$counts$y), "\n")
cat("Temps attente moyen 20/20/2 ", mean(TempsAtt(file$arrivees, file$departs)), "\n")

file <- FileMM(40, 20, 2, 12)
plot(file$counts$x, file$counts$y, type='l', xlab='t', ylab='count', main='En attente 40/20/2')
cat("Nb client moyen 40/20/2 ", mean(file$counts$y), "\n")
cat("Temps attente moyen 40/20/2 ", mean(TempsAtt(file$arrivees, file$departs)), "\n")

file <- FileMM(80, 20, 2, 12)
plot(file$counts$x, file$counts$y, type='l', xlab='t', ylab='count', main='En attente 80/20/2')
cat("Nb client moyen 80/20/2 ", mean(file$counts$y), "\n")
cat("Temps attente moyen 80/20/2 ", mean(TempsAtt(file$arrivees, file$departs)), "\n")

file <- FileMM(80, 20, 4, 12)
plot(file$counts$x, file$counts$y, type='l', xlab='t', ylab='count', main='En attente 80/20/4')
cat("Nb client moyen 80/20/4 ", mean(file$counts$y), "\n")
cat("Temps attente moyen 80/20/4 ", mean(TempsAtt(file$arrivees, file$departs)), "\n")