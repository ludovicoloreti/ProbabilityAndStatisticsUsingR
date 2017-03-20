# Calcolare intervallo confidenza media mu di una dist normale
# norm(mu=5,sd=2) nota la dev standard. Al variare di n e del lvl di confidena 1-a
# Visualizzare graficamente i risultati ottenuti, confrontandoli con la media esatta
stdev <- 2; 
mu <- 5
nn = c(20, 30, 100, 200)
alfas = c(0.05, 0.005, 0.1)

vector_zalfa = NULL
vector_mean = NULL

#funzione che calcola gli intervalli di confidenza
intervalli = function(n, alfa) {
  cat("\n Con alpha = ", alfa, " e n = ",n)
  distrib = rnorm(n, mu, stdev); distrib
  media = mean(distrib)
  sigma = sd(distrib)
  cat("\n\tMedia: ",media); cat("\n\tDev Standard: ", sigma)
  
  plot(distrib, type="l",  xlab="# Iterazioni", ylim=c(0, 10), col="black")
  
  zalfa = qnorm(1-alfa/2, 0, 1); zalfa
  
  za_minus = media - zalfa*stdev/(sqrt(n))
  za_plus = media + zalfa*stdev/(sqrt(n))
  cat("\n\tZalfa: ",zalfa); cat("\n\tEstremo inferiore: ", za_minus); cat("\n\tEstremo superiore: ", za_plus)
  
  abline(h=za_minus, col="green") # intervallo di confidenza inferiore
  abline(h=media, col="blue", lwd=3) # media calcolara
  abline(h=20, col="red", lwd=2) # media teorica
  abline(h=za_plus, col="orange") # intervallo di confidenza superiore
}

for( n in nn ) {
  for( alfa in alfas ) {
    intervalli(n, alfa)
  }
}

# All'aumetare di n (numero di elementi generati) l'intevallo di confidenza diminuisce 
# Se 1-alfa (livello di confidenza) aumenta, l'intervallo di confidenza aumeta
# Se l'intervallo di confidenza aumenta, L'intervallo aumenta
# questo perchè se alfa diminuisce, zalfa aumenta (zalfa è il quantile di Z di alfa/2)