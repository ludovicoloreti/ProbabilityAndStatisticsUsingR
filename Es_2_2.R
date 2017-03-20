# Creo la mia distribuzione esponenziale e preparo i dati per il controllo sucessivo
k <- 1000
vettore <- c(3,7,69,92,900,100000)
lambda <- 2
plot(function(x) dexp(x,rate=lambda),0,5,main="Distribuzione Esponenziale")
mu_ex <- 1/lambda
sigma_ex <- 1/lambda
mediaEsatta <- mean(vettore)
for(i in vettore) {
  m <- NULL
  m <- replicate(k, mean(rexp(i,rate=lambda)))
  m_campana <- mean(m)
  hist(m, breaks=30, main="Distribuzione Esponenziale", freq=F)
  s <- sigma_ex/sqrt(i)
  curve(dnorm(x,mu_ex,s), col="green", add=TRUE)
  curve(dnorm(x,m_campana,s), col="red", add=TRUE)
}