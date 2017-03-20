# Simple Random Sample (n) distribuzione normale con media e varianza assegnate 
# verifico che la distribuzione della media campionaria è norm(mean=mu,sd=sigma/sqer(n))
mu <- 5 # media
sigma <- 1 # deviazione standard
k <- 1000 # numero campioni considerati
n <- 10 # elementi del campione
# Assegno in 'm' tramite replicate valori che appartengono alla distribuzione della media
# con rnorm creo valori all'interno della distribuzione normale con i dati assegnati
# la lunghezza del risultato di rnorm è data dalla 'n'
m <- replicate(k, mean(rnorm(n, mu, sigma)))
# calcolo la media dei dati appartenenti alla distribuzione della media
mean(m)
# Creo un grafico (istogramma) della distribuzione della media campionaria e verifico che la formula rispecchi l'andamento
hist(m, breaks = 20, main = "distribuzione della media campionaria", xlab='Mean', ylab='Density', freq=F)
# Verifico che la formula verifichi l'andamento dell'istogramma
curve(dnorm(x,mean(m),sd(m)), col="red", from=-10, to=10, add=TRUE)
# Ora verifico che la formula della media campionaria norm(mean=mu,sd=sigma/sqer(n))
# rispecchi la distribuzione della media campionaria
curve(dnorm(x,mean(m),sigma/sqrt(n)), col="green", from=-10, to=10, add=TRUE)
# Verificato che la formula rispecchia l'andamento, continuo modificando i parametri n,k
n <- 30
k <- 3000
m <- replicate(k, mean(rnorm(n, mu, sigma)))
hist(m, breaks = 20, main = "distribuzione della media campionaria #1", xlab='Mean', ylab='Density', freq=F)
curve(dnorm(x,mean(m),sigma/sqrt(n)), col="green", from=-10, to=10, add=TRUE)
# Continua.. #2
n <- 5
k <- 500
m <- replicate(k, mean(rnorm(n, mu, sigma)))
hist(m, breaks = 20, main = "distribuzione della media campionaria #2", xlab='Mean', ylab='Density', freq=F)
curve(dnorm(x,mean(m),sigma/sqrt(n)), col="green", from=-10, to=10, add=TRUE)
# Continua.. #3
n <- 30
k <- 500
m <- replicate(k, mean(rnorm(n, mu, sigma)))
hist(m, breaks = 20, main = "distribuzione della media campionaria #3", xlab='Mean', ylab='Density', freq=F)
curve(dnorm(x,mean(m),sigma/sqrt(n)), col="green", from=-10, to=10, add=TRUE)

### --- verificato! ---

