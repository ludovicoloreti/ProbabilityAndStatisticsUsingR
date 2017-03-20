# Creo una distribuzione gamma sulla quale stimo i parametrirequire(stats4)
n = 100
sh = 5
rt = 25

rtVettore = NULL
shVettore= NULL
distrib= NULL

# Provo 10 campioni diversi
for(i in 1:10) {
  distrib = rgamma(n, shape=sh, rate=rt); distrib
  # Uso -log per diminuire l'ordine di grandezza per eseguire meglio i calcoli in aritmerica finita
  fl=function(sh, rt) -log(prod(dgamma(distrib, shape=sh, rate=rt))) 
  lmin = mle(fl, start=list(sh=0.1, rt=0.1)); cat("\n",coef(lmin))
  shVettore = append(shVettore, as.numeric(lmin@coef["sh"]))
  rtVettore = append(rtVettore, as.numeric(lmin@coef["rt"]))
}

coef(lmin)
summary(lmin)

shVettore; rtVettore

#Sommatoria (rate_stimato - rate)^2
somma_sh = 0
for( j in shVettore) {
  somma_sh = somma_sh + (j-sh)**2
}
#Sommatoria (shape_stimato - shape)^2
somma_rt = 0
for( j in rtVettore) {
  somma_rt = somma_rt + (j-rt)**2
}

lmin; length(rtVettore); shVettore; rtVettore; somma_sh; somma_rt
# Calcoliamo l'MSE di shape
MSE_sh = somma_sh/length(shVettore); MSE_sh

# Calcoliamo l'MSE di rate
MSE_rt = somma_rt/length(rtVettore); MSE_rt
#la stima di rate è fuori controllo