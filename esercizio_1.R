# importo datasets e uso USAccDeaths come csv dati che riporta le morti accidentali negli USA dal 1973 al 1978 da Gennaio a Dicembre
require(datasets)
# assegno alla variabile dati la megamatrice USAccDeaths
dati <- USAccDeaths
dati
anno = 1973
datas <- matrix(1:72,ncol=12)
k=1;
j=1;
for(i in 1:72) {
  datas[k,j] = dati[i]
  if (i %% 12 == 0) {
    print(anno)
    print( summary( datas[k,] ) )
    j=0  
    k = k+1
    anno = anno + 1
  }
  j = j+1
}

asd <- c(72,55,79,63,64)

plot(asd)
summary(asd)

1/5*((72-66.6)^2+(55-66.6)^2+(79-66.6)^2+(63-66.6)^2+(64-66.6)^2)

(sqrt(67.44)/66.6)*100

analisiDescr <- function(file, colStart, colEnd){
  dataset <- file[,colStart:colEnd]
  dim <- length(dataset)
  #2 significa colonne
  media <- apply(dataset, 2 , mean, na.rm = TRUE)
  varianza <-apply(dataset, 2,var, na.rm = TRUE)
  devStd <- sqrt(varianza)
  minimo <- apply(dataset, 2, min, na.rm = TRUE)
  massimo <- apply(dataset, 2, max, na.rm = TRUE)
  Desc <- data.frame(media, varianza, devStd, minimo, massimo)
  colnames <- c("media", "varianza", "devStd", "minimo", "massimo")
  return(Desc)
}

dati <- analisiDescr(monthlyByCateg, 1, 13)