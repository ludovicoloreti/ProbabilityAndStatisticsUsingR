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