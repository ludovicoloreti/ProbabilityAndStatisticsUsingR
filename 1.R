library(ggplot2)
library(stringr)
library(data.table)

datiChicago <- read.csv('~/Documents/EsameStatistica2k17/ProbabilityAndStatisticsUsingR/dataset/motor_vehicle_theft_final.csv', stringsAsFactors = FALSE)
#splitto lat e long usando extract dal pacchetto tidyr - estraggo da datiChicago, colonna Location, e creo due colonne lati e longi
#http://stackoverflow.com/questions/26383776/how-to-split-r-data-frame-column-based-regular-expression-condition
# na.amit rimuovo tutti i record con campi vuoti
# datoChicago <- na.omit(datiChicago)
# datiChicago$Location = gsub("\\(", "", datiChicago$Location)
# datiChicago$Location = gsub("\\)", "", datiChicago$Location)
# 
# setDT(datiChicago)[, paste0('coords',1:2) := tstrsplit(Location, ",")]
# datiChicago$Location <- NULL
# names(datiChicago)[names(datiChicago) == 'coords1'] <- "Latitude"
# names(datiChicago)[names(datiChicago) == 'coords2'] <- "Longitude"
# datiChicago
# write.csv(datiChicago,'~/Documents/EsameStatistica2k17/ProbabilityAndStatisticsUsingR/dataset/motor_vehicle_theft_final.csv')
  
#Converting the date to a recognizable format
datiChicago$Date <- strptime(datiChicago$Date, format = '%m/%d/%Y %I:%M:%S %p')

#Getting the day and hour of each crime
datiChicago$Day <- weekdays(datiChicago$Date)
datiChicago$Hour <- datiChicago$Date$hour

#Sorting the weekdays
dailyCrimes <- as.data.frame(table(datiChicago$Day, datiChicago$Hour))
names(dailyCrimes) <- c('Day', 'Hour', 'Freq')
dailyCrimes$Hour <- as.numeric(as.character(dailyCrimes$Hour))
dailyCrimes$Day <- factor(dailyCrimes$Day, ordered = TRUE, 
                          levels = c('Domenica', 'Lunedì', 'Martedì', 'Mercoledì', 'Giovedì', 'Venerdì', 'Sabato'))

#Plotting the number of crimes each day (line graph)
ggplot(dailyCrimes, aes(x = Hour, y = Day)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = 'Furti totali', low = 'white', high = 'red') + theme(axis.title.y = element_blank())
