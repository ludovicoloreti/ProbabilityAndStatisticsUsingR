library(ggplot2)
library(stringr)
library(data.table)
library(ggmap)
library(dplyr)


data("AirPassengers")
airp <- AirPassengers
knitr::kable(airp)

datiChicago <- read.csv('~/Desktop/Statistica /ProbabilityAndStatisticsUsingR/dataset/motor_vehicle_theft_final.csv', stringsAsFactors = FALSE)


str(datiChicago)

#https://www.r-bloggers.com/dplyr-example-1/



byMonSex <- group_by(datiChicago, Year,Description)
asd <- ( sumMonSex <- summarize(byMonSex,count=n()))
plot(asd)
summary(asd)



reverseYear <- function(data){
  
}

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
ggplot(dailyCrimes, aes(x = Hour, y = Day)) 
        + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = 'Furti totali', low = 'white', high = 'red') 
        + theme(axis.title.y = element_blank())



ChicagoMap <- qmap(location = "chicago", zoom = 10,  legend = "topleft")
ChicagoMap + geom_point(aes(x = Longitude, y = Latitude, colour = Location.Description, size = Location.Description), data = datiChicago)
ChicagoMap +
  stat_bin_2d(
    aes(x = Longitude, y = Latitude, colour = Location.Description, fill = Location.Description),
    size = .5, bins = 30, alpha = 0.4,   
    data = datiChicago
  )


summary(datiChicago$Year)


for(anno in 2001:2016){
  dataAnno = subset(datiChicago, Year = anno)
  print(anno)
}
ggmap(ChicagoMap, extent = "device") + geom_density2d(data = datiChicago, aes(x = Longitude, y = Latitude), size = 0.1) + 
  stat_density2d(data = datiChicago, 
                aes(x = Longitude, y = Latitude, fill = ..level.., alpha = 0.4), size = 0.01, 
                bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
                scale_alpha(range = c(0, 0.1), guide = FALSE)

