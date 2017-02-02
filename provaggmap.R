library(ggmap)

# load the data
tartu_housing <- read.csv('~/Documents/EsameStatistica2k17/ProbabilityAndStatisticsUsingR/dataset/motor_vehicle_theft_final.csv', sep = ";")

# Download the base map
tartu_map_g_str <- get_map(location = "tartu", zoom = 13)
# Draw the heat map
ggmap(tartu_map_g_str, extent = "device") + geom_density2d(data = tartu_housing, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = tartu_housing, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)