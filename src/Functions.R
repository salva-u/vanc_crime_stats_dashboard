library(readr)
library(dplyr)
library(terra)

#' Function removed missing data rows and converts the UTM Time zone coords to Lat/Long
preprocess <- function() {
  
    dataset_0 <- read_csv("../data/raw/crimedata_csv_AllNeighbourhoods_2024.csv")
  
  # Remove rows with missing values
  dataset_0 <- na.omit(dataset_0)
  #Conver the UTM coords Zone 10 to Lat/Long
  points <- cbind(dataset_0$X, dataset_0$Y)
  v <- vect(points, crs="+proj=utm +zone=10 +datum=WGS84  +units=m")
  y <- project(v, "+proj=longlat +datum=WGS84")
  lonlat <- geom(y)[, c("x", "y")]
  dataset_0$X <- lonlat[, "x"]
  dataset_0$Y <- lonlat[, "y"]

  return(dataset_0)
}


preprocess()
