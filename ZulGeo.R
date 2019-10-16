library(ggplot2)
library(readr)
library(readxl)
library(tidyverse)
library(tidyr)
library(readr)
library(lubridate)

setwd("C:/Users/abhis/Documents/R/Case Study")

komp_fzg_einzeil <- read.csv("FehlerFahrzeugeT05T06.csv")

Geodaten <- read_csv2("Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv")
Zulassungen <- read_csv2("Zulassungen_alle_Fahrzeuge.csv")

#maybe not?
Geodaten_selection <- Geodaten %>%
  filter(Gemeinde == "DRESDEN")
  

Geodaten$Breitengrad <- gsub(".{1}$","",Geodaten$Breitengrad)
Geodaten$Breitengrad <- as.numeric(gsub(",",".",Geodaten$Breitengrad))


#sollte zulassungsdatum auch considered werden? nein sonst zul_fzg merge geht nicht
Zulassungen$Zulassung <- ymd(Zulassungen$Zulassung)

#dresden important?
Zulassungen_selection <- Zulassungen %>%
  rename("ID_Fahrzeug" = "IDNummer") %>%
  rename("Gemeinde" = "Gemeinden")

Geodaten_selection$Breitengrad <- gsub(".{1}$","",Geodaten_selection$Breitengrad)
Geodaten_selection$Breitengrad <- as.numeric(gsub(",",".",Geodaten_selection$Breitengrad))


#not sure if appropriate, aber these were zugelassen in Dresden obwohl Fehler

Zul_fzg <- merge(komp_fzg_einzeil, Zulassungen_selection, by = "ID_Fahrzeug") %>%
  select(-c(X, X1)) %>%
  merge(Geodaten, by = "Gemeinde") %>%
  select(-c(X, X1))

# function to calculate distance between stuttgart and datapoint
calc_distance <- function(df){
  distance <- rep(0, nrow(df))
  # lat/lng stuttgart
  lat_st = 48.7706 * pi / 180
  lng_st = 9.18457 * pi / 180
  R_earth = 6371 # radius earth [km]
  # calc distance between stuttgart - point
  # after haversine distance algorithm
  for (i in 1:nrow(df)) {
    lng1 <- df$Laengengrad[i] * pi / 180
    lat1 <- df$Breitengrad[i] * pi / 180
    delta_lng <- (lng_st - lng1)
    delta_lat <- (lat_st - lat1)
    var_a <- sin(delta_lat/2) * sin(delta_lat/2) + cos(lat_st) * cos(lat1) * sin(delta_lng/2) * sin(delta_lng/2)
    var_c <- 2 * atan2(sqrt(var_a), sqrt(1 - var_a))
    var_d <- R_earth * var_c # [km]
    distance[i] <- round(var_d, digits = 2)
  }
  df <- cbind(df,distance)
  return(df)
}


#method for filtered data and geo data.


filtered_data <- calc_distance(Zul_fzg)
data_geo <- calc_distance(Geodaten)
data_geo <- data_geo %>%
  select(-X1)

# count occurences in each gemeinde
col_matches <- rep(0, nrow(data_geo))
for (i in 1:nrow(data_geo)) {
  col_matches[i] <- sum(filtered_data$Gemeinde == data_geo$Gemeinde[i])
  col_matches[i][col_matches[i] == 0] <- NA
}
data_geo <- cbind(data_geo, col_matches) %>%
  na.omit(col_matches) # remove rows with 0 matches since they should not appear on the map


  # save as new .csv
write.csv(filtered_data, file = "filtered_data.csv", na = "")
# saves cleaned file gemeinde
write.csv(data_geo, file = "gemeinden_clean.csv", na = "")

