library(ggplot2)
library(readr)
library(readxl)
library(tidyverse)
library(tidyr)
library(readr)
library(lubridate)

setwd("C:/Users/abhis/Documents/R/Case Study")

#reading all einzelteile, komponente and fahrzeuge, and bestandteile tabelle that relates them

T_05_dirty <- read_csv("Einzelteil_T05.csv")
T_06_dirty <- read_csv("Einzelteil_T06.csv")
Typ11_fzg_dirty <- read.csv("Fahrzeuge_OEM1_Typ11.csv")
K1DI1_komponente_dirty <- read.csv("Komponente_K1DI1.csv")
Bestandteile_typ11_fzg_dirty <- read.csv("Bestandteile_Fahrzeuge_OEM1_Typ11.csv")
Bestandteile_K1DI1_komponente <- read.csv("Bestandteile_Komponente_K1DI1.csv")

#Tidying each data table
#separate the one column into right columns with right names to relate to other tables

Bestandteile_typ11_fzg_clean <- Bestandteile_typ11_fzg_dirty %>%
  separate("X.ID_Karosserie.ID_Schaltung.ID_Sitze.ID_Motor.ID_Fahrzeug", c("X", "ID_Karoserie", "Schaltung", "Sitze", "ID_Motor", "ID_Fahrzeug"), sep = ";")

Bestandteile_K1DI1_komponente_clean <- Bestandteile_K1DI1_komponente %>%
  separate("X.ID_T1.ID_T2.ID_T5.ID_T6.ID_K1DI1", c("X", "ID_T01","ID_T02","ID_T05","ID_T06","ID_Motor"), sep = ";")

#renaming columns to fit all, selecting all K1DI1 Motoren
T_05_clean <- T_05_dirty %>%
  rename("ID_T05" = "ID_T05.x")


#relate all K1DI1 Motoren to Fahrzeuge (for ID Fahrzeuge(in fzg)) but fzg select first right ones

#change typeof to date
Typ11_fzg_dirty$Produktionsdatum <- ymd(Typ11_fzg_dirty$Produktionsdatum)

Typ11_fzg_clean <- Typ11_fzg_dirty %>%
  filter(Produktionsdatum >= as.Date("2010-09-21") & Produktionsdatum < as.Date("2012-09-04")) %>%
  filter(Fehlerhaft == 1)

Bestandteile_typ11_fzg_clean_K1D1 <- Bestandteile_typ11_fzg_clean %>% 
  filter(str_detect(ID_Motor, pattern = "K1DI1-"))

#merging bestandteile fzg clean with typ11fzg clean to get only with K1DI1 Motoren (no match)
K1DI1_fzg <- merge(x = Bestandteile_typ11_fzg_clean_K1D1, y = Typ11_fzg_clean, by = "ID_Fahrzeug")


#K1DI1 to relate to Einzeilteile
#take from bestandfzg (nur ausgefallene fzg und dazugehörige motoren) und innerjoin with bestandteile komp

komp_fzg <- merge(Bestandteile_K1DI1_komponente_clean, Bestandteile_typ11_fzg_clean_K1D1, by = "ID_Motor")
komp_fzg <- komp_fzg %>% select(c(ID_Motor, ID_T05:ID_T06, ID_Fahrzeug))



#gives einzelteile with right motoren and then have to merge those einzelteile with fehlerhaften einzelteile

T_05_fehler <- T_05_clean %>%
  filter(Produktionsdatum.x >= as.Date("2010-09-21") & Produktionsdatum.x < as.Date("2012-09-04")) %>%
  filter(Fehlerhaft.x == 1) %>% 
  select(c(ID_T05, Produktionsdatum.x, Fehlerhaft.x))

T_06_fehler <- T_06_dirty %>%
  filter(Fehlerhaft == 1) %>% 
  select(c(ID_T06, Fehlerhaft))

#merging T_05 does not work, dont need maybe cause its part of komponent
komp_fzg_einzeil <- merge(komp_fzg, T_06_fehler, by = "ID_T06")

write.csv(komp_fzg_einzeil, file = "FehlerFahrzeugeT05T06.csv")