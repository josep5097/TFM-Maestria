# Libreria ====
library(openxlsx)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(ggplot2)
library(tidyverse)
library(fs)

# Información ====
wd_actual <- ""
setwd(wd_actual)
set.seed(12345)
rawData <- read_csv("Glucose_measurements_sample.csv",
                    col_types = cols(Measurement_date = col_date(format = "%Y-%m-%d"), 
                                     Measurement_time = col_time(format = "%H:%M:%S")))
View(Glucose_measurements_sample)

# Extracción ====
summarySample <- rawData %>% group_by(Patient_ID,Measurement_date)%>% mutate(Registros=n()) %>% 
  select(-c(Measurement_time, Measurement))
summarySample <- summarySample[!duplicated(summarySample), ]

summarySample <- summarySample %>% mutate(Valido = ifelse(Registros >= 93,1,0)) 

summarySample <- summarySample %>% ungroup() %>% group_by(Patient_ID) %>% mutate(ValidosAcum = cumsum(Valido))
summarySample2 <- summarySample %>% ungroup() %>% group_by(Patient_ID,Valido) %>% mutate(numValidos = n())
summarySample2 <- summarySample2 %>% select(-c(Measurement_date,Registros,ValidosAcum))
summarySample2 <- summarySample2[!duplicated(summarySample2), ]

summarySample3 <- summarySample2 %>% pivot_wider(names_from = "Valido",values_from = "numValidos")
summarySample3 <- summarySample3 %>% mutate(diff = `1`- `0`)
summarySample4 <- summarySample3 %>% filter(diff>=15)
summarySample4 <- summarySample4 %>% mutate(Porc = `0` / `1`)
summarySample5 <- summarySample4 %>% filter(diff>30 & Porc <=0.25)


# Muestreo ====

userIDs <- summarySample5 %>% select((Patient_ID)) %>% unique()
userIDs <- userIDs$Patient_ID

userIDs_sample <- sample(userIDs, 10, replace=TRUE)
userIDs_sample

samples <- rawData %>% filter(Patient_ID %in% userIDs)
samples %>% select((Patient_ID)) %>% unique()

summarySamples <- samples %>% group_by(Patient_ID) %>% mutate(Registros=n(),
                                                             minFechaReg = min(Measurement_date),
                                                             maxFechaReg = max(Measurement_date),
                                                             dias = maxFechaReg-minFechaReg,
                                                             minGlucosa = min(Measurement),
                                                             maxGlucosa = max(Measurement),
                                                             meanGlucosa = mean(Measurement)) %>% 
  select(-c(Measurement_date,Measurement_time,Measurement))
summarySamples <- summarySamples[!duplicated(summarySamples), ]

summarySamples <- samples %>% group_by(Patient_ID,Measurement_date)%>% mutate(Registros=n()) %>% 
  select(-c(Measurement_time, Measurement))
summarySamples <- summarySamples[!duplicated(summarySample), ]