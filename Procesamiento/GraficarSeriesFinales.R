# Libreria ====
library(openxlsx)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(ggplot2)
library(tidyverse)
library(fs)

wd_actual <- ""
folderSave <- ""

setwd(wd_actual)
docs <- fs::dir_ls();docs

# Aux Func ====
VH_HP <- function(filename){
  longText <- nchar(filename)
  if (substr(filename,1, 4) == "Real" ){
    InitName <- (substr(filename,1,12)); conjunto <- "pacientes reales"; modelName <- (substr(filename,6,12)); VH <- substr(filename,14,16); HP <- substr(filename,18,20)
  } else {
    InitName = substr(filename,1, 11); conjunto <- "pacientes simulados"; modelName <- (substr(filename,5,11)); VH <- substr(filename,13,15); HP <- substr(filename,17,19)
  }
  VH_min <- ifelse (VH == "VH2","30 minutos", 
                    ifelse (VH == "VH3","45 minutos", 
                            ifelse (VH == "VH4","60 minutos", "90 minutos")))
  HP_min <- ifelse (HP == "P15","15 minutos", "30 minutos")
  auxpng <- ifelse (HP == "P15","HP15.png", "HP30.png")
  returnList <- list("InitName" = InitName,"modelName"=modelName,
                     "VH" = VH, "VH_min" = VH_min,"HP" = HP, "HP_min" = HP_min,
                     "conjunto" = conjunto,"auxpng" = auxpng)
  return(returnList)
}


## Real ====
### Modelo 1 ====
# Mejor Escenario VP15
filename = "Real-Modelo1-VH6-P15.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >48 & Index<=(192+48)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(48, 240, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

# Mejor Escenario VP30
filename = "Real-Modelo1-VH3-P30.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >48 & Index<=(192+48)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(48, 240, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

### Modelo 2 ====
# Mejor Escenario VP15
filename = "Real-Modelo2-VH6-P15.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >48 & Index<=(192+48)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(48, 240, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

# Mejor Escenario VP30
filename = "Real-Modelo2-VH6-P30.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >48 & Index<=(192+48)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(48, 240, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()


### Modelo 3 ====
# Mejor Escenario VP15
filename = "Real-Modelo3-VH6-P15.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >48 & Index<=(192+48)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(48, 240, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

# Mejor Escenario VP30
filename = "Real-Modelo3-VH4-P30.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >48 & Index<=(192+48)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(48, 240, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

### Modelo 4 ====
# Mejor Escenario VP15
filename = "Real-Modelo4-VH6-P15.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >48 & Index<=(192+48)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(48, 240, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

# Mejor Escenario VP30
filename = "Real-Modelo4-VH6-P30.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >48 & Index<=(192+48)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(48, 240, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

### Modelo 5 ====
# Mejor Escenario VP15
filename = "Real-Modelo5-VH6-P15.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >48 & Index<=(192+48)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(48, 240, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

# Mejor Escenario VP30
filename = "Real-Modelo5-VH6-P30.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >48 & Index<=(192+48)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(48, 240, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()


### Modelo 6 ====
# Mejor Escenario VP15
filename = "Real-Modelo6-VH6-P15.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >48 & Index<=(192+48)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(48, 240, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

# Mejor Escenario VP30
filename = "Real-Modelo6-VH6-P30.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >48 & Index<=(192+48)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(48, 240, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

## Simulado ====
wd_actual <- "C:/Users/jpereira/OneDrive - tvcable.com.ec/RStudio/Est/2_TFM/Generados4-Modelos6/VentanaPredicciones_Simulados/"
### Modelo 1 ====
# Mejor Escenario VP15
filename = "Sim-Modelo1-VH4-P15.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >0 & Index<=(192)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(0, 192, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

# Mejor Escenario VP30
filename = "Sim-Modelo1-VH6-P30.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >0 & Index<=(192)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(0, 192, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()


### Modelo 2 ====
# Mejor Escenario VP15
filename = "Sim-Modelo2-VH6-P15.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >0 & Index<=(192)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(0, 192, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

# Mejor Escenario VP30
filename = "Sim-Modelo2-VH6-P30.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >0 & Index<=(192)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(0, 192, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

### Modelo 3 ====
# Mejor Escenario VP15
filename = "Sim-Modelo3-VH4-P15.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >0 & Index<=(192)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(0, 192, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

# Mejor Escenario VP30
filename = "Sim-Modelo3-VH4-P30.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >0 & Index<=(192)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(0, 192, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

### Modelo 4 ====
# Mejor Escenario VP15
filename = "Sim-Modelo4-VH6-P15.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >0 & Index<=(192)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(0, 192, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

# Mejor Escenario VP30
filename = "Sim-Modelo4-VH4-P30.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >0 & Index<=(192)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(0, 192, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()


### Modelo 5 ====
# Mejor Escenario VP15
filename = "Sim-Modelo5-VH4-P15.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >0 & Index<=(192)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(0, 192, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

# Mejor Escenario VP30
filename = "Sim-Modelo5-VH6-P30.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >0 & Index<=(192)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(0, 192, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()


### Modelo 6 ====
# Mejor Escenario VP15
filename = "Sim-Modelo6-VH4-P15.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >0 & Index<=(192)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(0, 192, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()

# Mejor Escenario VP30
filename = "Sim-Modelo6-VH6-P30.xlsx"
characteristics <- VH_HP(filename) 
auxname = characteristics$InitName; modeloName = characteristics$modelName; 
conjunto = characteristics$conjunto; auxpng = characteristics$auxpng
VH = characteristics$VH_min; HP = characteristics$HP_min
user <- readxl::read_excel(filename)
plot <- user %>%
  filter(Index >0 & Index<=(192)) %>%
  rename(Real = Y_test_real, `Predicción` = Y_test_pred) %>%
  gather(Tipo,Glucosa, Real, `Predicción`) %>%
  ggplot(aes(x=Index, y=Glucosa, colour=Tipo)) +
  geom_line(size = 0.6, alpha = 0.7)+
  labs(title=paste0(modeloName, " con ", conjunto),
       subtitle = paste0("Caso de estudio: ", "VH = ", VH, " y HP = ", HP," \nPeriodo = 2 días"))+
  labs(x = expression(bold(paste("t"," [horas]"))), 
       y = expression(bold(paste('Glucosa ', bgroup("[",over(mg, dL), "]")))))+
  scale_x_continuous(breaks = seq(0, 192, by = 24),
                     labels = seq(0, 48, by = 6))+ theme_bw() ;plot
name = paste0(folderSave,auxname,"-", auxpng);name
png(file=name, width=1920, height=1000,res=150, bg = "transparent"); print(plot); dev.off()
