# Librerías ====
library(tidyverse)
library(ggplot2)
library(ega)
library(fs)

# Establecer Directorio ====
wd_actual <- " "
setwd(wd_actual)

Zones=c('A','B','C','D','E')
ZonesModels <- as.data.frame(Zones)

fun_VH <- function(VH, HP){
  VH2 = ifelse(VH == "VH2","VH30",
               ifelse(VH == "VH3","VH45",
                      ifelse(VH == "VH4","VH60","VH90")))
  HP2 = ifelse(HP == "P15","HP15","HP30")
  
  listReturn <- c(VH2, HP2)
  return(listReturn)
}

## Modelos ====

archivosDir <- fs::dir_ls(regexp = ".xlsx")
folderExport = "Graficas_Clarke"
for (filename in archivosDir) {
  print(filename)
  
  if (substr(filename,1, 4) == "Real" ){
    tipo = substr(filename,1, 4)
    name <- substr(filename,6,20)
    modelName <- substr(filename,6,12)
    VH <- substr(filename,14,16); PH <- substr(filename,18,20)
  } else {
    tipo = substr(filename,1, 3)
    name <- substr(filename,5,19)
    modelName <- substr(filename,5,11)
    VH <- substr(filename,13,15); PH <- substr(filename,17,19)
  }
  newName <- paste0(modelName, "_", (fun_VH(VH,PH)[1]), "_", (fun_VH(VH,PH)[2]), "_", tipo); 
  print(newName)
  
  user <- readxl::read_excel(filename)
  zones <- getClarkeZones (user$Y_test_real / 18, user$Y_test_pred / 18, unit="mol")
  zones <- round (table (zones) / length (zones) * 100, digits=2)
  zones <- as.data.frame(zones)
  colnames(zones)<- c('Zones', newName); zones
  ZonesModels <- left_join(ZonesModels, zones)
  
  # Clarke PNG
  filenamePNG <- paste0(substr(filename,1, nchar(filename)-5),".png")
  name = paste0(wd_actual,"/", folderExport, "/", filenamePNG); name
  
  plot <- plotClarkeGrid(user$Y_test_real, user$Y_test_pred,
                         title = "", #"Clarke Error Grid"
                         xlab = "", ylab = "", linesize = 0.8, linetype = "solid",
                         linecolor = "black", linealpha = 0.6, pointsize = 1, pointalpha = 1,
                         zones = NA, unit = "gram") 
  
  #name = paste0(wd_actual,"/", folderExport, "/", "try.png"); name
  png(file=name, width=1100, height=689,res = 150)
  print(plot)
  dev.off()
  
}

ZonesModels[is.na(ZonesModels)] <- 0

writexl::write_xlsx(ZonesModels,"Graficas_Clarke/Porcentajes.xlsx")