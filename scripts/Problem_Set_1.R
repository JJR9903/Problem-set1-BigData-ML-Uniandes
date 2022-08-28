##### PROBLEM SET 1 - SCRIPT ##### 
## Authors: Juan José Rincón , .... 
## Description: development of 1 problem set of Big Data and Machine Leanring for applied economics course at Universidad de los Andes 2022-2
## Creation Date: 25/08/2022
## Modification Date: 25/08/2022
####################################

#### setting the work space 

rm(list=ls())

# este setwd por ahora no esta funcionando, toca que cada uno lo haga independiente mientras logro que funcione. att: Juan Jose
dir_set <- function(){
  if("/Users/JuanJose"%in%getwd()){
    setwd("/Users/JuanJose/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Uniandes/9 Semestre - 1 PEG/Big Data/Problems Set/Problem Set 1/GitHub")
  }
  else if("/Users/PC-portatil"%in%getwd()){
  setwd("")
  }
  else{
  setwd("C:/Users/Usuario/Documents/GitHub/Problem Set 1")
  }
}

dir_set()


pacman:: p_load(rvest, tidyverse, skimr, stargazer)


##### 1. Web Scraping data set (GEIH 2018 - Bogotá)
problem_set_URL <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",1:10,".html")

GEIH_2018<- data.frame()
for (url in problem_set_URL){
  print(url)
  df <- as.data.frame(read_html(url)%>%
                        html_table())
  GEIH_2018<-rbind(GEIH_2018,df)
  
}

# save data in RData and csv files
save(GEIH_2018, file = "stores/GEIH_2018.RData")
write.csv(GEIH_2018,file = "stores/GEIH_2018.csv",fileEncoding = "UTF-8")


#load GEIH_2018
load("stores/GEIH_2018.RData")


##Filtrar por mayores de edad
names(GEIH_2018)

GEIH_2018<-GEIH_2018%>%
  filter(GEIH_2018$age>18)

#Seleccionar variable de Ingreso

GEIH_2018_ingreso <- GEIH_2018[,grep('^y', names(GEIH_2018))]
GEIH_2018_ingreso2 <- GEIH_2018[,grep('^ie', names(GEIH_2018))]
GEIH_2018_ingreso3 <- GEIH_2018[,grep('^ing', names(GEIH_2018))]
GEIH_2018_ingreso4 <- GEIH_2018[,grep('^im', names(GEIH_2018))]
GEIH_2018_ingreso5 <- GEIH_2018[,grep('^iof', names(GEIH_2018))]

GEIH_ingreso<- cbind(GEIH_2018_ingreso,GEIH_2018_ingreso2,GEIH_2018_ingreso3,GEIH_2018_ingreso4,GEIH_2018_ingreso5)
names(GEIH_ingreso)
skim(GEIH_ingreso)

GEIH_2018$age2<-GEIH_2018$age^2
#Nos quedamos con Ingtot
 #Modelo 1.0
lm(ingtot~age+age2,GEIH_2018)


