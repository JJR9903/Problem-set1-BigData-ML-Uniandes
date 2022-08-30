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
  if(Sys.info()["user"]=="JuanJose"){
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


pacman:: p_load(rvest, tidyverse, skimr, stargazer,cowplot)


##### 1.a Web Scraping data set (GEIH 2018 - Bogotá) ####
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

##### 1.a.2  Describe data #####

#Loading data without web Scraping step
load("stores/GEIH_2018.RData")













##Filtrar por mayores de edad
names(GEIH_2018)

GEIH_2018<-GEIH_2018%>%
  filter(GEIH_2018$age>18)


skim(GEIH_2018)

par(mfrow=c(2,2))
hist(GEIH_2018$age, main="Age of surveyed people distribution", col= '#28BFE8',xlab = "Age")
hist(GEIH_2018$sex, main="Distribución de la edad de los encuestados",col= '#28BFE8',xlab = "sexo")
hist(GEIH_2018$clase, main="Distribución de la edad de los encuestados",col= '#28BFE8',xlab = "Zona")
hist(GEIH_2018$age, main="Distribución de la edad de los encuestados")+  
  theme_minimal()


age <- ggplot(GEIH_2018, aes(age)) +
  geom_histogram(fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(age),color="Mean"), size=1)+
  geom_vline(aes(xintercept=mean(age)+sd(age),color="sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(age)-sd(age),color="sd"), linetype="dashed", size=1)+
  scale_color_manual(name= "age", values = c(Mean = "blue", sd = "red"))+
  theme_minimal()

estrato <- ggplot(GEIH_2018, aes(estrato1)) +
  geom_histogram(fill = "#28BFE8", color = "white",binwidth=1) + 
  geom_vline(aes(xintercept=mean(estrato1),color="Mean"), size=1)+
  geom_vline(aes(xintercept=mean(estrato1)+sd(estrato1),color="sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(estrato1)-sd(estrato1),color="sd"), linetype="dashed", size=1)+
  scale_color_manual(name= "Estrato", values = c(Mean = "blue", sd = "red"))+
  theme_minimal()


plot_grid(age, estrato,labels = c("Age", "Estrato"), ncol = 2, nrow = 1)

table(GEIH_2018$estrato1)

#verificar las variables dicotomas
range(GEIH_2018$clase)
range(GEIH_2018$sex)


#Seleccionar variable de Ingreso

GEIH_2018_ingreso <- GEIH_2018[,grep('^y|ie|ing|im|iof', names(GEIH_2018))]





######### 2. Age-earnings profile ##########
GEIH_2018$age2<-GEIH_2018$age^2
#Nos quedamos con Ingtot
 #Modelo 1.0
lm(ingtot~age+age2,GEIH_2018)


