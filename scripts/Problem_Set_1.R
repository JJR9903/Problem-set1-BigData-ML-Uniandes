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




str(GEIH_2018) # structure of dataset
head(GEIH_2018) # first 6 observations







##Filtrar por mayores de edad
names(GEIH_2018)

GEIH_2018<-GEIH_2018%>%
  filter(GEIH_2018$age>18)


skim(GEIH_2018)


age <- ggplot(GEIH_2018, aes(age)) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(age),color="Mean"), size=1)+
  geom_vline(aes(xintercept=mean(age)+sd(age),color="sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(age)-sd(age),color="sd"), linetype="dashed", size=1)+
  scale_color_manual(name= "age", values = c(Mean = "blue", sd = "red"))+
  theme_minimal()

plot_grid(age, estrato,labels = c("Age", "Estrato"), ncol = 2, nrow = 1)


## variables dummies 
GEIH_2018$dummy=1

sum_caract= summary(GEIH_2018%>%select(sex,clase,college,cotPension,cuentaPropia,formal,informal,inac,inac,ocu,pea,pet,wap,microEmpresa), digits = 4)

sex = GEIH_2018%>% 
  group_by(sex)%>%  
  count() %>% 
  ungroup() %>% 
  mutate(Porcentage = `n` / sum(`n`),
         Sexo = case_when(sex == 1 ~ "Mujeres", + sex == 0 ~ "Hombres"))%>%
  group_by(sex)%>%  
  mutate(etiquetas = paste(Sexo, toString(round(Porcentage*100, digits = 2)),"%"))

ggplot(sex, aes(x = 1, weight = Porcentage, fill = Sexo)) +
  geom_bar(width = 1,)+
  geom_text(aes(label = etiquetas, y=Porcentage ),position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y")+
  theme_void()
  
  
sum_Ing= summary(GEIH_2018%>%select(grep('^y|ie|ing|im|iof', names(GEIH_2018))), digits = 4)



#Seleccionar variable de Ingreso

GEIH_2018_ingreso <- GEIH_2018[,grep('^y|ie|ing|im|iof', names(GEIH_2018))]





######### 2. Age-earnings profile ##########
GEIH_2018$age2<-GEIH_2018$age^2
#Nos quedamos con Ingtot
 #Modelo 1.0
lm(ingtot~age+age2,GEIH_2018)


