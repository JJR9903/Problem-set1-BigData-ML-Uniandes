##### PROBLEM SET 1 - SCRIPT ##### 
## Authors: Juan José Rincón , Juan Andres Ospina, Juanita Chacón 
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
  setwd("C:/Users/Usuario/Documents/GitHub/Problem-set1-BigData-ML-Uniandes/stores")
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


#### Data Cleaning ####

str(GEIH_2018) # structure of dataset
head(GEIH_2018) # first 6 observations

##Filtrar por mayores de edad
GEIH_2018<-GEIH_2018%>%
  filter(GEIH_2018$age>18)

# eliminamos variables que salen tal cual como en la encuesta, pues hay otras variables construidas a partir de estas
# con esto reducimos el tamaño de la base de 174 variables a 73  
GEIH_2018 = subset(GEIH_2018, select = -c(grep('^p6|p5|p7|cc|fex', names(GEIH_2018))) )
GEIH_2018 = subset(GEIH_2018, select = -c(directorio,fweight,Var.1,mes,orden,secuencia_p) )

#revisamos la base de datos
skim=skim(GEIH_2018)
#eliminamos variables que no sirven por tener solo el 20% de complete rate 
skim=skim[skim$complete_rate > 0.2, ]
skim = as.list(subset(skim, select =skim_variable))
GEIH_2018 = subset(GEIH_2018, select = skim[["skim_variable"]] )

# verificamos ciertas variables que por los filtros solo deberían tener una opcion (depto=11, dominio=BOGOTA,clase==1) y las eliminamos
sum(GEIH_2018$depto==11)==nrow(GEIH_2018)
sum(GEIH_2018$dominio=="BOGOTA")==nrow(GEIH_2018)
sum(GEIH_2018$clase==1)==nrow(GEIH_2018)
GEIH_2018 = subset(GEIH_2018, select = -c(dominio,depto,clase))

skim=skim(GEIH_2018)


#### Verificar los tipos de las variables ####

# variables categoricas
cats = c("sex", "maxEducLevel","college","cotPension","cuentaPropia","dsi","estrato1","formal","ina","inac","informal","microEmpresa","ocu","oficio","pea","pet","regSalud","relab","wap","sizeFirm")
for (v in cats) {
  GEIH_2018[, v] <- as.factor(GEIH_2018[, v, drop = T])
}

## revisar variables categoricas que dicen lo mismo
table(GEIH_2018$ina)
table(GEIH_2018$inac)	
table(GEIH_2018$pea)	
GEIH_2018$ina<-NULL
GEIH_2018$inac<-NULL

table(GEIH_2018$formal)	
table(GEIH_2018$informal)	
GEIH_2018$informal<-NULL

skim=skim(GEIH_2018)

#### descripción de los datos ####

### Descripción de variables categoricas ###
GEIH_2018$dummy=1

sexo_PieG <- ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = sex),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("0"="#28BFE8" , "1"="#FA8072"), 
                    label = c("0"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['sex'])['1']) / sum(as.numeric(table(GEIH_2018['sex']))),2)),"%","Hombres")
                             ,"1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['sex'])['0']) / sum(as.numeric(table(GEIH_2018['sex']))),2)),"%"),"Mujeres") , 
                    name = "Sexo")+
  theme_void()



estrato1 <-ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = estrato1),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("1"="#F5B7B1" , "2"="#E8DAEF", "3"="#85C1E9","4"="#73C6B6","5"="#F8C471","6"="#BA4A00"), 
                    label = c("1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['1']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estatro 1")
                              ,"2"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['2']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estatro 2")
                              ,"3"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['3']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estatro 3")
                              ,"4"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['4']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estatro 4")
                              ,"5"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['5']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estatro 5")
                              ,"6"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['6']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estatro 6")
                              ) , 
                    name = "Sexo")+
  theme_void()



### Descripción de variables numericas (NO de ingreso) ### 


### Descripción de variables de Ingreso ## 
# comparación entre variables que dicen cosas parecidas 
# se escogen algunas y de esas se comparan por edad y sexo 

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

#Ver n�mero de missing values
skim(GEIH_2018_ingreso)




#### Estandarización de los datos ####

GEIH_2018_scaled = as.data.frame(scale(GEIH_2018, center = TRUE, scale = TRUE))
skim_scaled=skim(GEIH_2018_scaled)

      ######### 2. Age-earnings profile ##########

GEIH_2018$age2<-GEIH_2018$age^2
#Nos quedamos con Ingtot
 #Modelo 1.0
model1<-lm(ingtot~age+age2,GEIH_2018)



    ######### 3. Age-earnings profile ##########


#Ln de Ingtot 
GEIH_2018$lning<-log(GEIH_2018$ingtot)
GEIH_2018$lning[which(!is.finite(GEIH_2018$lning))] <- 0
GEIH_2018$lning


#MODELO 2.0
#0 es mujer 
model2<-lm(lning~sex==0,GEIH_2018)



