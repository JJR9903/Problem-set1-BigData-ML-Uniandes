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
  filter(GEIH_2018$age>18 & GEIH_2018$age<65)
  filter(GEIH_2018$age<65)

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
table(GEIH_2018$formal)	
table(GEIH_2018$informal)
summary(GEIH_2018$hoursWorkUsual)
summary(GEIH_2018$totalHoursWorked)


GEIH_2018 <- GEIH_2018 %>% subset(select=-c(hoursWorkUsual,informal,inac,ina))
skim=skim(GEIH_2018)

#### descripción de los datos ####

### Descripción de variables categoricas ###
GEIH_2018$dummy=1

sexo_PG <- ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = sex),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("0"="#28BFE8" , "1"="#FA8072"), 
                    label = c("0"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['sex'])['1']) / sum(as.numeric(table(GEIH_2018['sex']))),2)),"%","Hombres")
                             ,"1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['sex'])['0']) / sum(as.numeric(table(GEIH_2018['sex']))),2)),"%","Mujeres")
                             ) , name = "")+
  labs(title = "Sexo")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))
  



estrato_PG <-ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = estrato1),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("1"="#F5B7B1" , "2"="#E8DAEF", "3"="#85C1E9","4"="#73C6B6","5"="#F8C471","6"="#BA4A00"), 
                    label = c("1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['1']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estrato 1")
                              ,"2"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['2']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estrato 2")
                              ,"3"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['3']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estrato 3")
                              ,"4"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['4']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","  Estrato 4")
                              ,"5"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['5']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","  Estrato 5")
                              ,"6"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['6']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","  Estrato 6")
                              ) , name = "")+
  labs(title = "Estrato de la vivienda")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))

NivelEducativo_BG <- ggplot(data=GEIH_2018) +
  geom_bar(stat="identity",aes(y=dummy, x=maxEducLevel, fill = maxEducLevel), width = 0.5)+
  scale_fill_manual( values = c("1"="#9b5fe0" , "2"="#16a4d8", "3"="#60dbe8","4"="#8bd346","5"="#efdf48","6"="#f9a52c","7"="#d64e12"),
                    label = c("1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['1']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%"," Ninguno")
                              ,"2"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['2']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%"," Preescolar")
                              ,"3"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['3']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%"," Primaria incompelta")
                              ,"4"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['4']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Primaria completa")
                              ,"5"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['5']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Secundaria incompleta")
                              ,"6"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['6']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Secundaria completa")
                              ,"7"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['7']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Superior o universitaria")
                    ) , 
                    name = "")+
  labs(title = "Máximo Nivel Educativo alcanzado")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))

GEIH_2018$regSalud_nan=GEIH_2018$regSalud
GEIH_2018$regSalud_nan<-as.character(GEIH_2018$regSalud_nan)
GEIH_2018$regSalud_nan[is.na(GEIH_2018$regSalud_nan)] <- as.character(0)
GEIH_2018$regSalud_nan<-as.factor(GEIH_2018$regSalud_nan)

regSalud_PG <- ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = regSalud_nan),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("0"="#9D9B92","1"="#56ba5a" , "2"="#cb3747","3"="#0cc6b8"), 
                    label = c("0"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['regSalud_nan'])['0']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","  Nan")
                              ,"1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['regSalud_nan'])['1']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Regimen Contributivo")
                              ,"2"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['regSalud_nan'])['2']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","  Regimen Especial") 
                              ,"3"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['regSalud_nan'])['3']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Regimen Subsidiado") 
                    ) ,
                    name = "")+
  labs(title = "Regimen de Seguridad Social en Salud")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))


GEIH_2018$cotPension_nan=GEIH_2018$cotPension
GEIH_2018$cotPension_nan<-as.character(GEIH_2018$cotPension_nan)
GEIH_2018$cotPension_nan[is.na(GEIH_2018$cotPension_nan)] <- as.character(0)
GEIH_2018$cotPension_nan<-as.factor(GEIH_2018$cotPension_nan)

cotPension_PG <- ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = cotPension_nan),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("0"="#9D9B92","1"="#439790" , "2"="#fd6767","3"="#7a9c48"), 
                    label = c("0"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['cotPension_nan'])['0']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Nan") 
                              ,"1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['cotPension_nan'])['1']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Cotiza a pension")
                              ,"2"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['cotPension_nan'])['2']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","No cotiza a pension") 
                              ,"3"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['cotPension_nan'])['3']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","  Pensionado") 
                             
                    ) ,
                    name = "",na.translate=T)+
  labs(title = "Estado Pensional")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))

GEIH_2018$relab_nan=GEIH_2018$relab
GEIH_2018$relab_nan<-as.character(GEIH_2018$relab_nan)
GEIH_2018$relab_nan[is.na(GEIH_2018$relab_nan)] <- as.character(0)
GEIH_2018$relab_nan<-as.factor(GEIH_2018$relab_nan)

relab_BG <- ggplot(data=GEIH_2018) +
  geom_bar(stat="identity",aes(y=dummy, x=relab_nan, fill = relab_nan), width = 0.5)+
  scale_fill_manual( values = c("0"="#9D9B92","1"="#f6993f" , "2"="#ffed4a", "3"="#38c172","4"="#4dc0b5","5"="#3490dc","6"="#6574cd","7"="#9561e2","8"="#f66d9b","9"="656950"),
                     label = c("0"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['relab_nan'])['0']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%"," Nan")
                               ,"1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['relab_nan'])['1']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","  Obrero o empleado de empresa particular")
                               ,"2"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['relab_nan'])['2']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","  Obrero o empleado del gobierno")
                               ,"3"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['relab_nan'])['3']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","  Empleado doméstico")
                               ,"4"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['relab_nan'])['4']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Trabajador por cuenta propia")
                               ,"5"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['relab_nan'])['5']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","   Patron o empleadora")
                               ,"6"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['relab_nan'])['6']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%"," Trabajador familiar sin remuneracion")
                               ,"7"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['relab_nan'])['7']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%"," Trabajador sin remuneracin en empresas o negocios de otros hogares")
                               ,"8"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['relab_nan'])['8']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","      Jornalero o peon")
                               ,"9"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['relab_nan'])['9']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%"," Otro")
                     ) , 
                     name = "")+
  labs(title = "Relación Laboral")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))


categorical_plot<- plot_grid(sexo_PG, estrato_PG,regSalud_PG,cotPension_PG,NivelEducativo_BG ,relab_BG, ncol = 2, nrow = 3)
ggsave("views/categorical_plot.png", width = 70, height = 50, units="cm",plot = categorical_plot)

#hace falta oficio y sizefirm (ver que hacer con ellas que no son muy indicativas) y las dummies 

### Descripción de variables numericas (NO de ingreso) ### 

# age y hoursWorkUsual
age <- ggplot(GEIH_2018, aes(x=age)) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(age),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(age)+sd(age),color="+sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(age)-sd(age),color="-sd"), linetype="dashed", size=1)+
  scale_color_manual(name= "", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222") 
                    , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$age)+sd(GEIH_2018$age),2)),"= -1 sd"),
                               "Media"=paste(as.character(round(mean(GEIH_2018$age),2)),"= media"),
                               "+sd"=paste(as.character(round(mean(GEIH_2018$age)+sd(GEIH_2018$age),2)),"= +1 sd")
                               ))+
  labs(title="Edad",x="Edad",y="densidad")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))


totalHoursWorked <- ggplot(GEIH_2018, aes(x=totalHoursWorked)) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(age)+sd(age),color="-sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(age),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(age)-sd(age),color="+sd"), linetype="dashed", size=1)+
  scale_color_manual(name= "", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222") 
                     , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$totalHoursWorked,na.rm = T)+sd(GEIH_2018$totalHoursWorked,na.rm = T),2)),"= -1 sd"),
                                "Media"=paste(as.character(round(mean(GEIH_2018$totalHoursWorked,na.rm = T),2)),"= media"),
                                "+sd"=paste(as.character(round(mean(GEIH_2018$totalHoursWorked,na.rm = T)+sd(GEIH_2018$totalHoursWorked,na.rm = T),2)),"= +1 sd")
                     ))+
  labs(title="Horas trabajadas a la semana",x="",y="densidad")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))


numerical_plot<- plot_grid(age, totalHoursWorked, ncol = 2, nrow = 1)
ggsave("views/numerical_plot.png", width = 70, height = 25, units="cm",plot = numerical_plot)


table(GEIH_2018$estrato1,GEIH_2018$maxEducLevel)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(GEIH_2018, row.vars = "estrato1", col.vars = "maxEducLevel", type = "f")
crosstab(GEIH_2018, row.vars = "estrato1", col.vars = "maxEducLevel", type = "j")

### Descripción de variables de Ingreso ## 
# comparación entre variables que dicen cosas parecidas 
# se escogen algunas y de esas se comparan por edad y sexo 

  # y_salary_m - principal occ.
salary_hist <- ggplot(GEIH_2018, aes(x=y_salary_m/(1000000))) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(y_salary_m/(1000000),na.rm = T)+sd(y_salary_m/(1000000),na.rm = T),color="-sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(y_salary_m/(1000000),na.rm = T),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(y_salary_m/(1000000),na.rm = T)-sd(y_salary_m/(1000000),na.rm = T),color="+sd"), linetype="dashed", size=1)+
  scale_color_manual(name= "", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222") 
                     , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$y_salary_m/(1000000),na.rm = T)+sd(GEIH_2018$y_salary_m/(1000000),na.rm = T),2)),"= -1 sd"),
                                "Media"=paste(as.character(round(mean(GEIH_2018$y_salary_m/(1000000),na.rm = T),2)),"= media"),
                                "+sd"=paste(as.character(round(mean(GEIH_2018$y_salary_m/(1000000),na.rm = T)+sd(GEIH_2018$y_salary_m/(1000000),na.rm = T),2)),"= +1 sd")
                     ))+
  labs(title = "Salario actividad principal",x="Salario actividad principal (mensual)",y="densidad",caption="En millones")+
  xlim(-1,40)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))


salary_point <- ggplot(GEIH_2018, aes(y=y_salary_m/(1000000),x=age, color="#1da2d8")) +
  geom_point()+
  labs(title = "Salario actividad principal",y="Salario actividad principal (mensual)",x="Edad",caption="En millones")+
  theme_minimal()+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=14,face="bold"))


  #y_ingLab_m - all occ. 
ingLab_hist <- ggplot(GEIH_2018, aes(x=y_ingLab_m/(1000000))) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(y_ingLab_m/(1000000),na.rm = T)+sd(y_ingLab_m/(1000000),na.rm = T),color="-sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(y_ingLab_m/(1000000),na.rm = T),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(y_ingLab_m/(1000000),na.rm = T)-sd(y_ingLab_m/(1000000),na.rm = T),color="+sd"), linetype="dashed", size=1)+
  scale_color_manual(name= " ", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222") 
                     , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$y_ingLab_m/(1000000),na.rm = T)+sd(GEIH_2018$y_ingLab_m/(1000000),na.rm = T),2)),"= -1 sd"),
                                "Media"=paste(as.character(round(mean(GEIH_2018$y_ingLab_m/(1000000),na.rm = T),2)),"= media"),
                                "+sd"=paste(as.character(round(mean(GEIH_2018$y_ingLab_m/(1000000),na.rm = T)+sd(GEIH_2018$y_ingLab_m/(1000000),na.rm = T),2)),"= +1 sd")
                     ))+
  labs(title = "Salario laboral",x="Salario laboral (mensual)",y="densidad",caption="En millones")+
  xlim(-1,40)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))


ingLab_point <- ggplot(GEIH_2018, aes(y=y_ingLab_m/(1000000),x=age, color="#1da2d8")) +
  geom_point()+
  labs(title = "Salario laboral",y="Salario laboral (mensual)",x="Edad", caption="En millones")+
  theme_minimal()+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=14,face="bold"))
  

  
  #y_total_m - income salaried + independents total
y_total_hist <- ggplot(GEIH_2018, aes(x=y_total_m/(1000000))) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(y_total_m/(1000000),na.rm = T)+sd(y_total_m/(1000000),na.rm = T),color="-sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(y_total_m/(1000000),na.rm = T),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(y_total_m/(1000000),na.rm = T)-sd(y_total_m/(1000000),na.rm = T),color="+sd"), linetype="dashed", size=1)+
  scale_color_manual(name="", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222") 
                     , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$y_total_m/(1000000),na.rm = T)+sd(GEIH_2018$y_total_m/(1000000),na.rm = T),2)),"= -1 sd"),
                                "Media"=paste(as.character(round(mean(GEIH_2018$y_total_m/(1000000),na.rm = T),2)),"= media"),
                                "+sd"=paste(as.character(round(mean(GEIH_2018$y_total_m/(1000000),na.rm = T)+sd(GEIH_2018$y_total_m/(1000000),na.rm = T),2)),"= +1 sd")
                     ))+
  labs(title= "Salario + ingreso independientes",x="Salario + ingreso independientes (mensual)",y="densidad",caption="En millones")+
  xlim(-1,40)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))


y_total_point <- ggplot(GEIH_2018, aes(y=y_total_m/(1000000),x=age, color="#1da2d8")) +
  geom_point()+
  labs(title= "Salario + ingreso independientes",y="Salario + ingreso independientes (mensual)",x="Edad",caption="En millones")+
  theme_minimal()+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=14,face="bold"))



  
  #ingtot - 
ingtot_hist <- ggplot(GEIH_2018, aes(x=ingtot/(1000000))) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(ingtot/(1000000),na.rm = T)+sd(ingtot/(1000000),na.rm = T),color="-sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(ingtot/(1000000),na.rm = T),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(ingtot/(1000000),na.rm = T)-sd(ingtot/(1000000),na.rm = T),color="+sd"), linetype="dashed", size=1)+
  scale_color_manual(name= " ", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222") 
                     , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$ingtot/(1000000),na.rm = T)+sd(GEIH_2018$ingtot/(1000000),na.rm = T),2)),"= -1 sd"),
                                "Media"=paste(as.character(round(mean(GEIH_2018$ingtot/(1000000),na.rm = T),2)),"= media"),
                                "+sd"=paste(as.character(round(mean(GEIH_2018$ingtot/(1000000),na.rm = T)+sd(GEIH_2018$ingtot/(1000000),na.rm = T),2)),"= +1 sd")
                     ))+
  labs(title= "Ingreso total",x="Ingreso total (mensual)",y="densidad",caption="En millones")+
  xlim(-2,40)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))


ingtot_point <- ggplot(GEIH_2018, aes(y=ingtot/(1000000),x=age, color="#1da2d8")) +
  geom_point()+
  labs(title="Ingreso total",y="Ingreso total (mensual)",x="Edad", caption="En millones")+
  theme_minimal()+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=14,face="bold"))




y_hist<-plot_grid(salary_hist,ingLab_hist,y_total_hist,ingtot_hist, ncol = 2, nrow = 2)
y_point<-plot_grid(salary_point,ingLab_point,y_total_point,ingtot_point, ncol = 2, nrow = 2)


ggsave("views/y_hist.png", width = 70, height = 50, units="cm",plot = y_hist)
ggsave("views/y_point.png", width = 70, height = 50, units="cm",plot = y_point)




summary(GEIH_2018$y_salary_m)
summary(GEIH_2018$y_ingLab_m)
summary(GEIH_2018$y_total_m)
summary(GEIH_2018$ingtot)

  
GEIH_2018<- subset(GEIH_2018, filter=-c(relab_nan,regSalud_nan,cotPension_nan,dummy))
  
  
  #### escogemos 2 preliminarmente y_ingLab_m y y_total_m
  
GEIH_2018$scaled_y<-scale(GEIH_2018$y_total_m)


ing <- model.matrix(as.formula("y_total_m ~ a + b + c"), mydf)
  
GEIH_2018_scaled = as.data.frame(scale(GEIH_2018, center = TRUE, scale = TRUE))

#### Estandarización de los datos ####

GEIH_2018_scaled = as.data.frame(scale(GEIH_2018, center = TRUE, scale = TRUE))
skim_scaled=skim(GEIH_2018_scaled)

      ######### 2. Age-earnings profile ##########

GEIH_2018$age2<-GEIH_2018$age^2
#Nos quedamos con y_total_m y la estandarizamos
GEIH_2018$scaled_y<-scale(GEIH_2018$y_total_m)
summary(GEIH_2018$scaled_y)


#por sexo para llenar missings

GEIH_2018$sexualizandoando<-0
GEIH_2018$sexo<-as.numeric(GEIH_2018$sex)


ExcelFunctionsR::AVERAGEIFS(GEIH_2018$scaled_y,GEIH_2018$sexo,1)
ExcelFunctionsR::AVERAGEIFS(GEIH_2018$scaled_y,GEIH_2018$sexo,2)


GEIH_2018<-GEIH_2018%>%
  mutate(sexualizandoando=case_when((is.na(scaled_y)==TRUE & sexo==1)~1.511169,
                            (is.na(scaled_y)==TRUE & sexo==2)~1.103506,
                            TRUE ~ 0))

GEIH_2018$scaled_y[is.na(GEIH_2018$scaled_y)]<-GEIH_2018$sexualizandoando


 #Modelo 1.0
model1<-lm(scaled_y~age+age2,GEIH_2018)

#tabla

stargazer(model1,title="", out=file.path(getwd(),"/stores/model1.txt"),out.header = T)
install.packages("car")
library("car")
plot(scaled_y~age, data=GEIH_2018)
lines(lowess("GEIH_2018"),col="blue")

lowess_b <- lowess(GEIH_2018$age, GEIH_2018$scaled_y)
plot( lowess_b, type="c")


plot(GEIH_2018$age, GEIH_2018$y_total_m)
lines(lowess(GEIH_2018$age, GEIH_2018$y_total_m),col='red')  

installed.packages(ggplot)
ggplot(data=GEIH_2018,aes(x=age,y=y_total_m))+geom_point()+geom_smooth(method = "lm")

##Intervalos 
#Guardar betas 

Coef1<- model1$coef
Coef
b0<-Coef1[1]
b1<-Coef1[2]
b2<-Coef1[3]

#Derivando para encontrar el puto mx me interesa es b1 y b2 

require("boot")

#Defini la función que voy a usar 
eta.mod1.fn<-function(data,index,age_var=mean(GEIH_2018$age), age2_var=mean (GEIH_2018$age2)){
f<-lm(scaled_y~age+age2,data=data,subset =index )
coefs<-f$coefficients
b1<-coefs[2]
b2<-coefs[3]
opt<-(-b1/(2*b2))
return(opt)
}

eta.mod1.fn(GEIH_2018,1:1000)


##desviación e intervalo 
set.seed(50)
eta.mod1.fn<-function(data,index,age_var=mean(GEIH_2018$age), age2_var=mean (GEIH_2018$age2)){
  f<-lm(scaled_y~age+age2,data=data,subset =index )
  coefs<-f$coefficients
  b1<-coefs[2]
  b2<-coefs[3]
  opt<-(-b1/(2*b2))
  return(opt)
}
  
bootcor<- boot (GEIH_2018, eta.mod1.fn,R=1000)
summary (bootcor)

lb<-44.572 -((1.96*0.60077)/sqrt(1000))
lb
######### 3. Age-earnings profile ##########


#Ln de Ingtot 
GEIH_2018$lning<-log(GEIH_2018$y_total_m)
GEIH_2018$lning

summary(GEIH_2018$lning)
#########Arregloooooooooo

vista<-GEIH_2018%>%
  select(y_total_m,maxEducLevel,estrato1)

vista<-vista%>%
  drop_na()

promedios<-tapply(vista$y_total_m,vista$maxEducLevel,mean)

lnpromedios<-log(promedios)

GEIH_2018$nuevoln<-1

GEIH_2018<-GEIH_2018%>%
  mutate(nuevoln=case_when(n_edu==1 ~ 13.27868,
                           n_edu==2 ~ 0,
                           n_edu==3 ~ 13.57817,
                           n_edu==4 ~ 13.67374,
                           n_edu==5 ~ 13.69189,
                           n_edu==6 ~ 13.85868,
                           n_edu==7 ~ 14.75243,
                    TRUE~0))

GEIH_2018$lning[is.na(GEIH_2018$lning)]<-GEIH_2018$nuevoln
summary(GEIH_2018$lning)

#MODELO 2.0
#0 es mujer 

model2<-lm(lning~sex,GEIH_2018)

##grafica 
plot(scaled_y~age, data=GEIH_2018,group=sex, color=sex)
lines(lowess("GEIH_2018"),col="blue")

##Toca hacer las graficas y el intervalos!!!

##intervalo con boot 
set.seed(50)
eta.mod1.fn<-function(data,index,age_var=mean(GEIH_2018$age), age2_var=mean (GEIH_2018$age2)){
  f<-lm(lning~sex,data=data,subset =index )
  coefs<-f$coefficients
  b1<-coefs[2]
  b2<-coefs[3]
  opt<-(-b1/(2*b2))
  return(opt)
}

bootcor<- boot (GEIH_2018, eta.mod1.fn,R=1000)
summary (bootcor)

##MODELO CON CONTROLES 
edad_educacion<-(GEIH_2018$n_edu* GEIH_2018$age)
edad_sexo<-(GEIH_2018$age*GEIH_2018$sexo)
edad_estrato<-(GEIH_2018$age*GEIH_2018$estrato)

model3<-lm(lning~sex+age+estrato1+maxEducLevel+oficio+edad_educacion+edad_sexo+edad_estrato,GEIH_2018)

##MODELO CON WFL 
require("tidyverse")
require("fabricatr")

reg1<-lm(lning~sex+age+estrato1+maxEducLevel+oficio+edad_educacion+edad_sexo+edad_estrato,GEIH_2018)
stargazer(reg1,type="text")

GEIH_2018$res_y_a<-1
GEIH_2018$res_s_a<-1

GEIH_2018<-GEIH_2018%>% 
  mutate(res_y_a=lm(lning~age+estrato1+maxEducLevel+oficio+edad_educacion+edad_sexo+edad_estrato,GEIH_2018)$residuals,
         res_s_a=lm(sex~age+estrato1+maxEducLevel+oficio+edad_educacion+edad_sexo+edad_estrato,GEIH_2018)$residuals,
         )

reg2<-lm(res_y_a~res_s_a-1,GEIH_2018)
stargazer(reg1,reg2,type="text")


#MODELO CON BOOT ##sacar el error 
set.seed(50)
eta.mod6.fn<-function(data,index,age_var=mean(GEIH_2018$age), age2_var=mean (GEIH_2018$age2)){
  f<--lm(lning~sex+age+estrato1+maxEducLevel+oficio+edad_educacion+edad_sexo+edad_estrato,GEIH_2018)
  coefs<-f$coefficients
  b1<-coefs[2]
  b2<-coefs[3]
  
}

bootreg<- boot (GEIH_2018, eta.mod6.fn,R=1000)
summary (bootcor)


