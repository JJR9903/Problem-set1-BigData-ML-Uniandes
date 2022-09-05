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
  }else if(Sys.info()["user"]=="PC-PORTATIL"){
   setwd("C:/Users/PC-PORTATIL/OneDrive/Documentos/GitHub/Problem-set1-BigData-ML-Uniandes")
 }else{
  setwd("C:/Users/Usuario/Documents/GitHub/Problem-set1-BigData-ML-Uniandes/stores")
  }
}

dir_set()

pacman:: p_load(rvest, tidyverse, skimr, stargazer,cowplot,car,boot,caret)


        ##### 1.a Web Scraping data set (GEIH 2018 - Bogotá) ####

if ("GEIH_2018.RData"%in%list.files(path = "./stores")){
  load("stores/GEIH_2018.RData")
}else{
  
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
  
}


        ##### 1.a.2  Describe data #####

#### Data Cleaning ####

#head(GEIH_2018) # first 6 observations
#str(GEIH_2018) # structure of dataset

##Filtrar por mayores de edad
GEIH_2018<-GEIH_2018%>%
  filter(GEIH_2018$age>18 & GEIH_2018$ocu==1) 


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


#quitamos otras variables que solo tienen un valor, por ejemplo la pea==1 porque todos son ocupados 
GEIH_2018 = subset(GEIH_2018, select = -c(wap,ocu, dsi, pea, pet,inac))


#### Verificar los tipos de las variables ####

# variables categoricas
cats = c("sex", "maxEducLevel","college","cotPension","cuentaPropia","estrato1","formal","informal","microEmpresa","oficio","regSalud","relab","sizeFirm")
for (v in cats) {
  GEIH_2018[, v] <- as.factor(GEIH_2018[, v, drop = T])
}

## revisar variables categoricas que dicen lo mismo
table(GEIH_2018$formal)	
table(GEIH_2018$informal)
summary(GEIH_2018$hoursWorkUsual)
summary(GEIH_2018$totalHoursWorked)


GEIH_2018 <- GEIH_2018 %>% subset(select=-c(hoursWorkUsual,informal))
skim=skim(GEIH_2018)





#### descripción de los datos ####

### Descripción de variables categoricas ### oficio, sizeFirm
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
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18))
  

estrato_PG <-ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = estrato1),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("1"="#F5B7B1" , "2"="#E8DAEF", "3"="#85C1E9","4"="#73C6B6","5"="#F8C471","6"="#BA4A00"), 
                    label = c("1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['1']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estrato 1")
                              ,"2"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['2']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estrato 2")
                              ,"3"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['3']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estrato 3")
                              ,"4"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['4']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estrato 4")
                              ,"5"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['5']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estrato 5")
                              ,"6"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['estrato1'])['6']) / sum(as.numeric(table(GEIH_2018['estrato1']))),2)),"%","Estrato 6")
                              ) , name = "")+
  labs(title = "Estrato de la vivienda")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18))

NivelEducativo_BG <- ggplot(data=GEIH_2018) +
  geom_bar(stat="identity",aes(y=dummy, x=maxEducLevel, fill = maxEducLevel), width = 0.5)+
  scale_fill_manual( values = c("1"="#9b5fe0" ,"2"="#F5B7B1", "3"="#60dbe8","4"="#8bd346","5"="#efdf48","6"="#f9a52c","7"="#d64e12"),
                    label = c("1"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['1']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%"," Ninguno")
                              ,"3"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['3']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%"," Primaria incompelta")
                              ,"4"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['4']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Primaria completa")
                              ,"5"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['5']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Secundaria incompleta")
                              ,"6"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['6']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Secundaria completa")
                              ,"7"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['maxEducLevel'])['7']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Superior o universitaria")
                    ) , 
                    name = "")+
  labs(title = "Máximo Nivel Educativo alcanzado")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18))

college_PG <- ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = college),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("0"="#28BFE8" , "1"="#FA8072"), 
                    label = c("0"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['college'])['1']) / sum(as.numeric(table(GEIH_2018['college']))),2)),"%","No")
                              ,"1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['college'])['0']) / sum(as.numeric(table(GEIH_2018['college']))),2)),"%","Sí")
                    ) , name = "")+
  labs(title = "Tiene educación terciaria")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18))

GEIH_2018$regSalud_nan=GEIH_2018$regSalud
GEIH_2018$regSalud_nan<-as.character(GEIH_2018$regSalud_nan)
GEIH_2018$regSalud_nan[is.na(GEIH_2018$regSalud_nan)] <- as.character(0)
GEIH_2018$regSalud_nan<-as.factor(GEIH_2018$regSalud_nan)

regSalud_PG <- ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = regSalud_nan),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("0"="#9D9B92","1"="#56ba5a" , "2"="#cb3747","3"="#0cc6b8"), 
                    label = c("0"=paste("  ",as.character(round(100 * as.numeric(table(GEIH_2018['regSalud_nan'])['0']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Nan")
                              ,"1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['regSalud_nan'])['1']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Regimen Contributivo")
                              ,"2"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['regSalud_nan'])['2']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Regimen Especial") 
                              ,"3"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['regSalud_nan'])['3']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Regimen Subsidiado") 
                    ) ,
                    name = "")+
  labs(title = "Regimen de Seguridad Social en Salud")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18))

cotPension_PG <- ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = cotPension),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("1"="#439790" , "2"="#fd6767","3"="#7a9c48"), 
                    label = c("1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['cotPension'])['1']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Cotiza a pension")
                              ,"2"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['cotPension'])['2']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","No cotiza a pension") 
                              ,"3"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['cotPension'])['3']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Pensionado") 
                             
                    ) ,
                    name = "",na.translate=T)+
  labs(title = "Estado Pensional")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18))

relab_BG <- ggplot(data=GEIH_2018) +
  geom_bar(stat="identity",aes(y=dummy, x=relab, fill = relab), width = 0.5)+
  scale_fill_manual( values = c("1"="#f6993f" , "2"="#ffed4a", "3"="#38c172","4"="#4dc0b5","5"="#3490dc","6"="#6574cd","7"="#9561e2","8"="#f66d9b","9"="656950"),
                     label = c("1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['relab'])['1']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Obrero o empleado de empresa particular")
                               ,"2"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['relab'])['2']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Obrero o empleado del gobierno")
                               ,"3"=paste("  ",as.character(round(100 * as.numeric(table(GEIH_2018['relab'])['3']) / sum(as.numeric(table(GEIH_2018['dummy']))),2))," %","Empleado doméstico")
                               ,"4"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['relab'])['4']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Trabajador por cuenta propia")
                               ,"5"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['relab'])['5']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Patron o empleadora")
                               ,"6"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['relab'])['6']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Trabajador familiar sin remuneracion")
                               ,"7"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['relab'])['7']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Trabajador sin remuneracion en empresas o negocios de otros hogares")
                               ,"8"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['relab'])['8']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Jornalero o peon")
                               ,"9"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['relab'])['9']) / sum(as.numeric(table(GEIH_2018['dummy']))),2)),"%","Otro")
                     ) , 
                     name = "")+
  labs(title = "Relación Laboral")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18))



sizeFirm_BG <-ggplot(data=GEIH_2018) +
  geom_bar(stat='identity',aes(x = sizeFirm, y = dummy, fill = sizeFirm),width = 1)+
  scale_fill_manual(values = c("1"="#F5B7B1" , "2"="#E8DAEF", "3"="#85C1E9","4"="#73C6B6","5"="#F8C471"), 
                    label = c("1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['sizeFirm'])['1']) / sum(as.numeric(table(GEIH_2018['sizeFirm']))),2)),"%","Cuenta propia (independiente)")
                              ,"2"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['sizeFirm'])['2']) / sum(as.numeric(table(GEIH_2018['sizeFirm']))),2)),"%","2-5 trabajadores")
                              ,"3"=paste(" ",as.character(round(100 * as.numeric(table(GEIH_2018['sizeFirm'])['3']) / sum(as.numeric(table(GEIH_2018['sizeFirm']))),2)),"%","6-10 trabajadores")
                              ,"4"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['sizeFirm'])['4']) / sum(as.numeric(table(GEIH_2018['sizeFirm']))),2)),"%","11-50 trabajadores")
                              ,"5"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['sizeFirm'])['5']) / sum(as.numeric(table(GEIH_2018['sizeFirm']))),2)),"%","+ 50 trabajodores")
                    ) , name = "")+
  labs(title = "Tamaño de la empresa")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18))


formal_PG <- ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = formal),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("0"="#800080" , "1"="#008744"), 
                    label = c("0"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['formal'])['1']) / sum(as.numeric(table(GEIH_2018['formal']))),2)),"%","No")
                              ,"1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['formal'])['0']) / sum(as.numeric(table(GEIH_2018['formal']))),2)),"%","Sí")
                    ) , name = "")+
  labs(title = "Formal")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18))



cuentaPropia_PG <- ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = cuentaPropia),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("0"="#28BFE8" , "1"="#FA8072"), 
                    label = c("0"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['cuentaPropia'])['1']) / sum(as.numeric(table(GEIH_2018['cuentaPropia']))),2)),"%","No")
                              ,"1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['cuentaPropia'])['0']) / sum(as.numeric(table(GEIH_2018['cuentaPropia']))),2)),"%","Si")
                    ) , name = "")+
  labs(title = "Independientes (cuenta propia)")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18))


microEmpresa_PG <- ggplot(data=GEIH_2018) +
  geom_bar(aes(x = 1, weight = dummy, fill = microEmpresa),width = 1)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("0"="#ff6f69" , "1"="#ffeead"), 
                    label = c("0"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['microEmpresa'])['1']) / sum(as.numeric(table(GEIH_2018['microEmpresa']))),2)),"%","No")
                              ,"1"=paste(as.character(round(100 * as.numeric(table(GEIH_2018['microEmpresa'])['0']) / sum(as.numeric(table(GEIH_2018['microEmpresa']))),2)),"%","Sí")
                    ) , name = "")+
  labs(title = "Micro Empresa")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18))


caractPersona_plot<- plot_grid(sexo_PG,estrato_PG,NivelEducativo_BG,college_PG,regSalud_PG, ncol = 2, nrow = 3)
caractEmpelo_plot <- plot_grid(cotPension_PG,formal_PG,microEmpresa_PG,cuentaPropia_PG,sizeFirm_BG,relab_BG, ncol = 2, nrow = 3)

ggsave("views/caractPersona_plot.png", width = 70, height = 50, units="cm",plot = caractPersona_plot)
ggsave("views/caractEmpelo_plot.png", width = 70, height = 50, units="cm",plot = caractEmpelo_plot)

#hace falta oficio

### Descripción de variables numericas (NO de ingreso) ### 

# age y hoursWorkUsual
age_hist <- ggplot(GEIH_2018, aes(x=age)) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(age),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(age)+sd(age),color="+sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(age)-sd(age),color="-sd"), linetype="dashed", size=1)+
  scale_color_manual(name= "", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222") 
                    , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$age)-sd(GEIH_2018$age),2)),"= -1 sd"),
                               "Media"=paste(as.character(round(mean(GEIH_2018$age),2)),"= media"),
                               "+sd"=paste(as.character(round(mean(GEIH_2018$age)+sd(GEIH_2018$age),2)),"= +1 sd")
                               ))+
  labs(title="Edad",x="Edad",y="densidad")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18),axis.title = element_text(size = 20))


totalHoursWorked_hist <- ggplot(GEIH_2018, aes(x=totalHoursWorked)) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(age)+sd(age),color="-sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(age),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(age)-sd(age),color="+sd"), linetype="dashed", size=1)+
  scale_color_manual(name= "", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222") 
                     , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$totalHoursWorked,na.rm = T)-sd(GEIH_2018$totalHoursWorked,na.rm = T),2)),"= -1 sd"),
                                "Media"=paste(as.character(round(mean(GEIH_2018$totalHoursWorked,na.rm = T),2)),"= media"),
                                "+sd"=paste(as.character(round(mean(GEIH_2018$totalHoursWorked,na.rm = T)+sd(GEIH_2018$totalHoursWorked,na.rm = T),2)),"= +1 sd")
                     ))+
  labs(title="Horas trabajadas a la semana",x="",y="densidad")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18),axis.title = element_text(size = 20))


numerical_plot<- plot_grid(age_hist, totalHoursWorked_hist, ncol = 2, nrow = 1)
ggsave("views/numerical_plot.png", width = 70, height = 25, units="cm",plot = numerical_plot)


### Descripción de variables de Ingreso ## 
# comparación entre variables que dicen cosas parecidas se escogen algunas y de esas se comparan por edad
# impa, isa, y_salary_m, y_ingLab_m,y_gananciaIndep_m, y_total_m

# impa - principal occ.
impa_hist <- ggplot(GEIH_2018, aes(x=impa/(1000000))) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(impa/(1000000),na.rm = T)+sd(impa/(1000000),na.rm = T),color="-sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(impa/(1000000),na.rm = T),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(impa/(1000000),na.rm = T)-sd(impa/(1000000),na.rm = T),color="+sd"), linetype="dashed", size=1)+
  scale_color_manual(name= "", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222","missings"="#ffffff") 
                     , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$impa/(1000000),na.rm = T)-sd(GEIH_2018$impa/(1000000),na.rm = T),2)),"= -1 sd"),
                                "Media"=paste(as.character(round(mean(GEIH_2018$impa/(1000000),na.rm = T),2)),"= media"),
                                "+sd"=paste(as.character(round(mean(GEIH_2018$impa/(1000000),na.rm = T)+sd(GEIH_2018$impa/(1000000),na.rm = T),2)),"= +1 sd"),
                                "missings"=paste(as.character(sum(is.na(GEIH_2018$impa))),"missings")
                     ))+
  labs(title = "Ingreso actividad principal",x="Ingreso monetario actividad principal",y="densidad",caption="$COP En millones mensuales")+
  xlim(-1,20)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18),axis.title = element_text(size = 20))

impa_point <- ggplot(GEIH_2018, aes(y=impa/(1000000),x=age, color="#1da2d8")) +
  geom_point()+
  labs(title = "Ingreso actividad principal",y="Ingreso monetario actividad principal",x="Edad",caption="$COP En millones mensuales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),axis.title = element_text(size = 20))

# isa - principal occ.
isa_hist <- ggplot(GEIH_2018, aes(x=isa/(1000000))) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(isa/(1000000),na.rm = T)+sd(isa/(1000000),na.rm = T),color="-sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(isa/(1000000),na.rm = T),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(isa/(1000000),na.rm = T)-sd(isa/(1000000),na.rm = T),color="+sd"), linetype="dashed", size=1)+
  scale_color_manual(name= "", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222","missings"="#ffffff") 
                     , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$isa/(1000000),na.rm = T)-sd(GEIH_2018$impa/(1000000),na.rm = T),2)),"= -1 sd"),
                                "Media"=paste(as.character(round(mean(GEIH_2018$isa/(1000000),na.rm = T),2)),"= media"),
                                "+sd"=paste(as.character(round(mean(GEIH_2018$isa/(1000000),na.rm = T)+sd(GEIH_2018$impa/(1000000),na.rm = T),2)),"= +1 sd"),
                                "missings"=paste(as.character(sum(is.na(GEIH_2018$isa))),"missings")
                     ))+
  labs(title = "Ingreso actividad secundaria",x="Ingreso monetario actividad secundaria",y="densidad",caption="$COP En millones mensuales")+
  xlim(-1,20)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18),axis.title = element_text(size = 20))

isa_point <- ggplot(GEIH_2018, aes(y=impa/(1000000),x=age, color="#1da2d8")) +
  geom_point()+
  labs(title = "Ingreso actividad secundaria",y="Ingreso monetario actividad secundaria",x="Edad",caption="$COP En millones mensuales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),axis.title = element_text(size = 20))

# y_salary_m - principal occ.
salary_hist <- ggplot(GEIH_2018, aes(x=y_salary_m/(1000000))) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(y_salary_m/(1000000),na.rm = T)+sd(y_salary_m/(1000000),na.rm = T),color="-sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(y_salary_m/(1000000),na.rm = T),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(y_salary_m/(1000000),na.rm = T)-sd(y_salary_m/(1000000),na.rm = T),color="+sd"), linetype="dashed", size=1)+
  scale_color_manual(name= "", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222","missings"="#ffffff") 
                     , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$y_salary_m/(1000000),na.rm = T)-sd(GEIH_2018$y_salary_m/(1000000),na.rm = T),2)),"= -1 sd"),
                                "Media"=paste(as.character(round(mean(GEIH_2018$y_salary_m/(1000000),na.rm = T),2)),"= media"),
                                "+sd"=paste(as.character(round(mean(GEIH_2018$y_salary_m/(1000000),na.rm = T)+sd(GEIH_2018$y_salary_m/(1000000),na.rm = T),2)),"= +1 sd"),
                                "missings"=paste(as.character(sum(is.na(GEIH_2018$y_salary_m))),"missings")
                     ))+
  labs(title = "Salario actividad principal",x="Salario actividad principal",y="densidad",caption="$COP En millones mensuales (incluye propinas y comisiones)")+
  xlim(-1,20)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18),axis.title = element_text(size = 20))

salary_point <- ggplot(GEIH_2018, aes(y=y_salary_m/(1000000),x=age, color="#1da2d8")) +
  geom_point()+
  labs(title = "Salario actividad principal",y="Salario actividad principal",x="Edad",caption="$COP En millones mensuales (incluye propinas y comisiones)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),axis.title = element_text(size = 20))

# y_ingLab_m - all occ. 
ingLab_hist <- ggplot(GEIH_2018, aes(x=y_ingLab_m/(1000000))) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(y_ingLab_m/(1000000),na.rm = T)+sd(y_ingLab_m/(1000000),na.rm = T),color="-sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(y_ingLab_m/(1000000),na.rm = T),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(y_ingLab_m/(1000000),na.rm = T)-sd(y_ingLab_m/(1000000),na.rm = T),color="+sd"), linetype="dashed", size=1)+
  scale_color_manual(name= " ", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222","missings"="#ffffff") 
                     , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$y_ingLab_m/(1000000),na.rm = T)-sd(GEIH_2018$y_ingLab_m/(1000000),na.rm = T),2)),"= -1 sd"),
                                "Media"=paste(as.character(round(mean(GEIH_2018$y_ingLab_m/(1000000),na.rm = T),2)),"= media"),
                                "+sd"=paste(as.character(round(mean(GEIH_2018$y_ingLab_m/(1000000),na.rm = T)+sd(GEIH_2018$y_ingLab_m/(1000000),na.rm = T),2)),"= +1 sd"),
                                "missings"=paste(as.character(sum(is.na(GEIH_2018$y_ingLab_m))),"missings")
                     ))+
  labs(title = "Salario laboral",x="Salario laboral",y="densidad",caption="$COP En millones mensuales (incluye propinas y comisiones)")+
  xlim(-1,20)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18),axis.title = element_text(size = 20))

ingLab_point <- ggplot(GEIH_2018, aes(y=y_ingLab_m/(1000000),x=age, color="#1da2d8")) +
  geom_point()+
  labs(title = "Salario laboral",y="Salario laboral",x="Edad", caption="$COP En millones mensuales (incluye propinas y comisiones)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),axis.title = element_text(size = 20))
  
#y_total_m - income salaried + independents total
y_total_hist <- ggplot(GEIH_2018, aes(x=y_total_m/(1000000))) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(y_total_m/(1000000),na.rm = T)+sd(y_total_m/(1000000),na.rm = T),color="-sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(y_total_m/(1000000),na.rm = T),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(y_total_m/(1000000),na.rm = T)-sd(y_total_m/(1000000),na.rm = T),color="+sd"), linetype="dashed", size=1)+
  scale_color_manual(name="", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222","missings"="#ffffff") 
                     , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$y_total_m/(1000000),na.rm = T)-sd(GEIH_2018$y_total_m/(1000000),na.rm = T),2)),"= -1 sd"),
                                "Media"=paste(as.character(round(mean(GEIH_2018$y_total_m/(1000000),na.rm = T),2)),"= media"),
                                "+sd"=paste(as.character(round(mean(GEIH_2018$y_total_m/(1000000),na.rm = T)+sd(GEIH_2018$y_total_m/(1000000),na.rm = T),2)),"= +1 sd"),
                                "missings"=paste(as.character(sum(is.na(GEIH_2018$y_total_m))),"missings")
                     ))+
  labs(title= "Salario + ingreso independientes",x="Salario + ingreso independientes",y="densidad",caption="$COP En millones mensuales")+
  xlim(-1,20)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18),axis.title = element_text(size = 20))

y_total_point <- ggplot(GEIH_2018, aes(y=y_total_m/(1000000),x=age, color="#1da2d8")) +
  geom_point()+
  labs(title= "Salario + ingreso independientes",y="Salario + ingreso independientes",x="Edad",caption="$COP En millones mensuales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),axis.title = element_text(size = 20))

#y_gananciaIndep_m 
gananciaIndep_hist <- ggplot(GEIH_2018, aes(x=y_gananciaIndep_m/(1000000))) +
  geom_histogram(aes(y=..density..),fill = "#28BFE8", color = "white") + 
  geom_vline(aes(xintercept=mean(y_gananciaIndep_m/(1000000),na.rm = T)+sd(y_gananciaIndep_m/(1000000),na.rm = T),color="-sd"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(y_gananciaIndep_m/(1000000),na.rm = T),color="Media"), size=1)+
  geom_vline(aes(xintercept=mean(y_gananciaIndep_m/(1000000),na.rm = T)-sd(y_gananciaIndep_m/(1000000),na.rm = T),color="+sd"), linetype="dashed", size=1)+
  scale_color_manual(name= " ", values = c("-sd" = "#B22222","Media" = "#5cb85c",  "+sd" = "#B22222","missings"="#ffffff") 
                     , labels=c("-sd"=paste(as.character(round(mean(GEIH_2018$y_gananciaIndep_m/(1000000),na.rm = T)-sd(GEIH_2018$y_gananciaIndep_m/(1000000),na.rm = T),2)),"= -1 sd"),
                                "Media"=paste(as.character(round(mean(GEIH_2018$y_gananciaIndep_m/(1000000),na.rm = T),2)),"= media"),
                                "+sd"=paste(as.character(round(mean(GEIH_2018$y_gananciaIndep_m/(1000000),na.rm = T)+sd(GEIH_2018$y_gananciaIndep_m/(1000000),na.rm = T),2)),"= +1 sd")
                     ))+
  labs(title= "Ganancia total independientes",x="Ganancia total independientes",y="densidad",caption="$COP En millones mensuales")+
  xlim(-1,20)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18),axis.title = element_text(size = 20))

gananciaIndep_point <- ggplot(GEIH_2018, aes(y=y_gananciaIndep_m/(1000000),x=age, color="#1da2d8")) +
  geom_point()+
  labs(title="Ganancia total independientes",y="Ganancia total independientes)",x="Edad", caption="$COP En millones mensuales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),axis.title = element_text(size = 20))

y_hist<-plot_grid(impa_hist, isa_hist,salary_hist,ingLab_hist,gananciaIndep_hist,y_total_hist, ncol = 2, nrow = 3)
y_point<-plot_grid(impa_point, isa_point,salary_point,ingLab_point,gananciaIndep_point,y_total_point, ncol = 2, nrow = 3)


ggsave("views/y_hist.png", width = 70, height = 50, units="cm",plot = y_hist)
ggsave("views/y_point.png", width = 70, height = 50, units="cm",plot = y_point)


stargazer(GEIH_2018%>%select(c(impa,isa,y_salary_m,y_ingLab_m,y_gananciaIndep_m,y_total_m,totalHoursWorked)),title = "Estadisticas descriptivas", type = "text", column.labels = c("Variable","Obs","Media","Desv. est.","Min","Max"),
          covariate.labels=c("Edad","Ingreso de la actividad principal","Ingreso actividad secundaria","Salario actividad principal","Salario laboral","Salario + ingreso independientes","Ganancia total independientes"),
          out = "views/numericalSummaryStats.txt")

GEIH_2018<- subset(GEIH_2018, filter=-c(regSalud_nan,dummy))

rm(list= ls(pattern = "PG|BG|hist|point|plot"))


##### NAN IMPUTING 
#imputamos la media condicional de la variable en diferentes caracteristicas de la persona, 
#si es una variable categorica se le pone la categoria mas frecuente dadas las caracteristicas
# dado que son varias caracteristicas algunos valores no se rellenan en la primera ronda, entonces se relaja la condición en una segunda o tercera ronda hasta que queden todos rellenados

GEIH_2018 = GEIH_2018 %>% 
  group_by(estrato1,sex,age,oficio) %>% 
  mutate(mf_MaxEducLevel = names(which.max(table(maxEducLevel))) )%>%
  ungroup()

GEIH_2018 = GEIH_2018 %>% 
  mutate(maxEducLevel = ifelse(test = is.na(maxEducLevel)==T,
                           yes = mf_MaxEducLevel,
                           no = maxEducLevel))
skim(GEIH_2018$maxEducLevel)#ya estan todos completos 

GEIH_2018 = GEIH_2018 %>% 
  group_by(estrato1,sex,age,maxEducLevel,oficio) %>% 
  mutate(mean_y = mean(y_total_m,na.rm=T),
         mean_impa = mean(impa,na.rm=T),
         mf_regSalud = names(which.max(table(regSalud))) )%>%
  ungroup()

GEIH_2018 = GEIH_2018 %>% 
  mutate(y_total_m = ifelse(test = is.na(y_total_m)==T,
                           yes = mean_y,
                           no = y_total_m),
         impa = ifelse(test = is.na(impa)==T,
                       yes = mean_impa,
                       no = impa),
         regSalud = ifelse(test = is.na(regSalud)==T,
                       yes = mf_regSalud,
                       no = regSalud))

summary(GEIH_2018$y_total_m)#todavía quedan nan
summary(GEIH_2018$impa)#todavía quedan nan
skim(GEIH_2018$regSalud)#ya estan todos completos 

GEIH_2018 = GEIH_2018 %>% 
  group_by(estrato1,sex,maxEducLevel,oficio) %>% 
  mutate(mean_y = mean(y_total_m,na.rm=T),
         mean_impa = mean(impa,na.rm=T) )%>%
  ungroup()

GEIH_2018 = GEIH_2018 %>% 
  mutate(y_total_m = ifelse(test = is.na(y_total_m)==T,
                            yes = mean_y,
                            no = y_total_m),
         impa = ifelse(test = is.na(impa)==T,
                       yes = mean_impa,
                       no = impa) )

summary(GEIH_2018$y_total_m)#todavía quedan nan
summary(GEIH_2018$impa)#todavía quedan nan
        
GEIH_2018 = GEIH_2018 %>% 
  group_by(estrato1,sex,maxEducLevel) %>% 
  mutate(mean_y = mean(y_total_m,na.rm=T),
         mean_impa = mean(impa,na.rm=T) )%>%
  ungroup()

GEIH_2018 = GEIH_2018 %>% 
  mutate(y_total_m = ifelse(test = is.na(y_total_m)==T,
                            yes = mean_y,
                            no = y_total_m),
         impa = ifelse(test = is.na(impa)==T,
                       yes = mean_impa,
                       no = impa) )

summary(GEIH_2018$y_total_m)# ya esta completa
summary(GEIH_2018$impa)## ya esta completa
GEIH_2018$regSalud <- as.factor(GEIH_2018$regSalud)

sumstat <- GEIH_2018 %>%
  select(
    "Ingreso de la actividad principal" = impa,
    "Salario +  ganancia independientes" = y_total_m) %>%
  summarise_each(funs(mean, sd, min, max)) %>%
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  select(variable, mean, sd, min, max) %>%
  mutate_each(funs(round(., 1)), -variable)
sumstat=rename(sumstat,"Statistic"="variable","Mean"="mean","St. Dev."="sd","Min"="min", "Max"="max")
write.table(sumstat, file = "views/sumstats after nan imputing.txt", sep = ",", quote = FALSE, row.names = F)

GEIH_2018<-subset(GEIH_2018, select = -c(mf_regSalud,regSalud_nan,mean_impa,mean_y,dummy,mf_MaxEducLevel ))
skim=skim(GEIH_2018)
sumstat_maxEducLevel<-as.data.frame(prop.table(table(GEIH_2018$maxEducLevel)))%>%
  mutate(maxEducLevel= case_when(Var1==1~"Ninguno",Var1==2~"Primaria incompleta",Var1==3~"Primaria completa",Var1==4~"Secundaria incompleta",Var1==5~"Secundaria completa",Var1==6~"Superior o universitaria"),
         Freq=round(Freq,2))

sumstat_regSalud<-as.data.frame(prop.table(table(GEIH_2018$regSalud)))%>%
  mutate(regSalud=case_when(Var1==1~"r. contributivo",Var1==2~"r. especial",Var1==3~"r. subsidiado"), Freq=round(Freq,2))

sumstat_factors <- as.data.frame(cbind(sumstat_maxEducLevel$maxEducLevel,sumstat_maxEducLevel$Freq,sumstat_regSalud$regSalud,sumstat_regSalud$Freq))
sumstat_factors<- rename(sumstat_factors, "maxEducLevel"="V1","maxEducLevel %"="V2","regSalud"="V3","regSalud %"="V4")
sumstat_factors[4:6,3:4]<-NaN

write.table(sumstat_factors, file = "views/sumstats factors after nan imputing.txt", sep = ",", quote = FALSE, row.names = F)

rm(list=c("sumstat_factors","sumstat_regSalud","sumstat_maxEducLevel","sumstat"))

# quitamos todas las otras variables de ingreso que no nos interesan 
ing_vars = list(subset(skim, select =skim_variable))
ing_vars =  ing_vars[[1]][["skim_variable"]]
v = grep('^y|ie|ing|im|iof|is', ing_vars)
ing_vars= ing_vars[c(v)]
ing_vars<-ing_vars[ing_vars != 'y_total_m']
ing_vars<-ing_vars[ing_vars != 'impa']
for (i in ing_vars){GEIH_2018[i]<-NULL}
skim=skim(GEIH_2018)

#transformaciones de algunas variables 
GEIH_2018$ln_y<-log(GEIH_2018$y_total_m)
GEIH_2018$age2<-GEIH_2018$age^2
GEIH_2018<-GEIH_2018%>%
  mutate(Mujer=ifelse(test=sex==0,yes=1,no=0))
GEIH_2018$estrato<-as.numeric(GEIH_2018$estrato1)
GEIH_2018$educacion<-as.numeric(GEIH_2018$maxEducLevel)
## guardamos la base de datos limpia 
write.csv(GEIH_2018,file = "stores/GEIH_2018_clean.csv",fileEncoding = "UTF-8")



      ######### 2. Age-earnings profile ##########

 ### Modelo 1.0 (ln_y~age+age2)
Model1.m <- subset(GEIH_2018, select = c(y_total_m,ln_y,age,age2) )
model1<-lm(ln_y~age+age2,Model1.m)

#tabla
stargazer(model1,title="Perfil de ingreso - Edad", out=file.path(getwd(),"/views/model1.txt"),out.header = T, type = "text")
summary(model1)
##Intervalos 
#Guardar betas 
Coef1<- model1$coef
b0<-Coef1[1]
b1<-Coef1[2]
b2<-Coef1[3]
#Derivando para encontrar el punto mx me interesa es b1 y b2 
age_peak=-b1/(2*b2)


#Definir la función de para el bootstrap
set.seed(10101)
beta.m1<-function(data,index){
f<-lm(ln_y~age+age2,data=data,subset =index )
coefs<-f$coefficients
return(coefs)
}

res.m1<-function(data,index){
  f<-lm(ln_y~age+age2,data=data,subset =index )
  res <- f$residuals
  return(res)
}


boot_beta_m1<- boot (Model1.m, beta.m1,R=1000)
boot_res_m1<- boot (Model1.m, res.m1,R=1000)


b0_lwr <-boot.ci(boot_beta_m1,type = "basic",index = 1)[[4]][[4]]
b0_upr <-boot.ci(boot_beta_m1,type = "basic",index = 1)[[4]][[5]]
b1_lwr <-boot.ci(boot_beta_m1,type = "basic",index = 2)[[4]][[4]]
b1_upr <-boot.ci(boot_beta_m1,type = "basic",index = 2)[[4]][[5]]
b2_lwr <-boot.ci(boot_beta_m1,type = "basic",index = 3)[[4]][[4]]
b2_upr <-boot.ci(boot_beta_m1,type = "basic",index = 3)[[4]][[5]]

res_lwr <-boot.ci(boot_res_m1,type = "basic")[[4]][[4]]
res_upr <-boot.ci(boot_res_m1,type = "basic")[[4]][[5]]

Model1.m$y_fitted <- fitted(model1)
Model1.m$y_f_lw  <-predict(model1,interval='prediction')[,2]
Model1.m$y_f_up  <-predict(model1,interval='prediction')[,3]


Model1.m$y_fitted_lwr <- (b0_lwr + b1_lwr*Model1.m$age + b2_lwr*Model1.m$age2 + res_lwr)
Model1.m$y_fitted_upr <- (b0_upr + b1_upr*Model1.m$age + b2_upr*Model1.m$age2 + res_upr)


age_earningsProfile_m1 <- ggplot(Model1.m, aes(x = age, y = ln_y) ) +
  geom_line(aes(y = y_fitted), size = 1)+
  geom_ribbon( aes(ymin = y_fitted_lwr, ymax = y_fitted_upr,color = "IC Bootstrap"),fill="lightblue", alpha = .4)+
  geom_ribbon( aes(ymin = y_f_lw, ymax = y_f_up,color="IC"),fill="red", alpha = .4)+
  labs(title = "Age earnings profile",y="Salario + ingreso independientes",x="Edad",caption="logaritmo del ingreso",legend="Intervalos")+
  theme_light()+
  ylim(0,40)+
  scale_color_manual("",values = c("IC Bootstrap"="lightblue","IC"="red"))+
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),legend.text = element_text(size = 18),axis.title = element_text(size = 20))
  
ggsave("views/age_earningsProfile_m1.png", width = 70, height = 50, units="cm",plot = age_earningsProfile_m1)

 

  ######### 3. Age-earnings profile ##########

#MODELO 2.0 (ln_y~sex)
Model2.m <- subset(GEIH_2018, select = c(ln_y,Mujer) )
model2<-lm(ln_y~Mujer,Model2.m)

stargazer(model2,title="", out=file.path(getwd(),"/views/model2.txt"),out.header = T,type="text")
summary(model2)


#MODELO 2.1 (ln_y~sex+age+age2)
####### OJO TODAVIA NO ESTA  ######
Model2.1.m <- subset(GEIH_2018, select = c(ln_y,Mujer,age,age2) )
model2.1<-lm(ln_y~age+age2+Mujer,Model2.1.m)
summary(model2.1)

Coef<- model2.1$coef
b0<-Coef[1]
b1<-Coef[2]
b2<-Coef[3]
b3<-Coef[4]
res<-model2.1$residuals

Model2.1.m$ln_y_fitted_H <- b0 + b1*Model2.1.m$age + b2*Model2.1.m$age2 + res
Model2.1.m$ln_y_fitted_M <- b0 + b1*Model2.1.m$age + b2*Model2.1.m$age2 + b3*Model2.1.m$Mujer + res
Model2.1.m$ln_y_f_lw  <-predict(model2.1,interval='prediction')[,2]
Model2.1.m$ln_y_f_up  <-predict(model2.1,interval='prediction')[,3]


#Derivando para encontrar el punto mx me interesa es b1 y b2 
age_peak=-b1/(2*b2)

##grafica 
beta.m2.1<-function(data,index){
  f<-lm(ln_y~age+age2+Mujer,data=data,subset =index )
  coefs<-f$coefficients
  return(coefs)
}

res.m2.1<-function(data,index){
  f<-lm(ln_y~age+age2+Mujer,data=data,subset =index )
  res <- f$residuals
  return(res)
}

boot_beta_m2.1<- boot (Model2.1.m, beta.m2.1,R=1000)
boot_res_m2.1<- boot (Model2.1.m, res.m2.1,R=1000)

b0_lwr <-boot.ci(boot_beta_m2.1,type = "basic",index = 1)[[4]][[4]]
b0_upr <-boot.ci(boot_beta_m2.1,type = "basic",index = 1)[[4]][[5]]
b1_lwr <-boot.ci(boot_beta_m2.1,type = "basic",index = 2)[[4]][[4]]
b1_upr <-boot.ci(boot_beta_m2.1,type = "basic",index = 2)[[4]][[5]]
b2_lwr <-boot.ci(boot_beta_m2.1,type = "basic",index = 3)[[4]][[4]]
b2_upr <-boot.ci(boot_beta_m2.1,type = "basic",index = 3)[[4]][[5]]
b3_lwr <-boot.ci(boot_beta_m2.1,type = "basic",index = 4)[[4]][[4]]
b3_upr <-boot.ci(boot_beta_m2.1,type = "basic",index = 4)[[4]][[5]]

res_lwr <-boot.ci(boot_res_m2.1,type = "basic")[[4]][[4]]
res_upr <-boot.ci(boot_res_m2.1,type = "basic")[[4]][[5]]



Model2.1.m$ln_y_fitted_lwr <- (b0_lwr + b1_lwr*Model2.1.m$age + b2_lwr*Model2.1.m$age2 + b3_lwr*Model2.1.m$Mujer+ res_lwr)
Model2.1.m$ln_y_fitted_upr <- (b0_upr + b1_upr*Model2.1.m$age + b2_upr*Model2.1.m$age2 + b3_upr*Model2.1.m$Mujer+ res_upr)

#age_earningsProfile_m2.1 <-
 ggplot(Model2.1.m, aes(x = age, y = ln_y) ) +
  geom_line(aes(y = ln_y_fitted_H), size = 1)
  geom_line(aes(y = Model2.1.m$ln_y_fitted_lwr ,colour = "lightblue"),color="lightblue", size = 1)+
  geom_line(aes(y = Model2.1.m$ln_y_fitted_upr,colour = "lightblue"),color="lightblue", size = 1)+
  geom_ribbon( aes(ymin = Model2.1.m$ln_y_fitted_lwr, ymax = Model2.1.m$ln_y_fitted_upr, fill = "lightblue", alpha = .4))+
  geom_ribbon( aes(ymin = ln_y_f_lw, ymax = ln_y_f_up), fill = "red", alpha = .4)+
  labs(title = "Age earnings profile",y="Salario + ingreso independientes",x="Edad",caption="ingreso estandarizado")+
  scale_color_manual(name= "", values = c("ci" = "lightblue","y_hat" = "#5cb85c") 
                     , labels=c("ci"="ci",
                                "y_hat"="y_hat"
                     ))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5,size=14,face="bold"))

ggsave("views/age_earningsProfile_m1.png", width = 70, height = 50, units="cm",plot = age_earningsProfile_m1)



##MODELO CON CONTROLES 

edad_educacion<-(GEIH_2018$educacion*GEIH_2018$age)
edad_sexo<-(GEIH_2018$age*GEIH_2018$Mujer)
edad_estrato<-(GEIH_2018$age*GEIH_2018$estrato)


model3<-lm(ln_y~sex+age+estrato1+maxEducLevel+edad_educacion+edad_estrato+edad_sexo, Model3.m.c)

##MODELO CON WFL 
require("tidyverse")
require("fabricatr")

reg1<-lm(ln_y~sex+age+estrato1+maxEducLevel+oficio+edad_educacion+edad_sexo+edad_estrato,GEIH_2018)
stargazer(reg1,type="text")

GEIH_2018$res_y_a<-1
GEIH_2018$res_s_a<-1

GEIH_2018<-GEIH_2018%>% 
  res_y_a=lm(ln_y~age+estrato1+maxEducLevel+oficio+edad_educacion+edad_sexo+edad_estrato,GEIH_2018)$residuals,
  res_s_a=lm(sex~age+estrato1+maxEducLevel+oficio+edad_educacion+edad_sexo+edad_estrato,GEIH_2018)$residuals,
         

reg2<-lm(res_y_a~res_s_a-1,GEIH_2018)
stargazer(model1,title="fwl", out=file.path(getwd(),"/views/FWL.txt"),out.header = T, type = "text")


#MODELO CON BOOT ##sacar el error 
set.seed(1000)
eta.mod6.fn<-function(data,index,age_var=mean(GEIH_2018$age), age2_var=mean (GEIH_2018$age2)){
  f<--lm(ln_y~sex+age+estrato1+maxEducLevel+oficio+edad_educacion+edad_sexo+edad_estrato,GEIH_2018)
  coefs<-f$coefficients
  b1<-coefs[2]
  b2<-coefs[3]
  res <- f$residuals
  return(res)
}
bootbeta<- boot (GEIH_2018, eta.mod6.fn,R=1000)

b0_lwr <-boot.ci(bootbeta,type = "basic",index = 1)[[4]][[4]]
b0_upr <-boot.ci(bootbeta,type = "basic",index = 1)[[4]][[5]]
b1_lwr <-boot.ci(bootbeta,type = "basic",index = 2)[[4]][[4]]
b1_upr <-boot.ci(bootbeta,type = "basic",index = 2)[[4]][[5]]
b2_lwr <-boot.ci(bootbeta,type = "basic",index = 3)[[4]][[4]]
b2_upr <-boot.ci(bootbeta,type = "basic",index = 3)[[4]][[5]]


Model2.1.m$ln_y_fitted_lwr <- (b0_lwr + b1_lwr*Model2.1.m$age + b2_lwr*Model2.1.m$age2 + b3_lwr*Model2.1.m$Mujer+ res_lwr)
Model2.1.m$ln_y_fitted_upr <- (b0_upr + b1_upr*Model2.1.m$age + b2_upr*Model2.1.m$age2 + b3_upr*Model2.1.m$Mujer+ res_upr)




######### 4. predicts earnings ##########
# nuevas variables 
GEIH_2018$lneduc<-log(as.numeric(GEIH_2018$maxEducLevel))
GEIH_2018<-GEIH_2018%>%mutate(Mujer= case_when(GEIH_2018$sex=="0"~1,GEIH_2018$sex=="1"~0))                 
GEIH_2018$Mujer_age<-GEIH_2018$Mujer*GEIH_2018$age
GEIH_2018<-GEIH_2018%>%mutate(Mujer_ed = ifelse(test = GEIH_2018$Mujer==1,yes = GEIH_2018$maxEducLevel,no = "0"))
GEIH_2018$hwork2<-(GEIH_2018$totalHoursWorked)^2 
GEIH_2018 = GEIH_2018 %>% mutate(relab_formal = ifelse(test = GEIH_2018$formal=="1",yes = GEIH_2018$relab,no = 0))
GEIH_2018 = GEIH_2018 %>% mutate(propiasizefirm = ifelse(test = GEIH_2018$cuentaPropia=="1",yes = GEIH_2018$sizefirm,no = 0))

#4.1
#Partir la muestra
set.seed(10101)
GEIH_2018 <- GEIH_2018 %>%
  mutate(holdout= as.logical(1:nrow(GEIH_2018) %in%
                               sample(nrow(GEIH_2018), nrow(GEIH_2018)*.3))
  )
test<-GEIH_2018[GEIH_2018$holdout==T,]
train<-GEIH_2018[GEIH_2018$holdout==F,]

#4.2
#Modelos
mod_41<-lm(ln_y~age+age2+Mujer+lneduc,GEIH_2018)
mod_42<-lm(ln_y~age+age2+Mujer+maxEducLevel+estrato1,GEIH_2018)
mod_43<-lm(ln_y~+age+age2+Mujer_age+lneduc,GEIH_2018)
mod_44<-lm(ln_y~+age+age2+Mujer_age+Mujer_ed+regSalud,GEIH_2018)
mod_45<-lm(ln_y~+age+age2+Mujer+totalHoursWorked + hwork2,GEIH_2018)
mod_46<-lm(ln_y~+age+age2+Mujer+totalHoursWorked + hwork2+ relab+formal+relab_formal+cuentaPropia+sizefirm+propiasizefirm,GEIH_2018)


#Resultados
tab_4<-stargazer(mod_41,mod_42,mod_43,mod_44,mod_45,type="text",out = "views/modelos en muestra p4.text")# revisar como guardar solo el y_hat, y los errores 

#Poder predictivo
mod_41<-lm(ln_y~Mujer-1,data=train)
test$mod_41<-predict(mod_41,newdata = test)
test$se_41<-(test$ln_y-test$mod_41)^2
mse_41<-summary(test$se_41)[4]

mod_42<-lm(ln_y~age+age2,data=train)
test$mod_42<-predict(mod_42,newdata = test)
test$se_42<-(test$ln_y-test$mod_42)^2
mse_42<-summary(test$se_42)[4]

mod_43<-lm(ln_y~age+age2+Mujer,data=train)
test$mod_43<-predict(mod_43,newdata = test)
test$se_43<-(test$ln_y-test$mod_43)^2
mse_43<-summary(test$se_43)[4]

mod_44<-lm(ln_y~age+age2+Mujer+lneduc,data=train)
test$mod_44<-predict(mod_44,newdata = test)
test$se_44<-(test$ln_y-test$mod_44)^2
mse_44<-summary(test$se_44)[4]

mod_45<-lm(ln_y~age+age2+Mujer+maxEducLevel+estrato1,data=train)
test$mod_45<-predict(mod_45,newdata = test)
test$se_45<-(test$ln_y-test$mod_45)^2
mse_45<-summary(test$se_45)[4]

mod_46<-lm(ln_y~+age+age2+Mujer_age+lneduc,data=train)
test$mod_46<-predict(mod_46,newdata = test)
test$se_46<-(test$ln_y-test$mod_46)^2
mse_46<-summary(test$se_46)[4]

mod_47<-lm(ln_y~age+age2+Mujer_age+Mujer_ed+regSalud,data=train)
test$mod_47<-predict(mod_47,newdata = test)
test$se_47<-(test$ln_y-test$mod_47)^2
mse_47<-summary(test$se_47)[4]

mod_48<-lm(ln_y~age+age2+Mujer+totalHoursWorked + hwork2,data=train)
test$mod_48<-predict(mod_48,newdata = test)
test$se_48<-(test$ln_y-test$mod_48)^2
mse_48<-summary(test$se_48)[4]

mod_49<-lm(ln_y~age+age2+Mujer+totalHoursWorked + hwork2 +oficio+formal,data=train)
test$mod_49<-predict(mod_49,newdata = test)
mse_49<-summary(test$se_49)[4]
test$se_49<-(test$ln_y-test$mod_49)^2

mse<-table(mse_41, mse_42, mse_43, mse_44, mse_45, mse_46, mse_47, mse_48, mse_49)
write.table(mse, file = "views/mse.txt", sep = ",", quote = FALSE, row.names = F)

saveRDS(GEIH_2018,file=paste0(getwd(),"/GEIH_2018_4.rds"))

#GEIH_2018<-readRDS(file =paste0(getwd(),"/GEIH_2018_4.rds"))

## 4.3. influence statistic
mod_41<-lm(ln_y~Mujer-1,data=test)
mod_42<-lm(ln_y~age+age2,data=test)
mod_43<-lm(ln_y~age+age2+Mujer,data=test)
mod_44<-lm(ln_y~age+age2+Mujer+lneduc,data=test)
mod_45<-lm(ln_y~age+age2+Mujer+maxEducLevel+estrato1,data=test)
mod_46<-lm(ln_y~+age+age2+Mujer_age+lneduc,data=test)
mod_47<-lm(ln_y~age+age2+Mujer_age+Mujer_ed+regSalud,data=test)
mod_48<-lm(ln_y~age+age2+Mujer+totalHoursWorked + hwork2,data=test)

dfb_41=as.data.frame(dfbeta(mod_41, infl = lm.influence(mod_41, do.coef = TRUE)))
dfb_42=as.data.frame(dfbeta(mod_42, infl = lm.influence(mod_42, do.coef = TRUE)))
dfb_43=as.data.frame(dfbeta(mod_43, infl = lm.influence(mod_43, do.coef = TRUE)))
dfb_44=as.data.frame(dfbeta(mod_44, infl = lm.influence(mod_44, do.coef = TRUE)))
dfb_45=as.data.frame(dfbeta(mod_45, infl = lm.influence(mod_45, do.coef = TRUE)))
dfb_46=as.data.frame(dfbeta(mod_46, infl = lm.influence(mod_46, do.coef = TRUE)))
dfb_47=as.data.frame(dfbeta(mod_47, infl = lm.influence(mod_47, do.coef = TRUE)))
dfb_48=as.data.frame(dfbeta(mod_48, infl = lm.influence(mod_48, do.coef = TRUE)))

#stargazer(dfb_41, title = "Influencia observaciones sobre los coeficientes", type = "text", 
#        covariate.labels=c("Mujer","Hombre"),out = "views/dfb_41.txt",notes = "Modelo 4.1")
#stargazer(dfb_42, title = "Influencia observaciones sobre los coeficientes", type = "text", 
#          covariate.labels=c("Intercepto","Edad","Edad2"),out = "views/dfb_42.txt",notes = "Modelo 4.2")
#stargazer(dfb_43, title = "Influencia observaciones sobre los coeficientes", type = "text", 
#          covariate.labels=c("Intercepto","Edad","Edad2","Hombre"),out = "views/dfb_43.txt",notes = "Modelo 4.3")
#stargazer(dfb_44,  title = "Influencia observaciones sobre los coeficientes", type = "text", 
#          covariate.labels=c("Intercepto","Edad","Edad2","Hombre","ln Educ"),out = "views/dfb_44.txt",notes = "Modelo 4.4")
#stargazer(dfb_45,  title = "Influencia observaciones sobre los coeficientes", type = "text", 
#          covariate.labels=c("Intercepto","Edad","Edad2","Hombre","primaria incompleta","Primaria completa",
#                             "secundaria incompleta","secundaria completa","Educ superior","Estrato 2",
#                             "Estrato 3","Estrato 4","Estrato 5","Estrato 6"),out = "views/dfb_45.txt",notes = "Modelo 4.5")
#stargazer(dfb_46,  title = "Influencia observaciones sobre los coeficientes", type = "text", 
#          covariate.labels=c("Intercepto","Edad","Edad2","Edad*Hombre","ln Educ"),out = "views/dfb_46.txt",notes = "Modelo 4.6")
#stargazer(dfb_47, title = "Influencia observaciones sobre los coeficientes", type = "text", 
#          covariate.labels=c("Intercepto","Edad","Edad2","Edad*Hombre","primaria incompleta","Primaria completa",
#                             "secundaria incompleta","secundaria completa","Educ superior","Estrato 2",
#                             "Estrato 3","Estrato 4","Estrato 5","Estrato 6"),out = "views/dfb_47.txt",notes = "Modelo 4.5")
#stargazer(dfb_48, type = "text")



dffits_41= as.data.frame(dffits(mod_41))
dffits_42= as.data.frame(dffits(mod_42))
dffits_43= as.data.frame(dffits(mod_43))
dffits_44= as.data.frame(dffits(mod_44))
dffits_45= as.data.frame(dffits(mod_45))
dffits_46= as.data.frame(dffits(mod_46))
dffits_47= as.data.frame(dffits(mod_47))
dffits_48= as.data.frame(dffits(mod_48))
dffits=cbind(dffits_41,dffits_42,dffits_43,dffits_44,dffits_45,dffits_46,dffits_47,dffits_48)
rm(dffits_41,dffits_42,dffits_43,dffits_44,dffits_45,dffits_46,dffits_47,dffits_48)

stargazer(dffits, type = "text",title = "Influencia observaciones sobre la predicción",
covariate.labels=c("Modelo 4.1","Modelo 4.2","Modelo 4.3","Modelo 4.4","Modelo 4.5","Modelo 4.6","Modelo 4.7","Modelo 4.8"),out = "views/dffits.txt")


  ## 4.4 Validacion cruzada
# Se usan los modelos 5 y 7 porque son los que tienen menor error 

ctrl <- trainControl(method = "LOOCV")
model1 <- train(ln_y~age+age2+Mujer+maxEducLevel+estrato1, data = GEIH_2018, method = "lm", trControl = ctrl)
model2 <- train(ln_y~age+age2+Mujer_age+Mujer_ed+regSalud, data = GEIH_2018, method = "lm", trControl = ctrl)

mdl_1_results=as.data.frame(table(model1$results['intercept'],model1$results['RMSE'],model1$results['Rsquared'],model1$results['MAE']))
mdl_1_results['Freq']<-mean(model1$pred$pred)
mdl_1_results<- rename(mdl_1_results, "Intercepto"="Var1","RMSE"="Var2","R^2"="Var3","MAE"="Var4","Prediccion promedio"="Freq")
sink("views/mdl_1_results_LOOCV.txt")
print(mdl_1_results)
sink() 




mdl_2_results=as.data.frame(table(model2$results['intercept'],model2$results['RMSE'],model2$results['Rsquared'],model2$results['MAE']))
mdl_2_results['Freq']<-mean(model2$pred$pred)
mdl_2_results<- rename(mdl_2_results, "Intercepto"="Var1","RMSE"="Var2","R^2"="Var3","MAE"="Var4","Prediccion promedio"="Freq")
sink("views/mdl_2_results_LOOCV.txt")
print(mdl_2_results)
sink() 

