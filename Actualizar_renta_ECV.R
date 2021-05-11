#install.packages("tidyverse")
#install.packages("plyr")
library(tidyverse)




#Web de donde se extraen los ficheros de la ECV
directorio <- "https://www.ine.es/ftp/microdatos/ecv/ecv_b2013/"




##Define functions
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

substrLeft = function (string, char) {
    substr(string,1,nchar(string)-char)
}

shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}






#Income datasets
#Longitudinal

for (anno in 2012:2018)
{

anno_j = as.numeric(substrRight(anno, 2))
anno_j = ifelse(anno_j<10,paste("0",as.numeric(substrRight(anno, 2)),sep = ""),paste(as.numeric(substrRight(anno, 2)),sep = ""))

anno3=anno-3


#Debido a los cambios de formato en la forma de nombrar los ficheros longitudinales hay que cambiar en 2012 y 2013 la forma de importar los ficheros
#Si el usuario desea importar datos longitudinales previos a 2012 (por ejemplo, el fichero 2008-2011) hay que consultar el nombre especifico de las carpetas comprimidas
if (anno==2012) {

temp <- tempfile(fileext = ".zip")
download.file(paste(directorio,"periodo_",anno3,"-",anno,".zip",sep=""),temp)
temp2 <- tempfile()
datos_ecv_long <- unzip(temp,exdir=temp2)

es <- read.csv(unz(datos_ecv_long[2], paste("es",anno_j,"h.csv",sep="")))

unlink(temp)
unlink(temp2)


} else if (anno==2013) {

temp <- tempfile(fileext = ".zip")
download.file(paste(directorio,"periodo_",anno3,"-",anno,".zip",sep=""),temp)
temp2 <- tempfile()
datos_ecv_long <- unzip(temp,exdir=temp2)

es <- read.csv(unz(datos_ecv_long[1], paste("es",anno_j,"h.csv",sep="")))

unlink(temp)
unlink(temp2)


} else {

temp <- tempfile()
download.file(paste(directorio,"periodo_",anno3,"-",anno,".zip",sep=""),temp)

es <- read.csv(unzip(temp,files=paste("es",anno_j,"h.csv",sep="")))

unlink(temp)

}


assign(paste("es_", anno, sep = ""), es)

}




#Juntamos todos los datasets de datos longitudinales
agregar <- "es_"

df_list <- mget(ls(pattern = agregar))
Datos_long<- plyr::rbind.fill(df_list)


#Eliminar datos repetidos 
Datos_long <- unique(Datos_long)

#Seleccionar variables que vamos a utilizar para identificar los encuestados que forman parte de los ficheros
#de los datos longitudinales y transversales
Datos_long<-select(Datos_long,HB010,HB020,HB030,HB050,HB100,HY020,HY022,HY023,HY030N,HY040N,HY050N,HY060N,HY070N,HY080N,HY090N,HY100N,
	               HY110N,HY120N,HY130N,HY145N,HY010,HY040G,HY050G,HY060G,HY070G,HY080G,HY090G,HY100G,HY110G,HH010,HH030)




for (anno in 2009:2018)
{

#Clasificar cada conjunto de datos longitudinaes para que contenga la información referente al año en el que se realiza la encuesta
#Lo que obtenemos es cada vuelta del loop un fichero con observaciones referentes al año en que se realiza la encuesta
DD_long <- filter(Datos_long,HB010==anno)


#Eliminar observaciones repetidas
DD_long<-DD_long%>%distinct(HB010,HB020,HB030,HB050,HB100,HY020,HY022,HY023,HY030N,HY040N,HY050N,HY060N,HY070N,HY080N,HY090N,HY100N,HY110N,
	                        HY120N,HY130N,HY145N,HY010,HY040G,HY050G,HY060G,HY070G,HY080G,HY090G,HY100G,HY110G,HH010,HH030,.keep_all=TRUE)


#Para diferenciar las variables de identificación en los ficheros longitudinales de los transversales le añadiremos el prefijo P_a la variable de identificación en los ficheros longittudinales
DD_long['Iden']=paste('P_',DD_long$HB030, sep="")

DD_long<-select(DD_long,HB010,HB020,Iden,HB050,HB100,HY020,HY022,HY023,HY030N,HY040N,HY050N,HY060N,HY070N,
                HY080N,HY090N,HY100N,HY110N,HY120N,HY130N,HY145N,HY010,HY040G,HY050G,HY060G,HY070G,HY080G,HY090G,HY100G,HY110G,HH010,HH030)


names(DD_long)[names(DD_long) == 'Iden'] <- 'HB030'





#Transversal
#Importar datos
anno_j = as.numeric(substrRight(anno, 2))
anno_j = ifelse(anno_j<10,paste("0",as.numeric(substrRight(anno, 2)),sep = ""),paste(as.numeric(substrRight(anno, 2)),sep = ""))

temp <- tempfile(fileext = ".zip")
download.file(paste(directorio,"datos_",anno,".zip",sep=""),temp)
temp2 <- tempfile()
datos_ecv <- unzip(temp,exdir=temp2)

Datos_transver <- read.csv(unz(datos_ecv[1], paste("esudb",anno_j,"h.csv",sep="")))

unlink(temp)
unlink(temp2)



#Seleccionar variables que vamos a utilizar para identificar los encuestados que forman parte de los ficheros transversales
Datos_transver<-select(Datos_transver,HB010,HB020,HB030,HB050,HB100,HY020,HY022,HY023,HY030N,HY040N,HY050N,HY060N,HY070N
,HY080N,HY090N,HY100N,HY110N,HY120N,HY130N,HY145N,HY010,HY040G,HY050G,HY060G,HY070G,HY080G,HY090G,HY100G,HY110G,HH010,HH030)
#Eliminar repetidos
Datos_transver<-Datos_transver%>%distinct(HB010,HB020,HB030,HB050,HB100,HY020,HY022,HY023,HY030N,HY040N,HY050N,HY060N,HY070N
,HY080N,HY090N,HY100N,HY110N,HY120N,HY130N,HY145N,HY010,HY040G,HY050G,HY060G,HY070G,HY080G,HY090G,HY100G,HY110G,HH010,HH030,.keep_all=TRUE)






#Juntar conjunto de datos transversales y longitudinales
Obtener_Referencia <- rbind(DD_long,Datos_transver)


#Obtener las relaciones entre identificadores de los ficheros transversales y longitudinales
Sep_Referencia <- aggregate(HB030 ~ ., Obtener_Referencia, toString)

Sep_Referencia <- separate(Sep_Referencia, 'HB030', paste("HB030", 1:8, sep="_"), sep=",", extra="drop")
Sep_Referencia<- select(Sep_Referencia,HB010,HB030_1,HB030_2,HB030_3,HB030_4)

#Quitar observaciones que no tienen un homólogo en el fichero transversal
Sep_Referencia <- Sep_Referencia[!is.na(Sep_Referencia$HB030_2), ]
Sep_Referencia$HB030_2 <- replace(Sep_Referencia$HB030_2, grep("P_", Sep_Referencia$HB030_2), NA)
Sep_Referencia$HB030_2 <- apply(Sep_Referencia[,c("HB030_2","HB030_3")], 1, function(x) x[!is.na(x)][1])

#Seleccionar año y los identificadores del fichero longitudinal (HB030_1) y transversal (HB030_2)
Ref_final <- select(Sep_Referencia,HB010,HB030_1,HB030_2)

Ref_final <- Ref_final[!is.na(Ref_final$HB030_2), ]


assign(paste("Referencia_", anno, sep = ""), Ref_final)

rm(Ref_final,Sep_Referencia,Obtener_Referencia,DD_long,esudb18h2,Datos_transver,agregar,Ref_final)

}









###########

#Income datasets
#Longitudinal
#Obtener los datos de la variable renta de forma que correspondan con el año en el cual se completó la encuesta y no con el año anterior como es por defecto en los ficheros


#importar datos longitudinales de nuevo
for (anno in 2012:2018)
{

anno_j = as.numeric(substrRight(anno, 2))
anno_j = ifelse(anno_j<10,paste("0",as.numeric(substrRight(anno, 2)),sep = ""),paste(as.numeric(substrRight(anno, 2)),sep = ""))

anno3=anno-3

if (anno==2012) {

temp <- tempfile(fileext = ".zip")
download.file(paste(directorio,"periodo_",anno3,"-",anno,".zip",sep=""),temp)
temp2 <- tempfile()
datos_ecv_long <- unzip(temp,exdir=temp2)


es_L <- read.csv(unz(datos_ecv_long[2], paste("es",anno_j,"h.csv",sep="")))

unlink(temp)
unlink(temp2)




} else if (anno==2013) {

temp <- tempfile(fileext = ".zip")
download.file(paste(directorio,"periodo_",anno3,"-",anno,".zip",sep=""),temp)
temp2 <- tempfile()
datos_ecv_long <- unzip(temp,exdir=temp2)


es_L <- read.csv(unz(datos_ecv_long[1], paste("es",anno_j,"h.csv",sep="")))



unlink(temp)
unlink(temp2)




} else {

#Normal
temp <- tempfile()
download.file(paste(directorio,"periodo_",anno3,"-",anno,".zip",sep=""),temp)

es_L <- read.csv(unzip(temp,files=paste("es",anno_j,"h.csv",sep="")))

unlink(temp)



}


assign(paste("es_L_", anno, sep = ""), es_L)

}


#Agregar datos y eliminar datos repetidos
agregar <- "es_L_"

df_list <- mget(ls(pattern = agregar))
Datos_long<- plyr::rbind.fill(df_list)

Datos_long <- unique(Datos_long)

Datos_long<- Datos_long[with(Datos_long, order(HB030, HB010)), ]


#Calculamos una variable renta con una definición particular, que incluye alquiler imputado, autoconsumo y deducidos intereses de prestamos hipotecarios
Datos_long$vhRentaAIa <- Datos_long$HY020 + Datos_long$HY030N+Datos_long$HY170N-Datos_long$HY100N


#Laggear la variable renta de manera que asocie con el año en el que se realiza la en cuesta a cada hogar
Datos_long$vhRentaAIa <- shift(Datos_long$vhRentaAIa, 1)

Datos_long_lagged <- Datos_long %>%
   group_by(HB030) %>%
   filter(row_number()!=n()|n()==1)

Datos_long_lagged <- Datos_long_lagged %>% group_by(HB030) %>% filter(n()>1)

#Seleccionar variables identificadores y la nueva variable renta que hemos definidip
Datos_long_lagged <- select(Datos_long_lagged,HB010,HB030,vhRentaAIa)


#Volver a formatear la variable identificador para distinguirla de los datos trasnversales
Datos_long_lagged['Iden']=paste('P_',Datos_long_lagged$HB030, sep="")
Datos_long_lagged$HB030 <- NULL
names(Datos_long_lagged)[names(Datos_long_lagged) == 'Iden'] <- 'HB030'
Datos_long_lagged <- select(Datos_long_lagged,HB010,HB030,vhRentaAIa)


#Clasificar datos laggeados en función del año
for (anno in 2009:2018)
{

Datos_long_lagged_anno <- filter(Datos_long_lagged,HB010==anno)

assign(paste("Renta_long_", anno, sep = ""), Datos_long_lagged_anno)

}

rm(Datos_long_lagged_anno,Datos_long,Datos_long_lagged)






##Mezclar los datos transversales con los datos longitudianales (con la variable renta correctamente asignada, eso es, laggeada)
#Lo que obtendremos será un conjunto de datos para cada año con las variables:
#HB010 que define el año en que se realiza la encuesta al hogar
#HB030_1 el identificador de cada hogar en el fichero de datos longitudinal
#HB030_2 el identificador de cada hogar en el fichero de datos transversal
#vhRentaAIa la variable que informa de la renta generada por cada hogar en el mismo año en el que fue encuestado

for (anno in 2009:2017)
{


Referencia <- get(paste("Referencia_", anno, sep = ""))
Renta_long <- get(paste("Renta_long_", anno, sep = ""))


Referencia_filtrada <- merge(Referencia, Renta_long, by.x="HB030_1", by.y="HB030")

Referencia_filtrada['HB010'] <- anno

Referencia_filtrada <- select(Referencia_filtrada,HB010,HB030_1,HB030_2,vhRentaAIa)

assign(paste("Relacion_", anno, sep = ""), Referencia_filtrada)

#UUU$HB030_3 <- as.numeric(UUU$HB030_3)

#LOLO2 <- merge(UUU, LLL, by.x="HB030_3", by.y="HB030")


}

rm(list = ls()[grep("Relacion_", ls(),invert=TRUE)])

