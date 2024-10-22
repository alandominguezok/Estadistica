########################
#  Practico 5 - Ej 1 y 2
########################

rm(list = ls()) #Removemos todos los objetos


#-------------
#Ejercicio 1
#-------------

tiempo <- c(1.17, 1.61, 1.16, 1.38, 3.53, 1.23, 0.82, 0.96, 2.01, 0.15, 2.11, 0.71, 0.02, 1.59, 0.19, 1.91, 2.16, 0.92, 0.75, 2.59, 3.07, 1.1, 3.76, 0.47, 4.75)

sort(tiempo)
#c) Una opción es construir intervalos de longitud lint

lint<- round( (max(tiempo)-min(tiempo))/5,2)
lint


#Construimos una tabla "a mano" con los siguientes intervalos 

a1<- min(tiempo)

a2<-a1+lint

a3<- a2+lint

a4<- a3+lint

a5 <- a4+lint

a6<- a5+lint 

a1; a2; a3; a4; a5; a6

#Para la tabla de frecuencias 
#Calcula las frecuencias en cada intervalo
f1<- sum(  a1<=   sort(tiempo) & sort(tiempo)< a2  )
f2 <-  sum(  a2<=   sort(tiempo) & sort(tiempo)< a3  )
f3 <-  sum(  a3<=   sort(tiempo) & sort(tiempo)< a4  )
f4 <-  sum(  a4<=   sort(tiempo) & sort(tiempo)< a5  )
f5 <-  sum(  a5<=   sort(tiempo) & sort(tiempo)<= a6  )

f1; f2; f3; f4; f5


#d) 

( sum( sort(tiempo) < 3.8)/length(tiempo))*100

sum( sort(tiempo) >=1.92)


#e) 
f3/length(tiempo)
4/25

# g) Resumen estadístico

summary(tiempo)

sd(tiempo)# desvío

cvar=  ( sd(tiempo)/mean(tiempo))*100 #Coeficiente de variación
cvar

mad(tiempo)#mediana de los desvíos absolutos

quantile(tiempo, .75)


dinter<- ( quantile(tiempo, .75) - quantile(tiempo, .25) )/1.349 # Distancia intercuartílica con factor de corrección

dinter



#h) Diagrama de caja e histograma

boxplot(tiempo, main="Diagrama de caja para 25 tiempos de procesamiento")

hist(tiempo, breaks=4, main="Histograma para 25 tiempos de procesamiento")



#-------------  
#Ejercicio 2
#-------------
#https://rdrr.io/rforge/Lock5Data/man/StudentSurvey.html

Alumnos=read.csv("./Escritorio/Estadistica/basedatos.csv",header=TRUE) 
attach(Alumnos)

#fix(Alumnos) #Vemos la base de datos
#Primer acercamiento a los datos

summary(Alumnos)
dim(Alumnos)
#moda
library(modeest)
moda_anio <- mfv(Alumnos$Anio); moda_anio

#otra forma de calcular la moda sin llamar a la biblioteca modeest
mode <- function(x) {
  return(names(which.max(table(x))))
}

moda_genero <- mode(Alumnos$Genero); moda_genero
moda_anio <- mode(Alumnos$Anio); moda_anio

#a) 
names(Alumnos) #Muestra los nombres de las variables (columnas)

#b)
#Asignamos los nombres de las variables en castellano
colnames(Alumnos)<-c("Anio","Genero","Fuma","Premio","MayorSAT","Ejercicio","TV","Altura","Peso","Hermanos","Orden de Nac.","LenguaSAT","MatSAT","SAT","GPA","Pulso","Piercings")
names(Alumnos)

#c) 

Altura2 <- 2.54* Alumnos$Altura; Altura2  #transformacion de pulgadas a centímetros
mean(Altura2, na.rm=TRUE) #na.rm=TRUE remueve los NA o not avaiable 

#Si quisieramos agregar una nueva columna a la base de datos original

#altura2 = data.frame(Altura2); altura2
#names(altura2)<- 'altura_cm'
#altura2
#concatenamos ambos dataframe
#alumnos1= data.frame(Alumnos, altura2); alumnos1


# En general si queremos filtrar los NA en toda la base

Alumnos_NA <- na.omit(Alumnos); Alumnos_NA
dim(Alumnos_NA)
#fix(Alumnos_NA)

#De ahora en m?s accedemos a las variables sin llamar al  data frame 

attach(Alumnos)

#d)
table(Alumnos$Anio) #Aparece una frecuencia de dos que no pertenece a ninguna categoría
counts.anio <- table(Alumnos$Anio); counts.anio
pie(counts.anio, col=c("blue","green","red", "brown" ), main="Grafico de Torta para la variable Anio")
Alumnos$Anio

mean(Anio)

#observar que en Año hay dos celdas con "", que filtraremos

library(tidyverse)#para utilizar el operador pipe %>% y filtrar
Alumnosfilt1<- Alumnos%>%filter(Anio != "")# La variable Anio tiene celdas con """"
Aniofilt<- Alumnosfilt1$Anio
table(Aniofilt)
counts.Aniofilt <- table(Aniofilt)
x11()#para que muestre los graficos en otra ventana
pie(counts.Aniofilt, col=c("blue","green","red", "brown" ), main="Gráfico de Torta para Año")


#e) Genero y fuma

Fuma2 <- gsub("Yes", "Si", Alumnos$Fuma); Fuma2
counts <- table(Fuma2,Genero); counts
prop<- prop.table(table(Fuma2,Alumnos$Genero), margin = 2) * 100; prop


barplot(counts,col=c("blue","red"),main="Fuma vs. Genero" ) 

#mas completo, agregamos porcentajes

bp<- barplot(prop,col=c("blue","red"),main="Distribucion de Fuma segun Genero (%)", beside=TRUE, axes=FALSE, xlab="Genero" , ylab="Frecuencia (%)",  ylim=c(0,100)) 
axis(2, at=seq(0,100,10))
legend("topright", legend=c("No", "Si"), bty="n", fill=c("blue", "red"))
text(bp, 0, round(prop, 1), cex=1, pos=3)


#Observar que la proporcion ( 0.1398964) de hombres (M) que fuma es mayor que la proporcion (16/(16+153))de mujeres  que fuma(F)


#f)

boxplot(Altura)# Hay un valor atipico que tambien puede observarse haciendo

quantile(Altura,.75, na.rm=TRUE)+(3/2)*IQR(Altura, na.rm=TRUE)

sort(Altura)

#El dato 83 sobrepasa este límite


# g) Altura versus anio

boxplot(Altura~Anio) #comparar y discutir; observar que en Año hay dos celdas con ""

library(tidyverse)#para utilizar el operador pipe %>% y filtrar

Alumnosfilt1<- Alumnos%>%filter(Anio != "")# La variable año tiene celdas con """"

boxplot(Alumnosfilt1$Altura~Alumnosfilt1$Anio,  xlab="A?o" , ylab="Altura") #comparar y discutir; observar que hay dos celdas con ""


#h) Altura versus Peso
cor(Altura, Peso, use="complete.obs") #con esto calculamos la correlación entre el peso y la altura

plot(Alumnos$Altura, Alumnos$Peso, 
     main="Relación entre Altura y Peso", 
     xlab="Altura", 
     ylab="Peso", 
     pch=19, col="blue")
abline(lm(Peso ~ Altura, data=Alumnos), col="red")



#-------------  
#Ejercicio 3
#-------------
numCuentasNuevas <- c(43, 37, 50, 51, 58, 105, 52, 45, 45, 10)

#Ordenamos las frecuencias de cuentas nuevas
numCuentasNuevas <- sort(numCuentasNuevas)

#Resumen
summary (numCuentasNuevas) 

#desvio estandar
sd(numCuentasNuevas) 

# Coeficiente de variación
cvar2 =  (sd(numCuentasNuevas)/mean(numCuentasNuevas))*100 
cvar2

varianza = var(numCuentasNuevas)
varianza
# Mediana de los desvíos absolutos
mad(numCuentasNuevas) 

#Cuantil .75
q3 = quantile (numCuentasNuevas, .75)
#Cuantil .25
q1 = quantile (numCuentasNuevas, .25)
# Distancia intercuartílica con factor de corrección
dinter2 <- (q3 - q1)/1.349 
dinter2

#Valores Atípicos podemos con el diagrama de caja encontrarlos
boxplot(numCuentasNuevas)

# por definicion
limInf = q1 - (3/2)* (q3 - q1)
limSup = q3 + (3/2)* (q3 - q1)
limInf;limSup
valores_atipicos <- numCuentasNuevas[numCuentasNuevas < limInf | numCuentasNuevas > limSup]
valores_atipicos


#-------------  
#Ejercicio 4
#-------------
numUsuariosConec <- c(17.2, 22.1, 18.5, 17.2, 18.6, 14.8, 21.7, 15.8, 16.3, 22.8,
                      24.1, 13.3, 16.2, 17.5, 19, 23.9, 14.8, 22.2, 21.7, 20.7,
                      13.5, 15.8, 13.1, 16.1, 21.9, 23.9, 19.3, 12.0, 19.9, 19.4,
                      15.4, 16.7, 19.5, 16.2, 16.9, 17.1, 20.2, 13.4, 19.8, 17.7,
                      19.7, 18.7, 17.6, 15.9, 15.2, 17.1, 15.0, 18.8, 21.6, 11.9)

#Ordenamos las frecuencias de Usuarios conectados
numUsuariosConec <- sort(numUsuariosConec)

#Calcular la media, la varianza y el desvıo estandar del numero de usuarios conectados 
summary(numUsuariosConec)

varianza2 = var(numUsuariosConec)
varianza2

sd (numUsuariosConec)

rango = max(numUsuariosConec) - min(numUsuariosConec)
rango

boxplot(numUsuariosConec)

# por definicion valores atipicos
#Cuantil .75
qu3 = quantile (numUsuariosConec, .75)
#Cuantil .25
qu1 = quantile (numUsuariosConec, .25)

limInf2 = qu1 - ((3/2)*(qu3 - qu1))
limSup2 = qu3 + ((3/2)*(qu3 - qu1))
limInf;limSup
valores_atipicos <- numUsuariosConec[numUsuariosConec < limInf2 | numUsuariosConec > limSup2]
valores_atipicos

distanciaInter = qu3 - qu1
distanciaInter

hist(numUsuariosConec)