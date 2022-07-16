##########################################################################
############ PROYECTO ECONOMETRÍA: DEMANDA DE LA GASOLINA EN MÉXICO ######
##########################################################################
"ALUMNO: VALLEJO RANGEL ALEJANDRO"

rm(list = ls())

install.packages("tidyverse")
install.packages("withr")
install.packages("ggstatsplot")
install.packages("dgof")
install.packages("moments")

library(withr)
library(tidyverse)
library(readxl)
library(dplyr)
library(pacman)
library(stargazer)
library(ggplot2)
library(ggstatsplot)
library(dgof) #para el SK Test
library(moments) #para sesgo y curtosis

"Librerías útiles para series de tiempo"

install.packages("tseries")
install.packages("astsa")
install.packages("timsac")
install.packages("MASS")
install.packages("mFilter")
install.packages("dynlm")
install.packages("kableExtra")
install.packages("mlogit")
install.packages("forecast")
install.packages("fpp2")
install.packages("normtest")
install.packages("ARDL")
install.packages("skedastic")
install.packages("bdsmatrix")
install.packages("RFGLS")


library(lubridate)
library(tidyverse)
library(car)
library(tseries)
library(astsa)
library(foreign)
library(timsac)
library(vars)
library(lmtest)
library(normtest)
library(mFilter)
library(dynlm)
library(nlme)
library(broom)
library(kableExtra)
library(knitr)
library(MASS)
library(mlogit)
library(dplyr)
library(tidyr)
library(forecast)
library(fpp2)
library(stats)
library(quantmod)
library(ARDL)
library(skedastic)
library(bdsmatrix)
library(RFGLS)


#Cargamos la base de datos
datos<- read_excel("CIDE Segundo Semestre/Econometría I/Demanda de la gasolina.xlsx")
View(datos)
attach(datos)
names(datos)

#Estadística descriptiva de las variables
"Ingreso per cápita"
summary(Ing)
range(Ing)
var(Ing)
sd(Ing)
CV_Ing<-sd(Ing)/mean(Ing)
CV_Ing
"Demanda de la gasolina"
summary(D)
range(D)
var(D)
sd(D)
CV_D<-sd(D)/mean(D)
CV_D
"Total de vehículos"
summary(Fv)
range(Fv)
var(Fv)
sd(Fv)
CV_Fv<-sd(Fv)/mean(Fv)
CV_Fv
"Precios de la gasolina"
summary(P)
range(P)
var(P)
sd(P)
CV_P<-sd(P)/mean(P)
CV_P

#Obtenemos los histogramas de las variables

par(mfrow=c(2,2))

hist(Ing, main="Ingreso per cápita",col="chocolate1",xlab="pesos",probability=TRUE)
curve(dnorm(x, mean=mean(Ing), sd(Ing)), add=TRUE)

hist(D,main="Demanda de la gasolina",col="chocolate2",xlab="Millones de litros",probability=TRUE)
curve(dnorm(x, mean=mean(D), sd(D)), add=TRUE)

hist(Fv, main="Total de vehículos",col="chocolate3",xlab="millones",probability=TRUE)
curve(dnorm(x, mean=mean(Fv), sd(Fv)), add=TRUE)

hist(P, main="Precios de la gasolina",col="chocolate4",xlab="pesos por litro",probability=TRUE)
curve(dnorm(x, mean=mean(P), sd(P)), add=TRUE)

par(mfrow=c(1,1))



#Creamos una serie de tiempo para todos los datos
datos.ts<-ts(datos, start=2000, frequency=4) #tenemos trimestres por eso es frequency=4
datos.ts #visualizar la serie de tiempo
#Serie de tiempo para cada variable
Ing.ts<-ts(datos.ts[,1],start=2000,frequency=4) #Ingreso per capita
D.ts<-ts(datos.ts[,2],start=2000,frequency=4)   #Demanda gasolina
Fv.ts<-ts(datos.ts[,3],start=2000,frequency=4)  #Flota vehicular
P.ts<-ts(datos.ts[,4],start=2000,frequency=4)   #Precio gasolinas

#Obtenemos las gráficas de las series de tiempo

par(mfrow=c(2,2))

plot(Ing.ts,main="Ingreso per cápita",ylab="[pesos]",col="red",lwd=3,xlab="")
plot(D.ts,main="Demanda de la gasolina",ylab="[Millones de litros]",col="blue",lwd=3,xlab="")
plot(Fv.ts,main="Total de vehículos",ylab="[Millones]",col="green",lwd=3,xlab="")
plot(P.ts,main="Precios de la gasolina",ylab="[pesos por litro]",col="magenta",lwd=3,xlab="")

par(mfrow=c(1,1))


#Renombremos las variables para usarlas más ágilmente: 

colnames(datos)<-c("PIB_capita","Demanda_gas","Flota","Precios_gas")

###############################################################################
#################### PRIMER MODELO: Modelo Estático ###########################
###############################################################################

#Efectuamos la regresión: 
reg_estatico<-lm(Demanda_gas~PIB_capita+Flota+Precios_gas,data=datos)
summary(reg_estatico)

#Probemos los supuestos:

"NO MULTICOLINEALIDAD"

car::vif(reg_estatico)


"NORMALIDAD DE LOS ERRORES"

residuales_estatico<-reg_estatico$residuals #obtenemos los residuales
skewness(residuales_estatico) #sesgo= 0?
kurtosis(residuales_estatico) #curtosis= 3?
"Obtenemos el histograma"
hist(residuales_estatico, main="Histograma errores modelo estático",col="cyan",probability=TRUE)
curve(dnorm(x, mean=mean(residuales_estatico), sd(residuales_estatico)), add=TRUE)
"Hacemos la prueba KS Test"
ks.test(residuales_estatico,"pnorm",mean=mean(residuales_estatico),sd=sd(residuales_estatico))
"Hacemos la prueba Shapiro-Wilk"
shapiro.test(residuales_estatico)
"Hacemos la prueba de Jarque-Bera"
jarque.bera.test(residuales_estatico)
"Cálculo Jarque-Bera"
JB_estatico= (skewness(residuales_estatico) **2)/6
JB_estatico= JB_estatico + (((kurtosis(residuales_estatico)-3)**2)/24)
JB_estatico=JB_estatico*length(residuales_estatico)
JB_estatico


"HOMOCEDASTICIDAD"

"Hacemos la prueba Breusch-Pagan" #librería lmtest es necesaria
bptest(reg_estatico)
"Hacemos la prueba de White"
bptest(reg_estatico, ~ PIB_capita*Flota +
                       PIB_capita*Precios_gas +
                       Flota*Precios_gas
                       + I(PIB_capita^2) +I(Flota^2)+I(Precios_gas^2),data=datos)
"Forma alternativa prueba de White"
skedastic::white_lm(reg_estatico)


"NO AUTOCORRELACIÓN DE LOS ERRORES"

"Hacemos la prueba Breusch-Godfrey"
bgtest(Demanda_gas~PIB_capita+Flota+Precios_gas,order=5,data=datos)
"Hacemos la prueba Durbin-Watson"
durbinWatsonTest(reg_estatico, max.lag=12)
"Gráfica de autocorrelación de los errores"
acf(residuales_estatico,lag=20,main="Autocorrelación de errores en modelo estático")


#Simulación: 

yhat_estatico<-matrix(fitted.values(reg_estatico))
yhat_estatico.ts<-ts(yhat_estatico, start=2000, frequency=4)
plot(D.ts,main="Demanda de la gasolina",ylab="[Millones de litros]",col="red",lwd=3,xlab="")
lines(yhat_estatico.ts,col="green",lwd=3)
legend(2002,3600, legend=c("Modelo Estático","Demanda gasolina"),col=c("green","red"),lty=1:1)

#Juntemos las gráficas JEJEJEJE

par(mfrow=c(2,2))


hist(residuales_estatico, main="Histograma errores modelo estático",col="cyan",probability=TRUE)
curve(dnorm(x, mean=mean(residuales_estatico), sd(residuales_estatico)), add=TRUE)

acf(residuales_estatico,lag=20,main="Autocorrelación de errores en modelo estático")

plot(D.ts,main="Simulación demanda de la gasolina",ylab="[Millones de litros]",col="red",lwd=3,xlab="")
lines(yhat_estatico.ts,col="green",lwd=3)
legend(2000,3700, legend=c("Modelo Estático","Demanda gasolina"),col=c("green","red"),lty=1:1,cex=0.65)

mtext("Figura 4: Gráficos del modelo estático", side=1, line=-1, outer=TRUE) #side=1 título va abajo


par(mfrow=c(1,1))


###############################################################################
#################### SEGUNDO MODELO: Modelo LN ################################
###############################################################################

#Obtenemos los logaritmos de las variables:

datosL=log(datos)
colnames(datosL)<-c("LPIB","LDemanda","LFlota","LPrecios")


#Efectuamos la regresión: 
reg_logaritmo<-lm(LDemanda~LPIB+LFlota+LPrecios,data=datosL)
summary(reg_logaritmo)

#Probemos los supuestos:

"NO MULTICOLINEALIDAD"

car::vif(reg_logaritmo)


"NORMALIDAD DE LOS ERRORES"

residuales_logaritmo<-reg_logaritmo$residuals #obtenemos los residuales
skewness(residuales_logaritmo) #sesgo= 0?
kurtosis(residuales_logaritmo) #curtosis= 3?
"Obtenemos el histograma"
hist(residuales_logaritmo, main="Histograma errores modelo logaritmico",col="cyan",probability=TRUE)
curve(dnorm(x, mean=mean(residuales_logaritmo), sd(residuales_logaritmo)), add=TRUE)
"Hacemos la prueba KS Test"
ks.test(residuales_logaritmo,"pnorm",mean=mean(residuales_logaritmo),sd=sd(residuales_logaritmo))
"Hacemos la prueba Shapiro-Wilk"
shapiro.test(residuales_logaritmo)
"Hacemos la prueba de Jarque-Bera"
jarque.bera.test(residuales_logaritmo)
"Cálculo Jarque-Bera"
JB_logaritmo= (skewness(residuales_logaritmo) **2)/6
JB_logaritmo= JB_logaritmo + (((kurtosis(residuales_logaritmo)-3)**2)/24)
JB_logaritmo=JB_logaritmo*length(residuales_logaritmo)
JB_logaritmo


"HOMOCEDASTICIDAD"

"Hacemos la prueba Breusch-Pagan" #librería lmtest es necesaria
bptest(reg_logaritmo)
"Hacemos la prueba de White"
bptest(reg_logaritmo, ~ LPIB*LFlota +
         LPIB*LPrecios +
         LFlota*LPrecios
       + I(LPIB^2) +I(LFlota^2)+I(LPrecios^2),data=datosL)
"Forma alternativa prueba de White"
skedastic::white_lm(reg_logaritmo)



"NO AUTOCORRELACIÓN DE LOS ERRORES"

"Hacemos la prueba Breusch-Godfrey"
bgtest(LDemanda~LPIB+LFlota+LPrecios,order=5,data=datosL)
"Hacemos la prueba Durbin-Watson"
durbinWatsonTest(reg_logaritmo, max.lag=12)
"Gráfica de autocorrelación de los errores"
acf(residuales_logaritmo,lag=20,main="Autocorrelación de errores en modelo logaritmico")



"Existen problemas de autocorrelación y heterocedasticidad, entonces se usa FGLS"
"FEASIBLE GENERALIZED LEAST SQUARES"

# reg_fgls<-fgls(LDemanda~LPIB+LFlota+LPrecios,data=datosL) #No se utiliza


#Simulación: 

yhat_logaritmico<-matrix(fitted.values(reg_logaritmo))
yhat_logaritmico<-exp(yhat_logaritmico) #convertimos de logaritmos a valores estaticos
yhat_logaritmico.ts<-ts(yhat_logaritmico, start=2000, frequency=4) #convertimos a serie de tiempo
plot(D.ts,main="Demanda de la gasolina",ylab="[Millones de litros]",col="red",lwd=3,xlab="")
lines(yhat_logaritmico.ts,col="blue",lwd=3)
legend(2002,3600, legend=c("Modelo Logaritmico","Demanda gasolina"),
       col=c("blue","red"),lty=1:1)

#Juntemos las gráficas JEJEJEJE

par(mfrow=c(2,2))

hist(residuales_logaritmo, main="Histograma errores modelo logaritmico",col="cyan",probability=TRUE)
curve(dnorm(x, mean=mean(residuales_logaritmo), sd(residuales_logaritmo)), add=TRUE)

acf(residuales_logaritmo,lag=20,main="Autocorrelación de errores en modelo logaritmico")

plot(D.ts,main="Demanda de la gasolina",ylab="[Millones de litros]",col="red",lwd=3,xlab="")
lines(yhat_logaritmico.ts,col="blue",lwd=3)
legend(2000,3700, legend=c("Modelo Logaritmico","Demanda gasolina"),
       col=c("blue","red"),lty=1:1,cex=0.65)

mtext("Figura 5: Gráficos del modelo logarítmico", side=1, line=-1, outer=TRUE) #side=1 título va abajo


par(mfrow=c(1,1))








####################### ANÁLISIS DE AUTOCORRELACIÓN ###########################

par(mfrow=c(2,2))

acf(datos$PIB_capita,lag=20,main="Autocorrelación PIB per capita")
acf(datos$Demanda_gas,lag=20,main="Autocorrelación Demanda gasolina")
acf(datos$Flota,lag=20,main="Autocorrelación Flota vehicular")
acf(datos$Precios_gas,lag=20,main="Autocorrelación Precios gasolina")

mtext("Figura 6: Autocorrelación de las variables", side=1, line=-1, outer=TRUE) #side=1 título va abajo

par(mfrow=c(1,1))

###############################################################################
#################### TERCER MODELO: Modelo ARDL ################################
###############################################################################

"Creamos una serie de tiempo para los datos en logaritmo"
datosL.ts<-ts(datosL, start=2000, frequency=4)
datosL.ts

################# Best ARDL model selection ####################################

AIC_selection <- auto_ardl(data=datosL.ts,max_order=5, selection="AIC", #AIC: Criterio de Información de Akaike
                           formula=LDemanda~LPIB+LFlota+LPrecios,
                           selection_minmax= "min",searchtype="horizontal",
                           start=1999, end=2017,grid=TRUE)


AIC_selection

#Efectuamos la regresión: 
reg_ARDL<- ardl(data=datosL.ts, order=AIC_selection$best_order,
                formula=LDemanda~LPIB+LFlota+LPrecios,
                start=1999, end=2017)  #IMPORTANTE: los datos van del 2000 al 2016, pero se tiene que escribir así para que el R entienda.

reg_ARDL #Así se comprueba que el R haya tomado el período correcto. En este caso dice que empezó en 2001(2) porque tenemos 5 rezagos:
         # Los rezagos son: 2000(1) 2000(2) 2000(3) 2000(4) 2001(1). Y es entonces que empieza a estimar a partir de 2001(2)
         # El período termina en 2016(4) lo cual concuerda con nuestros datos :)
summary(reg_ARDL)



#Probemos los supuestos:

"NO MULTICOLINEALIDAD"
#En un modelo ARDL por supuesto que la prueba VIF saldrá muy elevada para todas las variables. 
#En este contexto NO tiene mucho sentido llevar a cabo esta prueba
#Sabemos que no existe multicolinealidad porque ya hicimos esta prueba VIF para el modelo estático y
#para el modelo logaritmico. En ninguno de los dos casos hubo evidencia de multicolinealidad. 
#Dicha evidencia puede ser extendida para este modelo ARDL. 



"NORMALIDAD DE LOS ERRORES"

residuales_ARDL<-reg_ARDL$residuals #obtenemos los residuales
skewness(residuales_ARDL) #sesgo= 0?
kurtosis(residuales_ARDL) #curtosis= 3?
"Obtenemos el histograma"
hist(residuales_ARDL, main="Histograma errores modelo ARDL",col="cyan",probability=TRUE)
curve(dnorm(x, mean=mean(residuales_ARDL), sd(residuales_ARDL)), add=TRUE)
"Hacemos la prueba KS Test"
ks.test(residuales_ARDL,"pnorm",mean=mean(residuales_ARDL),sd=sd(residuales_ARDL))
"Hacemos la prueba Shapiro-Wilk"
shapiro.test(residuales_ARDL)
"Hacemos la prueba de Jarque-Bera"
jarque.bera.test(residuales_ARDL)
"Cálculo Jarque-Bera"
JB_ARDL= (skewness(residuales_ARDL) **2)/6
JB_ARDL= JB_ARDL + (((kurtosis(residuales_ARDL)-3)**2)/24)
JB_ARDL=JB_ARDL*length(residuales_ARDL)
JB_ARDL


"HOMOCEDASTICIDAD"

"Hacemos la prueba Breusch-Pagan" #librería lmtest es necesaria
bptest(reg_ARDL)
"Hacemos la prueba de White"
#White solo admite regresiones tipo "lm". Por eso es necesario reescribir la regresión ya que reg_ARDL es del tipo dynlm
reg_ARDL2<-lm(LDemanda~lag(LDemanda,1)+lag(LDemanda,2)+lag(LDemanda,3)+lag(LDemanda,4)+lag(LDemanda,5)
              +LPIB+LPrecios+lag(LPIB,1)+LFlota,data=datosL)
summary(reg_ARDL2)
skedastic::white_lm(reg_ARDL2)



"NO AUTOCORRELACIÓN DE LOS ERRORES"

"Hacemos la prueba Breusch-Godfrey"
bgtest(reg_ARDL,order=5,data=datosL.ts)
"Hacemos la prueba Durbin-Watson"
durbinWatsonTest(reg_ARDL, max.lag=12)
"Gráfica de autocorrelación de los errores"
acf(residuales_ARDL,lag=20,main="Autocorrelación de errores en modelo ARDL")


#Simulación: 

yhat_ARDL<-fitted.values(reg_ARDL)
yhat_ARDL<-exp(yhat_ARDL) #convertimos de logaritmos a valores lineales

plot(D.ts,main="Demanda de la gasolina",ylab="[Millones de litros]",col="red",lwd=3,xlab="")
lines(yhat_ARDL,col="violet",lwd=3)
legend(2002,3600, legend=c("Modelo ARDL","Demanda gasolina"),
       col=c("violet","red"),lty=1:1)

#Juntemos las gráficas JEJEJEJE

par(mfrow=c(2,2))

hist(residuales_ARDL, main="Histograma errores modelo ARDL",col="cyan",probability=TRUE)
curve(dnorm(x, mean=mean(residuales_ARDL), sd(residuales_ARDL)), add=TRUE)

acf(residuales_ARDL,lag=20,main="Autocorrelación de errores en modelo ARDL")

plot(D.ts,main="Demanda de la gasolina",ylab="[Millones de litros]",col="red",lwd=3,xlab="")
lines(yhat_ARDL,col="violet",lwd=3)
legend(2000,3700, legend=c("Modelo ARDL","Demanda gasolina"),
       col=c("violet","red"),lty=1:1,cex=0.6)


mtext("Figura 7: Gráficos del modelo ARDL", side=1, line=-1, outer=TRUE) #side=1 título va abajo


par(mfrow=c(1,1))







######################## COMPARACIÓN #########################################

"Grafica de simulación para los 3 modelos: "

plot(D.ts,main="Figura 8. Demanda de la gasolina",ylab="[Millones de litros]",col="red",lwd=3,xlab="")
lines(yhat_logaritmico.ts,col="blue",lwd=2,pch=15)
lines(yhat_estatico.ts,col="green",lwd=2)
lines(yhat_ARDL,col="violet",lwd=3)
legend(2000,3600, legend=c("Demanda gasolina","Modelo Logaritmico","Modelo Estático","Modelo ARDL"),
       col=c("red","blue","green","violet"),lty=1:1)


"Grafica de errores para los 3 modelos en el tiempo"

errores_estatico<- datos$Demanda_gas - yhat_estatico
errores_logaritmo<-datos$Demanda_gas - yhat_logaritmico
errores_ARDL<-datos$Demanda_gas[6:68] - yhat_ARDL #[6:68] para considerar los 5 rezagos que tiene yhat_ARDL y que los datos se acoplen

errores_estatico.ts<-ts(errores_estatico, start=2000, frequency=4) #convertimos a serie de tiempo
errores_logaritmo.ts<-ts(errores_logaritmo, start=2000, frequency=4) #convertimos a serie de tiempo

plot(errores_estatico.ts,main="Figura 9. Errores de estimación",ylab="[Millones de litros]",col="green",lwd=3,xlab="")
lines(errores_logaritmo.ts,col="blue",lwd=3,xlab="")
lines(errores_ARDL,col="red",lwd=3,xlab="")
legend(2000,-200, legend=c("Modelo Estático","Modelo Logaritmico","Modelo ARDL"),
       col=c("green","blue","red"),lty=1:1)

"Gráfica de errores al cuadrado para probar homocedasticidad"

plot(errores_estatico**2~datos$Demanda_gas, main="Figura 10. Prueba Homocedasticidad",ylab="Errores al cuadrado",xlab="Demanda gasolina",col="green",pch=16)
points(errores_logaritmo**2~datos$Demanda_gas,col="blue",pch=15)
points(errores_ARDL**2~datos$Demanda_gas[6:68],col="red",pch=17)
legend(x="center", legend=c("Modelo Estático","Modelo Logaritmico","Modelo ARDL"),
       col=c("green","blue","red"),lty=1:1)


