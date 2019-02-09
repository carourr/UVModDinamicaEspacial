#Clase Febrero 9 2019
#MODELACION DINAMICA ESPACIAL

require(TSA)
data(tempdub)
tempdub
class(tempdub)
tempdub=(tempdub-32)*5/9
plot(tempdub, ylab="Temperatura")
descomp=decompose(tempdub)
plot(descomp)

data("airmiles")
data("airpass")
data("gold")

#analizando estas series
airpass
class(airpass)
plot(airpass, ylab="Number of passagers")
descomp_airpass=decompose(airpass)
plot(descomp_airpass)
#Es evolutiva y estacionaria

airmiles
class(airmiles)
plot(airmiles, ylab="Number of miles")
descomp_airmiles=decompose(airmiles)
plot(descomp_airmiles)

gold
class(gold)
plot(gold, ylab="Gold")
descomp_gold=decompose(gold)
plot(descomp_gold)
#No se puede hacer la descomposicion porque el periodo analizado no es suficeintemente largo.

##Ejemplo Dengue y Malaria

library(readxl)
datos=read_excel("D:/Usuario/Documents/ModelacionDinamica/dengemalaria.xls")

denge=ts(data = datos$denge,start = 2004,end = 2019, frequency = 12)
plot(denge, ylab="Search Terms on Google: Denge")
decompose_denge=decompose(denge)
plot(decompose_denge)

malar=ts(data = datos$malaria,start = 2004,end = 2019, frequency = 12)
plot(malar, ylab = "Search Terms on Google: Malar")
decompose_malar=decompose(malar)
plot(decompose_malar$trend+decompose_malar$seasonal+decompose_malar$random,type="l")
lines(malar, type="l",col="red")
plot(decompose_malar)

#Graficas de Serie de Tiempo DINAMICOS
require(plotly)
datos$ID=1:182
datos$malaria_trend=c(NA,as.numeric(decompose_denge$trend))
datos$denge_trend=c(NA,as.numeric(decompose_malar$trend))


plot_ly(datos,x=~ID,y=~denge_trend,mode="lines")

plot_ly(datos, x=~ID, color=I("black"),mode="lines") %>% 
  add_lines(y=~denge_trend) %>%
  add_lines(y=~malaria_trend, color=I("red"))

#Alisado Exponencial Simple
data(tempdub)
tempdub<-(tempdub-32)*5/9
plot(tempdub, ylab="TemperaturaC", xlab="")
plot(tempdub, ylab="Temperatura")
descomp=decompose(tempdub)
plot(descomp)

alisim1= HoltWinters(tempdub, gamma=FALSE, beta=FALSE)
alisim1

pred.aliholt <- predict(alisim1, n.ahead=2, prediction.interval=TRUE)
plot(alisim1, pred.aliholt, ylab="Temperatura °C", xlab="", main="Predicciones de la Temperatura")
labs <- c("Valores observados", "Valores pronosticados", "IC de las predicciones")
legend("bottomleft", lty=rep(1,3), col=c("black", "red", "blue"), legend=labs, cex=0.8)


alisim2= HoltWinters(tempdub, gamma=TRUE, beta=FALSE)
alisim2

pred.aliholt <- predict(alisim2, n.ahead=2, prediction.interval=TRUE)
plot(alisim2, pred.aliholt, ylab="Temperatura °C", xlab="", main="Predicciones de la Temperatura")
labs <- c("Valores observados", "Valores pronosticados", "IC de las predicciones")
legend("bottomleft", lty=rep(1,3), col=c("black", "red", "blue"), legend=labs, cex=0.8)

alisim3= HoltWinters(tempdub, gamma=TRUE, beta=TRUE)
alisim3

pred.aliholt <- predict(alisim3, n.ahead=2, prediction.interval=TRUE)
plot(alisim2, pred.aliholt, ylab="Temperatura °C", xlab="", main="Predicciones de la Temperatura")
labs <- c("Valores observados", "Valores pronosticados", "IC de las predicciones")
legend("bottomleft", lty=rep(1,3), col=c("black", "red", "blue"), legend=labs, cex=0.8)

#oMITIENDO UNOS VALORES: HACIENDO UNA VALIDACION CRUZADA



