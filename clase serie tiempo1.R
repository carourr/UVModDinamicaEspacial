require(TSA)

##Ejemplo Temperatura
data(tempdub)
tempdub
class(tempdub)
tempdub=(tempdub-32)*5/9
plot(tempdub, ylab="Temperatura (C)", xlab="")
descomp=decompose(tempdub)
plot(descomp)

##Ejemplo Viajes Avion
data(airmiles)
airmiles
plot(airmiles, ylab="Viajes en Avion", xlab="")
descomp2=decompose(airmiles)
plot(descomp2)

##Ejemplo Viajes Avion2
data(airpass)
airpass
plot(airpass, ylab="Viajes en Avion2", xlab="")
descomp3=decompose(airpass)
plot(descomp3)

##Ejemplo Oro
data(gold)
gold
plot(gold, ylab="Precio Oro", xlab="")
descomp4=decompose(gold) ##no se puede por la longitud de la serie (menos de un a??o)
plot(descomp4)

##Ejemplo Dengue y Malaria
library(readxl)
datos = read_excel("Desktop/multiTimeline.xls")

dengue=ts(data = datos$dengue,start = 2004,end = 2019,frequency = 12)
plot(dengue,ylab="Consultas Dengue (Google)")
descomp5=decompose(dengue)
plot(descomp5)

malaria=ts(data = datos$malaria,start = 2004,end = 2019,frequency = 12)
plot(malaria,ylab="Consultas Malaria (Google)")
descomp6=decompose(malaria)
plot(descomp6$trend)
plot(descomp6$trend+descomp6$seasonal+descomp6$random,type="l")
lines(malaria,type="l",col="red")
plot(descomp6)


##Graficos de Serie de Tiempo (Dinamicos)
require(plotly)
datos$ID=1:182
datos$malaria_trend=c(NA,as.numeric(descomp6$trend))
datos$dengue_trend=c(NA,as.numeric(descomp5$trend))

plot_ly(datos, x = ~ID, y = ~dengue_trend,mode="lines")

plot_ly(datos, x = ~ID, color = I("black"),mode="lines") %>%
  add_lines(y = ~dengue_trend) %>%
  add_lines(y = ~malaria_trend, color = I("red"))


