#Clase Febrero 9 2019
#MODELACION DINAMICA ESPACIAL

require(TSA)
data(tempdub)
tempdub
class(tempdub)
tempdub=(tempdub-32)*5/9
plot(tempdub, ylab="Temperatura")
git add Rplot.png
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
plot(denge, ylab="Search Terms on Google")
decompose_dengmal=decompose(denge)
plot(decompose_dengmal)
