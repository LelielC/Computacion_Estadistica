#TAREA II Computación Estadística
## Venus Celeste Puertas
## Daniel Mesías
## Julieth Sofia Moreno

##### Ejercicio Películas
setwd('C:\\Users\\User\\OneDrive\\Documentos\\Universidad\\Computación Estadística\\Archivos')

library(dplyr)
source("mismacros.txt")
library("readr")

#Importar los datos
movies<-read_delim('movie_avshotlength.csv',delim=',',skip=1,
                   col_types = cols('c',col_date('%Y'),'?','c','-'),
                   col_names = c('Título','Año','Director','País'))
str(movies)

#El conjunto tiene 11001 filas

missings <- function(x) return(sum(is.na(x)))
apply(movies,2,missings)

#Hay 1367 valores faltantes en la columna País, 5 en la columna año y 15 en la columna director

library(stringr)

palabras <- with(movies,str_count(Director," ") + 1)
table(palabras)

# 9736 películas tienen 2 directores.

Mas3 <- with(movies,str_count(Director," ") >= 3)
table(Mas3)

#9768 películas tienen 3 o más directores
nums<-subset(movies,str_detect(Título,'\\d'))
str(nums)
#351 películas incluyen números en su título, su información está almacenada en nums
punc<- subset(movies,str_detect(Título,'[.,\\/#!$%\\^&\\*;:{}=\\-_`~()”“"…]'))
str(punc)
# 3690 películas incluyen signos de puntuación, su información está almacenada en punc


#Nuevo conjunto de datos con nueva variable Director2
new <- subset(movies,str_count(Título," ")+1==1)
new<-within(new,{
  Director2 <- str_to_lower(Director)
  Director2 <- str_to_title(Director2)
  Director2<-paste(str_extract(Director2,' \\S+'),str_extract(Director2,'\\S+'))
  Director2 <- str_replace_all(Director2,",","")
  Director2 <- str_squish(Director2)
})
new


# Películas dirigidas por Kathryn Bigelow
Bigelow<-subset(movies,str_detect(Director,"Bigelow, Kathryn"))
str(Bigelow)
#En total hay 6 películas dirigidas por Kathryn Bigelow

#Películas dirigidas por Martin Scorsese
Scorsese<-subset(movies,str_detect(Director,"Scorsese, Martin"))
str(Scorsese)
#19 películas fueron dirigidas por Scorsese


#Películas argentinas
arg<-subset(movies,str_detect(País,'ARG'))
str(arg)
#16 películas son de Argentina

#Películas Mexicanas
mex<-subset(movies,str_detect(País,'MEX'))
str(mex)
# 20 películas son Mexicanas



###### Ejercicio Exportaciones

library(readxl) # Libreria para poder cargar el archivo excel
library(dplyr) # libreria para poder cargar mismacros
source("mismacros.txt")# Cargamaos el archivo mismacros.txt

#Cargar el conjunto de datos exportaciones.xlsx
Expo<-read_excel("exportaciones.xlsx", col_names = TRUE, sheet = 1)
str(Expo)
#porcentaje del volumen de las exportaciones de café en 2018 se hicieron durante el mes de agosto
mytable(Volumen~Mes, data = Expo, subset = c(Ano=="2018" & Producto=="Café"), ord = "level", cum = F)
#### El porcentaje fue 9.25%

#porcentaje del volumen de las exportaciones de café en 2018 se hicieron durante el primer trimestre del año
mytable(Volumen~Mes, data = Expo, subset = c(Ano=="2018" & Producto=="Café"), ord = "level")
#### El porcentaje fue 26.60%

#porcentaje del valor de las exportaciones de café en 2018 se hicieron durante el mes de septiembre
mytable(Valor~Mes, data = Expo, subset = c(Ano=="2018" & Producto=="Café"), ord = "level", cum = F)
#### El porcentaje fue 7.63%

#porcentaje del valor de las exportaciones de café en 2018 se hicieron durante el cuarto trimestre del año
mytable(Valor~Mes, data = Expo, subset = c(Ano=="2018" & Producto=="Café"), ord = "-level")
#### El porcentaje fue 25.65%

#porcentaje del volumen de las exportaciones de flores en Antioquia en 2019 se hicieron durante el primer trimestre del año
mytable(Volumen~Mes, data = Expo, subset = c(Ano=="2019" & Producto=="Flores" & Departamento=="Antioquia"), ord = "level")
#### El porcentaje fue de 26.16

#porcentaje del volumen de las exportaciones de flores durante el primer trimestre del 2019 se hicieron desde Antioquia
mytable(Volumen~Departamento, data = Expo, subset = c(Mes<=3 & Ano=="2019" & Producto=="Flores"), ord = "level")
#### El porcentaje fue 27.32%

#porcentaje del valor de las exportaciones de flores en Antioquia en 2019 se hicieron durante el tercer trimestre del año
mytable(Valor~Mes, data = Expo, subset = c(Ano=="2019" & Producto=="Flores" & Departamento=="Antioquia"), ord = "level", cum = T)
#### El porcentaje fue 8.49+7.22+7.38= 23.09%

#porcentaje del valor de las exportaciones de flores durante el tercer trimestre del 2019 se hicieron desde Antioquia
mytable(Valor~Departamento, data = Expo, subset = c(7<=Mes & Mes<=9 & Ano=="2019" & Producto=="Flores"), ord = "level", cum = F)
#### El porcentaje fue 19.13%



##### Ejercicio créditos
help("CASdatasets")
#install.packages("xts")
#install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
library(CASdatasets)
data(credit)
#1
fac=table(credit$class)
# el 30% se consideran como creditos malos del total
Porcentajes=fac*100/nrow(credit)
Porcentajes
#2
dim(credit)
PCredM<-0
PCredB<-0

for (i in 1:dim(credit)[1]) {
  if (credit$class[i]==1){
    PCredM=PCredM+credit$credit_amount[i]
  }else {
    PCredB=PCredB+credit$credit_amount[i]
  }
}
PorcentajeValorCreditosMalos=PCredM*100/sum(credit$credit_amount)
PorcentajeValorCreditoBuenos=PCredB*100/sum(credit$credit_amount)
#El porcentaje del valor total de creditos malos es 36.11571%
#Historial de crédito
#A30 : no se tomaron créditos/todos los créditos se devolvieron debidamente
#A31 : todos los créditos en este banco reembolsados debidamente
#A32: créditos existentes reembolsados debidamente hasta ahora
#A33 : retraso en el pago en el pasado
#A34 : cuenta crítica/otros créditos existentes (no en este banco)
#3
#El porcentaje de la cartera (número de créditos) que corresponde a créditos "malos" es menor para los clientes con "buen" historial crediticio?
mytable(~credit_history*class,data = credit)
#Para la categoria A30, consideraba la de mejor historial crediticia, cuenta dentro de su categoria con el 62.5% de creditos
#considerados como malos, y para la categoria A31 cuentan con un 57.14% de creditos malos dentro de su categoria, asi que no.
#4
#El porcentaje de la cartera (en valor) que corresponde a créditos "malos" es menor para los clientes con "buen" historial crediticio?
mytable(credit_amount~credit_history*class,data = credit)
#Para la categoria A30, EL 65.1% de la cartera en valor corresponden a creditos "malos", mientras que para 
#la categotia A31, es del 57.6% de creditos "malos"
#5
#Empleo actual desde
#A71 : desempleado
#A72 : ... < 1 año
#A73 : 1 <= ... < 4 años
#A74 : 4 <= ... < 7 años
#A75 : .. >= 7 años
#El porcentaje de la cartera (número de créditos) que corresponde a créditos "malos" es menor para los clientes con empleos más estables?
mytable(~employment*class,data = credit)
#Es verdad, dado que las personas que tienen trabajo hace mas de 4 años (A74) o hace mas de 7 años (A75), tienen el 77.59% y el 74.7% de numero
#de creditos "buenos"
#6
#El porcentaje de la cartera (valor de los créditos) que corresponde a créditos "malos" es menor para los clientes con empleos más estables?
mytable(credit_amount~employment*class,data = credit)
#Es verdad, dado que las personas que tienen trabajo hace mas de 4 años (A74) o hace mas de 7 años (A75), tienen el 74.9% y el 65.21% del valor
# de creditos en "buenos"
