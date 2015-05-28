##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre:SEBASTIAN ALCIVAR


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()

data = read.table("data.txt", header = TRUE,dec=",", sep="\t")
str(data)


# 2.2 Calcular el mi???nimo, la media, el máximo de la variable Edad
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE

edad <- data[,"Edad"]

mean(edad,na.rm=TRUE)
min(edad,na.rm=TRUE)
max(edad, na.rm=TRUE)

# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()

data_genero <- subset(data, data[,"Genero"]=="Femenino")
table(data[,"Genero"])
table(data_genero[,"Genero"])

# 2.4 Encontrar la Edad m????nima, media, máxima de los sujetos que Si son dependientes.

data_dependientes <- subset(data, data[,"Dependiente"]=="Si")
max(data_dependientes[,1],na.rm=TRUE)
min(data_dependientes[,1],na.rm=TRUE)
mean(data_dependientes[,1],na.rm=TRUE)

# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()

tipo <- character()

for (i in 1:ncol(data)){
  tipo[i] <- typeof(data[,i])
}
tipo


# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()

clase <- charecter()

for (i in 1:ncol(data)){
  clase[i] <- class(data[,i])
}
clase

# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para n factor no es posible obtener la media debido a que 
# éstos representan variables

s_num<-logical()
for (i in 1:ncol(data)){
  s_num[i] <- is.numeric(data[,i])
}
d_num<-data[,s_num]

media_s<-numeric()
for (j in 1:ncol(d_num)){
  media_s[j]<-mean(d_num[,j],na.rm=TRUE)
}
media_s

# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()

na_s<-numeric()
for (i in 1:ncol(data))
{
  na_s[i]<-sum(is.na(data[,i]))
}
na_p<-na_s/nrow(data)
na_p

# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()

d_edad <- subset(data, data[,"Edad"]>40)
min(d_edad[,"Edad"])

# 3.2 Seleccione los sujetos que tienen Vivienda Propia.

d_vivienda <- subset(data, data[,"Vivienda"]=="Propia")

# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.

d_cargas <- subset(data, data[,"Cargas"]>2)

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.

d_deuda <- subset(data, data[,"Deuda"]>=500 & data[,"Dias_Atraso"]>8)


# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC).

d_score<- subset(data, data[,"Edad"]<=35 & data[,"Numero_TC"]>3)

# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red

library(ggplot2)

graf1<-qplot(edad, geom="histogram", binwidth = 0.5, main = "Histograma de Edad", xlab = "Edad",  fill=I("white"), col=I("red"), alpha=I(.2),xlim=c(20,50))


# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()

boxplot(edad,col = "green")
