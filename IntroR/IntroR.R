##=========================================================================##
##                                                                         ##
##  Seminari d'introducció a R                                             ##
##  @authors: Jordi Cuadros, Vanessa Serrano, Francesc Martori             ##
##                                                                         ##
##=========================================================================##

#### ACCÉS A L'AJUDA #### 
# Cercar una pàgina d'ajuda (títols, sobre una funció, objecte...)
# Retorna una pàgina d'ajuda
? "NULL"
? "mean"
# A RStudio, es pot seleccionar i prémer F1

# Cercar un text dins dels documents d'ajuda (pàgines d'ajuda, manuals...)
# Retorna llista de documents
?? "data types"
?? "mean"

# Accedir a la documentació d'un paquet
help(package="stats")

# La cerca es fa en els paquets instal·lats.


#### DADES BÀSIQUES #### 
## Numeric
a <- 2
a
class(a)
b <- 13.6788956789
b
class(b)
print(b, digits = 10)

a+b
a-b
a*b
a^b
a/b

## Integer
n <- as.integer(1)
n <- as.integer(340000)
class(n)
n <- 20L
class(n)

# En operar un integer es converteix en numeric
class(n + 1)

## Character
a <- "aaa"
b <- "bbb"
paste(a, b, "hola")
paste(a, b, "hola", sep = ", ")
paste(a, b, "hola", sep = "")

## Logical 
a <- 3 == 2 # Comparació igualtat
a
b <- 3 != 2 # Comparació diferent
b

a & b # Operador AND
a | b # Operador OR
!b  # Operador NOT

## Complex (menys útil en l'anàlisi de dades)
a <- 2 + 3i
class(a)
a ^ 2


#### DADES COMPOSTES #### 
## Vector
a <- c(2, 3, 4)
b <- c(10, 20)
class(a)
str(a)
a[2]

# Totes les dades han de ser del mateix tipus
c(1,"a","b")
c(1, 5, T)
c(T,"a",F)
c(2, "c", F)

# Les operacions s'apliquen a vectors i retornen vectors
a + a 
b * b
a > 2.5

a * b # Reciclatge de dades

# Funcions amb vectors
abs(sin(a))
exp(a)
sort(-a)

length(a)
sum(a)
mean(a)
sd(a)
max(a)

3 %in% a
c(15,10,20,40) %in% b

# Vectors seqüència
1:10
seq(1, 10, by = 2)
seq(1, 3, length.out = 5) 

## List
a <- list(2, "2", FALSE)
b <- list(3, "hola", c(2, 3, 4))
a
b
length(a)
a[[3]]
b[[3]]
b[3]
b[[3]][1]
str(b)

## Factor
a <- c("hola", "adeu","hola", "adeu", "adeu", "bye")
a
b <- as.factor(a)   # factor(a) fa el mateix
b
as.character(b)
as.numeric(b)
str(b)
str(a)

a <- factor(c(3, 1, 3, 1, 1, 2), labels = c("adeu", "bye", "hola"))
# as.factor no serviria
a
levels(a)

## Factor ordenat
notes <- c("Aprovat", "Insuficient", "Notable", "Insuficient", "Notable", "Excel·lent", "Aprovat")
str(notes)
notes <- factor(notes)
str(notes)
levels(notes)
notes <- as.character(notes)
str(notes)
notes <- factor(notes,
                levels = c("Insuficient", "Aprovat", "Notable", "Excel·lent"),
                ordered = TRUE)
# Compte amb la diferència entre labels (el vector és d'índexs)
# i levels (el vector és de cadenes de text).
# ordered = FALSE per defecte

str(notes)
levels(notes)

## Matrius
a <- matrix(c(2, 4, -3, 5), ncol = 2)
a[2,2]
a * a # Producte posició a posició
a %*% a # Producte matricial

t(a) # Transposició

## Data Frame
dfA <- data.frame(int = 1:10, let = sample(letters, 10, replace = TRUE), 
                  ran = rnorm(10))
dfA
dim(dfA) # Dimensio, com si fos una matriu
nrow(dfA) # Número de columnes
ncol(dfA) # Número de fileres
str(dfA)
head(dfA) # Primers valors, 6 per defecte
tail(dfA, 2) # Darrers valors


#### CONVERSIÓ ENTRE DADES COMPOSTES #### 
# list <=> vector
list1 <- list(1:5, letters[20:10], rep("c", 5), c(1, 2, 2, 2))
list1
vec1 <- unlist(list1)
vec1
list2 <- list(vec1)
list2
list2 <- as.list(vec1)
list2

# matrix <=> data.frame
df1 <- data.frame(1:5, letters[20:16], rep("c", 5), c(1, 2, 2, 4, 5))
df1
mat1 <- as.matrix(df1)
mat1
df2 <- data.frame(mat1) # O as.data.frame
str(df2)


#### MANIPULACIÓ DE TAULES DE DADES ####
# Anomenar files i columnes
dfTest1 <- data.frame(1:5, letters[1:5], c(rep("a", 3), rep("b", 2)))
dfTest1

colnames(dfTest1) <- c("var1", "var2", "var3") 
rownames(dfTest1) <- paste("subject00", 1:5, sep = "")
dfTest1

# Afegir files i columnes
dfTest2 <- cbind(dfTest1, rnorm(5)) # afegint un vector al data frame
dfTest2
dfTest2$var5 <- 5:1 # assignant els valor a una nova variable
dfTest2

dfTest2 <- rbind(dfTest1, list(6, "e", "b"))
dfTest2

# Segmentar (subsetting)
# ... per índexs
dfTest2 <- dfTest1[1:3,]
dfTest2

dfTest2 <- dfTest1[,c(1,3)]
dfTest2

dfTest2 <- dfTest1[-3,-2]
dfTest2

# ... per noms
dfTest2 <- dfTest1[,"var2"]
dfTest2
dfTest1$var2

dfTest2 <- dfTest1[,c("var2","var3")]
dfTest2

# ... per condicions o valors lògics
dfTest2 <- dfTest1[c(T,T,F,T,F), c(T,F,T)]
dfTest2

dfTest2 <- dfTest1[dfTest1[,1] == 3 | dfTest1[,3] == "b",]
dfTest2

# ... per presència en vector
dfTest2 <- dfTest1[rownames(dfTest1) %in% c("subject001","subject003"),]
dfTest2

sel<-which(rownames(dfTest1) %in% c("subject001","subject003"))
sel
dfTest1[sel,]

# També poden ser interessant which.max i which.min
set.seed(123)
a <- runif(10)
a
which.max(a)
a[which.min(a)]

# Eliminar columna
dfTest2 <- dfTest1
dfTest2[,2] <- NULL
dfTest1
dfTest2


#### EXERCICIS 1 #### 
# A partir del conjunt de dades "Davis"...
if (!require("car")) {
  install.packages("car")
  library("car")
}
ds <- Davis
help("Davis",package="car")

# 1- Quin tipus de dades tenim?

# 2- Quins camps/columnes conté? A quin tipus de dades correspon cada una de elles?

# 3- Quin és el pes mitjà i pes mitjà autoinformat? I per cada gènere?

# 4- Quin és la mitjana de les diferències entre pes i pes autoinformat?

# 5- Quantes dones hi ha en el conjunt de dades? I homes?


#### ELIMINAR OBJECTES #### 
dfTest2 <- NULL
rm(dfTest2)
rm(list = ls())


#### CONSTANTS #### 
NA
NULL
is.null(NULL)
is.na(NA)
is.null(NA)
is.na(NULL)

Inf
NaN

exp(999999)
log(-5)


#### IMPORTACIÓ I EXPORTACIÓ D'ARXIUS DE DADES #### 
# https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data
# https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.names

# mpg:           continuous
# cylinders:     multi-valued discrete
# displacement:  continuous
# horsepower:    continuous
# weight:        continuous
# acceleration:  continuous
# model year:    multi-valued discrete
# origin:        multi-valued discrete 1-North America, 2-Europe and 3-Asia
# car name:      string (unique for each instance)

getwd()
#setwd(...)

carsUCI <- read.table("auto-mpg-fixed.data", header = FALSE, 
                      sep = "\t", strip.white = TRUE, na.strings = "?", quote = "\"")
colnames(carsUCI) <- c("mpg","cylinders","displacement","horsepower","weight",
                       "acceleration","year","origin","carName")

str(carsUCI)
head(carsUCI)

# Importació d'un conjunt de dades mal formatades o d'amplada fixa
lines <- readLines("auto-mpg.data")
str(lines)
head(lines)
mpg <- as.numeric(substr(lines,1,4))
str(mpg)

# Exportació
# ... a text
write.table(x = carsUCI, file = "carsUCI.tsv", sep = "\t", dec = ",",row.names = FALSE)
# ... a RData
save(x = carsUCI, file = "carsUCI.RData")

# Espai de treball a RData
save(list = ls(all.names = TRUE), file = "workspace.RData") 
save.image("workspace.RData")

# Lectura de RData
load("carsUCI.RData")

print(load("carsUCI.RData"))
rm(list=ls()) # Esborra el workspace
loadedDS <- get(load("carsUCI.RData"))

#### ESTADÍSTICA DESCRIPTIVA ####
airquality
?airquality

## Estadístics habituals
summary(airquality)

mean(airquality) # No OK
mean(airquality$Ozone) # Compte amb NA's
mean(airquality$Ozone, na.rm = TRUE)
median(airquality$Ozone, na.rm = TRUE)

# Càlcul de quantils
quantile(airquality$Ozone, na.rm = TRUE, c(0.25, 0.5, 0.75))

# Dispersió
sd(airquality$Ozone, na.rm = TRUE)
var(airquality$Ozone, na.rm = TRUE)
IQR(airquality$Ozone, na.rm = TRUE)

## Taules de freqüències
table(airquality$Month)
table(airquality$Ozone) # No OK
table(airquality$Ozone > mean(airquality$Ozone, na.rm = TRUE)) 
table(airquality$Ozone > mean(airquality$Ozone, na.rm = TRUE), airquality$Month)

## Regressió Lineal
mod1 <- lm(Temp ~ Wind, data = airquality)
summary(mod1)

## Ús de funcionals
apply(airquality, 2, mean, na.rm = TRUE)
tapply(airquality$Ozone, airquality$Month, mean, na.rm = TRUE)


### GRÀFICS HABITUALS ####

# Per una variable
hist(airquality$Wind) # Histograma
boxplot(airquality$Wind) # Diagrama de caixa
boxplot(airquality$Wind ~ airquality$Month) # o més d'una

# Per a dues variables
plot(airquality$Wind, airquality$Temp) # Gràfic de dispersió
abline(mod1, col = "red") # Afegim la recta de regressió

pairs(airquality[,1:4]) # Matriu de gràfics de dispersió


#### EXERCICIS 2 #### 
# Seguim amb el conjunt de dades "Davis"...
if (!require("car")) {
  install.packages("car")
  library("car")
}
ds <- Davis
help("Davis",package="car")

# 6- Feu un diagrama de caixa per al pes d'homes i de dones

# 7- Quin és l'individu que presenta un IMC més baix? I el més alt?
# NOTA: L'IMC es calcula com pes (en kg) entre alçada (en m) al quadrat

# 8- Quin és la mitjana de les diferències entre pes i pes autoinformat per
# per a les dones? I per als homes?

# 9- Quin és la mitjana de les diferències entre pes i pes autoinformat
# per a les dones que pesen més que la mediana? I per a les que pesen menys?

# 10- Feu un diagrama de dispersió de l'alçada i del pes, de manera que les dades 
# d'homes i de dones surtin de colors diferents. Afegiu la recta de regressió.

