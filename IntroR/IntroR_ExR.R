#### EXERCICIS 1 #### 
# A partir del conjunt de dades "Davis"...
if (!require("car")) {
  install.packages("car")
  library("car")
}
ds <- Davis
help("Davis",package="carData")


# 1- Quin tipus de dades tenim?
class(ds)

# 2- Quins camps/columnes conté? A quin tipus de dades correspon cada una de elles?
str(ds)

# 3- Quin és el pes mitjà i pes mitjà autoinformat? I per cada gènere?
mean(ds$weight)
mean(ds$repwt,na.rm=TRUE)

aggregate(ds$weight,list(ds$sex),mean)
aggregate(ds$repwt,list(ds$sex),mean,na.rm=TRUE)

# 4- Quin és la mitjana de les diferències entre pes i pes autoinformat?
mean(ds$weight-ds$repwt,na.rm=TRUE)
mean(ds[!is.na(ds$repwt),"weight"])-mean(ds$repwt,na.rm=TRUE)

# 5- Quantes dones hi ha en el conjunt de dades? I homes?
sum(ds$sex=="F")
sum(ds$sex=="M")
table(ds$sex)


#### EXERCICIS 2 #### 
# Seguim amb el conjunt de dades "Davis"...
if (!require("car")) {
  install.packages("car")
  library("car")
}
ds <- Davis
help("Davis",package="carData")

# 6- Feu un diagrama de caixa per al pes d'homes i de dones
boxplot(ds$weight~ds$sex)

# 7- Quin és l'individu que presenta un IMC més baix? I el més alt?
# NOTA: L'IMC es calcula com pes (en kg) entre alçada (en m) al quadrat
ds$imc <- ds$weight / (ds$height / 100) ^ 2
sort(ds$imc)
min(ds$imc)
max(ds$imc)
ds[which.max(ds$imc),]
max(ds$imc[-12])

# 8- Quin és la mitjana de les diferències entre pes i pes autoinformat per
# per a les dones? I per als homes?
dsF <- ds[ds$sex=="F",]
mean(dsF$weight-dsF$repwt,na.rm=TRUE)

dsM <- ds[ds$sex=="M",]
mean(dsM$weight-dsM$repwt,na.rm=TRUE)

# 9- Quin és la mitjana de les diferències entre pes i pes autoinformat
# per a les dones que pesen més que la mediana? I per a les que pesen menys?
weightFmed <- median(dsF$weight)
dsFM <- dsF[dsF$weight>weightFmed,]
dsFm <- dsF[dsF$weight<weightFmed,]

mean(dsFM$weight-dsFM$repwt,na.rm=TRUE)
mean(dsFm$weight-dsFm$repwt,na.rm=TRUE)

# 10- Feu un diagrama de dispersió de l'alçada i del pes, de manera que les dades 
# d'homes i de dones surtin de colors diferents. Afegiu la recta de regressió.
plot(ds$weight,ds$height,type="n")
points(ds$weight[ds$sex=="F"],ds$height[ds$sex=="F"],col="deeppink2")
points(ds$weight[ds$sex=="M"],ds$height[ds$sex=="M"],col="blue")
abline(lm(height~weight,data=ds[ds$sex=="M",]),col="blue")
abline(lm(height~weight,data=ds[ds$sex=="F",]),col="deeppink2")

ds1 <- ds[-12,]
plot(ds1$weight,ds1$height,type="n")
points(ds1$weight[ds1$sex=="F"],ds1$height[ds1$sex=="F"],col="deeppink2")
points(ds1$weight[ds1$sex=="M"],ds1$height[ds1$sex=="M"],col="blue")
abline(lm(height~weight,data=ds1[ds1$sex=="M",]),col="blue")
abline(lm(height~weight,data=ds1[ds1$sex=="F",]),col="deeppink2")
