options(install.packages.check.source = "no")
pckgs<-c("tidyverse","ggthemes","RColorBrewer","carData")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#### EXERCICIS 1 #### 

# A partir del conjunt de dades mtcars 
mtcars
? "mtcars"
str(mtcars)

# 1- Crea una visualització que permeti discutir la relació
# entre la potència (hp) i l'acceleració(qsec)
ggplot(data=mtcars, mapping=aes(x=hp,y=qsec))+geom_point()+theme_classic()

# 2- Afegeix a aquesta visualització la variable pes (wt)
ggplot(mtcars, aes(x=hp, y=qsec, size=wt)) + geom_point(alpha=.5)
ggplot(data=mtcars, mapping=aes(x=hp,y=qsec,col=wt))+geom_point()+theme_classic()
ggplot(data=mtcars, mapping=aes(x=hp,y=qsec,size=wt,col=wt))+geom_point(size=3)+theme_classic()
ggplot(data=mtcars, mapping=aes(x=hp,y=qsec,size=wt,col=wt))+geom_point()+theme_classic()

# 3- Estudia l'efecte del nombre de cilindres (cyl) en la relació entre
# potència i acceleració
ggplot(data=mtcars, mapping=aes(x=hp,y=qsec,col=as.factor(cyl)))+geom_point(size=3)+theme_classic()

# 4- Crea un histograma que et permeti estudiar la distribució de la
# potència
ggplot(data=mtcars,aes(x=hp))+geom_histogram()
ggplot(mtcars, aes(x=hp)) + geom_histogram(binwidth=50, 
                                           fill="grey", color="black")
ggplot(mtcars, aes(x=hp)) + geom_histogram(binwidth=50, 
                                           boundary=100, fill="grey", color="black")
ggplot(data=mtcars,aes(x=hp,y=..density..))+geom_histogram(binwidth = 50, boundary=100, fill="green")+geom_density()+theme_classic()

# 5- Crea un conjunt de diagrames de caixa tals que permetin estudiar
# la relació entre potència i nombre de cilindres
ggplot(mtcars, aes(y=hp, x=1)) + geom_boxplot() #NO
ggplot(mtcars, aes(y=hp, x=cyl)) + geom_boxplot() #NO
ggplot(data=mtcars,aes(x=as.factor(cyl),y=hp))+geom_boxplot()+theme_classic()

#### EXERCICIS 2 #### 
# A partir del conjunt de dades SLID (Survey of Labour and Income Dynamics)
# del 1994 a Ontario (Canadá)

dsSLID <- SLID
str(dsSLID)

# 1- Crea una visualització que permeti discutir la relació
# entre l'edat i els anys d'escolarització
dsSLIDm <- dsSLID[dsSLID$age<=25,]
dsSLIDM <- dsSLID[dsSLID$age>25,]

ggplot(dsSLID, aes(x=age,y=education)) + 
  geom_point(shape=21,alpha=0.2)+
  geom_smooth(data=dsSLIDm,method="lm",se=FALSE)+
  geom_smooth(data=dsSLIDM,method="lm",se=FALSE)


if(!require("splines")) {
  install.packages("splines")
  library("splines")
}

ggplot(dsSLID, aes(x=age,y=education)) + 
  geom_point(shape=21,alpha=0.2)+
  geom_smooth(method="lm",formula=y~bs(x,knots=25))

# 2- Aquesta relació és diferent en funció de l'idioma?
dsSLIDM_EF <- dsSLID[dsSLID$age>25 &
                       (dsSLID$language == "French" | dsSLID$language == "English"),]
dsSLIDM_EF <- dsSLIDM_EF[!is.na(dsSLIDM_EF$language),]

ggplot(dsSLIDM_EF, aes(x=age,y=education,color=language)) + 
  geom_point(shape=21,alpha=0.2)+
  geom_smooth(method="lm",se=FALSE)

# 3- Mostra amb un gràfic si la proporció de població que parla un idioma
# depén de l'edad
dsSLID_EF <- dsSLID[ 
  !is.na(dsSLID$language) & 
    (dsSLID$language == "French" | dsSLID$language == "English"),]

ggplot(dsSLID_EF, aes(x=language,y=age))+geom_boxplot()
ggplot(dsSLID_EF, aes(fill=language,x=age)) +geom_bar()
ggplot(dsSLID_EF, aes(fill=language,x=age)) +geom_bar(position="fill")

# 4- Estudia la distribució de les persones enquestades per idioma
ggplot(dsSLID,aes(x=language))+geom_bar()

# 5- Representa la distribució d'anys d'escolarització
ggplot(dsSLID,aes(x=education))+geom_histogram(binwidth = 1,
                                               boundary=0)
ggplot(dsSLID,aes(x=education)) +
  geom_histogram(aes(y=..density..), fill="lightgrey",color="black",
                 binwidth = 1, boundary = 0) +
  geom_density(fill="red",alpha=0.1)

# 6- Hi ha alguna diferència entre les distribució d'anys
# d'escolarització en funció del gènere
ggplot(dsSLID,aes(x=education,fill=sex)) +
  geom_histogram(aes(y=..density..), position="dodge",color="black",
                 binwidth = 1, boundary = 0) +
  geom_density(alpha=0.1)

ggplot(dsSLID,aes(x=sex,y=education)) + geom_boxplot()
