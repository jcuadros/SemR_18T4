##=========================================================================##
##                                                                         ##
##  Anàlisi de qüestionaris amb R                                          ##
##  @authors: Jordi Cuadros, Vanessa Serrano                               ##
##                                                                         ##
##=========================================================================##

options(install.packages.check.source = "no")

pckgs<-c("psych","tidyverse", "ggthemes","RColorBrewer",
         "ShinyItemAnalysis","corrplot","tm","wordcloud")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}


#### TIPUS DE RESPOSTES I TIPUS DE DADES A R ####
# Les dades que ens trobem com a respostes a les preguntes d'un qüestionari
# poden ser de diferents tipus.
# Considerant les més habituals, ens trobem respostes
# - qualitatives
#   * assaig
#   * categòriques
#     - nominals
#       * dicotòmiques (només 2 categories)
#     - ordinals
# - quantitatives
#   * discretes
#   * continues
#     - d'interval (amb zero arbitrari)
#     - de raó (amb zero absolut)


# Els diferents tipus de dades de respostes solen emmagatzemar en diferents 
# tipus de variables.

# Assaig ============== Cadena de text
# Nominal  ============ Factor
# Dicotòmica ========== Lògica
# Ordinal ============= Factor ordenat
# Discretes =========== Numèric / Factor
# Continues =========== Numèric


#### PRESENTACIÓ DE RESULTATS ####
# Treballar amb les dades de l'enquesta 'Sleep in America' del 2013.
# https://www.sleepfoundation.org/sleep-polls
# Les dades estan recollies a "2013sleep.txt".

dadesSiA <- read.table("2013sleep.txt", header=TRUE, stringsAsFactors = TRUE,
                       sep="\t", quote="")


### Respostes assaig ####
# Cal convertir-les a categòriques o aplicar tècniques de mineria de textos
# Per exemple, es poden visualitzar les respostes addicionals a la pregunta 43
# usant un núvol de paraules.
# How much total time per day did you spend sitting during each of the following 
# activities in the past 7 days:
#   G. Something else (SPECIFY)

resp43g <- paste(dadesSiA$Q43AGOT1,dadesSiA$Q43AGOT2,dadesSiA$Q43AGOT3,
                 sep=" ", collapse=" ")

resp43g <- tolower(resp43g)
resp43g <- gsub("[^[:print:]]"," ", resp43g)
resp43g <- gsub("[[:punct:]]"," ", resp43g)
resp43g <- gsub("\\<\\w{0,3}\\>"," ", resp43g)
resp43g <- gsub("\\s{2,}"," ", resp43g)

resp43g <- removeWords(resp43g, gsub("[[:punct:]]"," ", stopwords("SMART")))

corpus43g <- VCorpus(VectorSource(resp43g))
tdm43g <- TermDocumentMatrix(corpus43g)
frequent <- findMostFreqTerms(tdm43g, n = 100)
wordcloud(names(frequent$`1`),frequent$`1`,rot.per=0,
          colors=brewer.pal(4,"Spectral"),random.order = FALSE)


### Respostes nominals ####
# Treballem per exemple amb les respostes a la pregunta S2 (qs2)
#   S2. What has been your employment status over the past month? Were you primarily…
#      01 Working full-time or part-time,
#      02 A full-time homemaker,
#      03 Not working, retired, or
#      04 Something else?
#      98 DO NOT READ: Refused
#      99 DO NOT READ: Don’t know

respS2 <- dadesSiA$qs2
str(respS2)

resp2 <- factor(respS2)
levels(resp2)
levels(resp2) <- c("Working full-time or part-time",
                    "A full-time homemaker",
                    "Not working, retired",
                    "Something else?",
                    "Refused")
resp2

# Taula de freqüències
tfreqS2 <- table(resp2)
tfreqS2
sum(tfreqS2)

str(tfreqS2)

data.frame(Resposta=names(tfreqS2), FreqAbs= as.numeric(tfreqS2),
           FreqRel= as.numeric(tfreqS2)/sum(tfreqS2))

# Taula de freqüències (ordenada per freqüència de resposta)
opcOrdenades <- names(sort(tfreqS2,decreasing = T))
resp2o <- factor(resp2, levels=opcOrdenades, ordered = TRUE)

tfreqS2o <- table(resp2o)
tfreqS2o

data.frame(Resposta=names(tfreqS2o), FreqAbs= as.numeric(tfreqS2o),
           FreqRel= as.numeric(tfreqS2o)/sum(tfreqS2o))

# Diagrama de barres (mantenint l'ordre de les respostes)
ggplot(NULL,aes(x=resp2)) +
  geom_bar(fill="lightgrey", col="black") +
  scale_x_discrete(limits = rev(levels(resp2)))+
  theme_classic() +
  coord_flip() +
  labs(x=NULL, y="Freqüència absoluta")

# Diagrama de barres (ordenat per freqüència)
ggplot(NULL,aes(x=resp2o, col=)) +
  geom_bar(fill="lightgrey", col="black") +
  scale_x_discrete(limits = rev(levels(resp2o)))+
  theme_classic() +
  coord_flip() +
  labs(x=NULL, y="Freqüència absoluta")

# Diagrama de barres (ordenat per freqüència, destacant més
# significatius)
tfreqS2oDF <- data.frame(Resposta=names(tfreqS2o), FreqAbs= as.numeric(tfreqS2o),
           FreqRel= as.numeric(tfreqS2o)/sum(tfreqS2o))
tfreqS2oDF$Sel <- c(TRUE, FALSE, FALSE, FALSE, FALSE)

ggplot(tfreqS2oDF,aes(x=Resposta, y=FreqAbs, fill=Sel)) +
  geom_bar(stat="identity", col="black") +
  scale_x_discrete(limits = rev(levels(resp2o)))+
  scale_fill_manual(values=c("lightgrey","black")) +
  theme_classic() +
  coord_flip() +
  labs(x=NULL, y="Freqüència absoluta")+
  theme(legend.position = "none")

ggplot(tfreqS2oDF,aes(x=Resposta, y=FreqAbs, fill=Sel)) +
  geom_bar(stat="identity", col="black") +
  scale_x_discrete(limits = rev(levels(resp2o)))+
  scale_fill_manual(values=c("lightgrey","darkgreen")) +
  theme_classic() +
  coord_flip() +
  labs(x=NULL, y="Freqüència absoluta")+
  theme(legend.position = "none")

# Diagrama de punts
ggplot(tfreqS2oDF,aes(x=Resposta, y=FreqAbs, fill=Sel)) +
  geom_segment(aes(xend=Resposta,yend=0), color="lightgrey")+
  geom_point(size=5, shape=21) +
  scale_x_discrete(limits = rev(levels(resp2o)))+
  scale_fill_manual(values=c("lightgrey","darkgreen")) +
  theme_classic() +
  coord_flip() +
  labs(x=NULL, y="Freqüència absoluta")+
  theme(legend.position = "none")

# El waffle chart també és visualment una bona opció.

### Respostes dicotòmiques ####
# Prenem com a exemple la pregunta 15...
# Thinking about the past two weeks, does your current work schedule or 
# typical weekday routine, including your duties at home, allow you to get 
# adequate sleep?
#   01 Yes
#   02 No
#   98 Refused
#   99 Don’t know

resp15 <- dadesSiA$q15
resp15 <- factor(resp15)
levels(resp15)
levels(resp15) <- c("Yes","No", "Refuse")

# Taula de freqüències
tfreq15 <- table(resp15, useNA="ifany")
tfreq15
sum(tfreq15)

# Diagrama de barres (mantenint l'ordre de les respostes)
ggplot(NULL,aes(x=resp15)) +
  geom_bar(col="black",fill="lightgrey") +
  scale_x_discrete(limits = rev(levels(resp15)))+
  theme_classic() +
  coord_flip() +
  labs(x=NULL, y="Freqüència absoluta") +
  theme(legend.position = "none")

# Diagrama de barres apilades
resp15o <- factor(resp15, levels=c("Yes", "Refuse","No"),
                  ordered = TRUE)
tfreq15o <- table(resp15o, useNA="ifany")
tfreq15o <- tfreq15o / sum(tfreq15o) * 100
df15o <- as.data.frame(tfreq15o)
colnames(df15o) <- c("Response","FreqRel")

ggplot(df15o,aes(x=1, y=FreqRel, fill=Response)) +
  geom_bar(stat="identity", position="stack", col="black") +
  theme_classic() +
  coord_flip() +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())+
  scale_fill_manual(values=rev(brewer.pal(3,"RdBu")))

### Respostes dicotòmiques (múltiples) ####
# Prenem com a exemple les preguntes 20-23...
# 20. Do you snore loudly? Loudly, meaning louder than talking or loud enough to be 
# heard through a closed door?
# 21. Do you often feel tired, fatigued or sleepy during the day?
# 22. Has anyone observed you stop breathing during your sleep?
# 23. Do you have or are you being treated for high blood pressure?
#   01 Yes
#   02 No
#   98 Refused
#   99 Don’t know

resp20ss <- dadesSiA[,c("q20","q21","q22","q23")]
resp20ss <- gather(resp20ss,"Question", "Response")

resp20ss$Response[resp20ss$Response=="98"]<-"99"
resp20ss$Response <- factor(resp20ss$Response)
levels(resp20ss$Response) <- c("Yes","No", "Not Answered")

resp20ss$Question<- factor(resp20ss$Question)
levels(resp20ss$Question) <-
  c("20. Do you snore loudly? Loudly, meaning louder than talking or loud enough to be heard through a closed door?",
    "21. Do you often feel tired, fatigued or sleepy during the day?",
    "22. Has anyone observed you stop breathing during your sleep?",
    "23. Do you have or are you being treated for high blood pressure?")


# Taula de freqüències
tfreq20ss <- table(resp20ss$Question, resp20ss$Response, useNA="ifany")
sum(tfreq20ss)

tfreq20ss <- tfreq20ss / 10 


# Diagrama de barres apilades
df20ss <- as.data.frame(tfreq20ss)

colnames(df20ss) <- c("Question","Response","FreqRel")
levels(df20ss$Question)[nchar(levels(df20ss$Question))>20] <-
  paste(substr(levels(df20ss$Question[nchar(levels(df20ss$Question))>20]),
               1,17),"...",sep="")

df20ss$Response <- factor(df20ss$Response, levels=c("Yes", "Not Answered","No"),
                  ordered = TRUE)

ggplot(df20ss,aes(x=Question, y=FreqRel, fill=Response)) +
  geom_bar(stat="identity", position="stack", col="black") +
  theme_classic() +
  coord_flip() +
  theme(axis.title.y = element_blank())+
  scale_fill_manual(values=rev(brewer.pal(3,"RdBu")))+
  scale_x_discrete(limits = rev(levels(df20ss$Question)))


# Diagrama de barres apilades centrades
df20ssMes <- df20ss 
df20ssMenys <- df20ss

df20ssMes$FreqRel[df20ssMes$Response=="No"] <- 0
df20ssMes$FreqRel[df20ssMes$Response=="Not Answered"] <- 
  df20ssMes$FreqRel[df20ssMes$Response=="Not Answered"] / 2

df20ssMenys$FreqRel[df20ssMenys$Response=="Yes"] <- 0
df20ssMenys$FreqRel[df20ssMenys$Response=="Not Answered"] <- 
  -df20ssMenys$FreqRel[df20ssMenys$Response=="Not Answered"] / 2
df20ssMenys$FreqRel[df20ssMenys$Response=="No"] <- 
  -df20ssMenys$FreqRel[df20ssMenys$Response=="No"]

df20ssMenys <- df20ssMenys[nrow(df20ssMenys):1,]
df20ssMenys$Response <- factor(df20ssMenys$Response,
                               levels=c("No","Not Answered","Yes"),
                               ordered = TRUE)

ggplot() +
  geom_bar(aes(x=Question, y=FreqRel, fill=Response),
           data=df20ssMes,stat="identity", position="stack", col="black") +
  geom_bar(aes(x=Question, y=FreqRel, fill=Response),
           data=df20ssMenys,stat="identity", position="stack", col="black") +
  theme_classic() +
  coord_flip() +
  theme(axis.title.y = element_blank())+
  scale_y_continuous(labels=abs) +
  scale_x_discrete(limits = rev(levels(df20ss$Question)))+
  scale_fill_manual(values=rev(brewer.pal(3,"RdBu")))

# En función de les preguntes pot ser convenient ordenar per 
# freqüència de la resposta positiva.
# Per exemple, en una pregunta de selecció múltiple. 


### Respostes ordinals ####
### Respostes ordinals (múltiples) ####

# S'usen les mateixes lògiques que les presentades per a una 
# dicotòmica encara que amb més nivells per a cada factor.


### Respostes quantitatives discretes ####
# Quan nombre de valors diferents és petit, es poden tractar 
# com a factors ordenats.
# Quan el nombre de valors diferents és gran, sovint es tracten
# com a numèriques.

# Fixem-nos en la pregunta S1
# S1. What is your age?
respS1 <- dadesSiA$qs1
class(respS1)

# Tractant-la com a numérica (continua)...
# Estadístics paramètrics
summary(respS1)
mean(respS1, na.rm = TRUE)
sd(respS1, na.rm = TRUE)

# Boxplot
ggplot(NULL, aes(x=1, y=respS1)) +
  geom_boxplot() +
  labs(y="Edat", x = NULL)+
  theme_classic() + 
  coord_flip() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y= element_blank())

# Tractant-la com a factor ordenat...
# Estadístics no paramètrics
fivenum(respS1)
median(respS1)
IQR(respS1)
quantile(respS1,(1:19)/20)
quantile(respS1,c(0.01,0.99))

# Diagrama de barres
# Compte amb els nivells absents...
respS1f <- factor(respS1, levels=min(respS1):max(respS1))

ggplot(NULL,aes(x=factor(respS1))) +
  geom_bar(col="black",fill="lightgrey") +
  scale_x_discrete(limits = rev(levels(respS1)), drop=FALSE,
                   breaks = (1:12) * 5)+
  theme_classic() +
  labs(x="Edat", y="Freqüència absoluta")+
  theme(axis.text.x = element_text(vjust=0.5))

ggplot(NULL,aes(x=factor(respS1)[runif(1000)>.95])) +
  geom_bar(col="black",fill="lightgrey") +
  scale_x_discrete(limits = rev(levels(respS1)), drop=FALSE,
                   breaks = (1:12) * 5)+
  theme_classic() +
  labs(x="Edat", y="Freqüència absoluta")+
  theme(axis.text.x = element_text(vjust=0.5))


### Respostes quantitatives continues ####
# Treballem amb les respostes a la pregunta 5...
# On average worknights or weeknights, how many hours, not including naps,
# do you usually sleep during one night?

resp5 <- dadesSiA$q5hour + dadesSiA$q5min / 60    # en hores

# Estadístics paramètrics
summary(resp5)
mean(resp5, na.rm = TRUE)
sd(resp5, na.rm = TRUE)

# Histograma
ggplot(NULL, aes(x=resp5)) +
  geom_histogram(binwidth = 1, fill="lightgrey", col="black") +
  labs(y="Freqüencia absoluta", x="Hores de son")+
  theme_classic()

ggplot(NULL, aes(x=resp5, y=..density..)) +
  geom_histogram(binwidth = 1, fill="lightgrey", col="black") +
  geom_density(bw=.5) +
  labs(y=NULL, x="Hores de son")+
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# Boxplot
ggplot(NULL, aes(x=1, y=resp5)) +
  geom_boxplot(col="black",outlier.shape = NA,width=.05) +
  geom_jitter(alpha=.2, shape=21, width=.2, height=.25, 
              color="green") +
  labs(y="Hores de son", x = NULL)+
  coord_flip()+
  theme_classic() + 
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y= element_blank())


### Relació entre quantitatives ####
# Mirem la relació entre la pregunta 5 i la pregunta 6...
# 5. On average worknights or weeknights, how many hours, 
# not including naps, do you usually sleep during one night?
# 6. On average nights you do not work or weekend nights, 
# how many hours, not including naps, do you usually sleep during one night?

resp5 <- dadesSiA$q5hour + dadesSiA$q5min / 60    # en hores
resp6 <- dadesSiA$q6hour + dadesSiA$q6min / 60    # en hores

df5i6 <- data.frame(resp5,resp6)

# Coeficient de correlació
cor.test(resp5,resp6)

# Model lineal
ajust <- lm(resp6~resp5)
summary(ajust)

# Diagrama de dispersió
ggplot(df5i6,aes(x=resp5,y=resp6)) +
  geom_jitter(shape=21, alpha=0.5, width=.5, height=.5) +
  labs(x="Hores de son (laborables)", y="Hores de son (festius)") +
  theme_classic()

ggplot(df5i6,aes(x=resp5,y=resp6)) +
  geom_smooth(method="lm", fill="#ccffcc", col="green", alpha=.5)+
  geom_jitter(shape=21, alpha=0.5, width=.5, height=.5) +
  labs(x="Hores de son (laborables)", y="Hores de son (festius)") +
  theme_classic()


### Relació entre qualitatives ####
# Per exemple, entre la 20 i la 21
# 20. Do you snore loudly? Loudly, meaning louder than talking or loud enough to 
# be heard through a closed door?
# 21. Do you often feel tired, fatigued or sleepy during the day?
#   01 Yes
#   02 No
#   98 Refused
#   99 Don’t know

df20i21 <- dadesSiA[,c("q20","q21")]
df20i21$q20 <- factor(df20i21$q20)
df20i21$q21 <- factor(df20i21$q21)
levels(df20i21$q20) <- c("Yes","No", "Refused", "Don't know")
levels(df20i21$q21) <- c("Yes","No", "Refused", "Don't know")

# Taula de contingència
tab20i21 <- table(df20i21$q20,df20i21$q21)

tab20i21 <- tab20i21[1:2,1:2]
fisher.test(tab20i21)

# Diagrama de barres apilades
ggplot(df20i21[df20i21$q20 %in% c("Yes","No") & df20i21$q21 %in% c("Yes","No"),],
       aes(x=q20, fill=q21)) +
  geom_bar(position="stack",col="black") +
  theme_classic() +
  labs(y = "", x = "Do you snore loudly?...") +
  scale_fill_manual(values=rev(brewer.pal(3,"RdBu")[c(1,3)])) +
  guides(fill=guide_legend(title="Do you often feel tired...")) +
  theme(legend.position = "top")

# Una altra alternativa és un 'mosaic plot'


### Relació entre una quantitativa i una qualitativa ####
# Prenem la relació entre les preguntes 5 (quantitativa) i 21 (qualitativa)...
# 5. On average worknights or weeknights, how many hours, 
# not including naps, do you usually sleep during one night?
# 21. Do you often feel tired, fatigued or sleepy during the day?
#   01 Yes
#   02 No
#   98 Refused
#   99 Don’t know

resp5 <- dadesSiA$q5hour + dadesSiA$q5min / 60    # en hores
resp21 <- factor(dadesSiA$q21)
levels(resp21) <- c("Yes","No", "Refused", "Don't know")

df5i21 <- data.frame(resp5,resp21)

# Estadístics per a cada valor de la qualitativa
df5i21 <- df5i21[df5i21$resp21 %in% c("Yes","No"),]
by(df5i21$resp5,df5i21$resp21,summary)
by(df5i21$resp5,df5i21$resp21,
   function(x) c(mean=mean(x,na.rm = TRUE),sd=sd(x,na.rm=TRUE)))
by(df5i21$resp5,df5i21$resp21,
   quantile, (1:9)/10, na.rm=TRUE)

# Boxplot
ggplot(df5i21, aes(x=resp21, y=resp5)) +
  geom_boxplot(col="black",outlier.shape = NA,width=.3) +
  geom_jitter(alpha=.2, shape=21,width=.3, height=.25) +
  labs(y="Hores de son", x = "Do you often feel tired...")+
  coord_flip()+
  theme_classic()

# Violin plot
ggplot(df5i21, aes(x=resp21, y=resp5)) +
  geom_violin(col="black", bw=0.5) +
  geom_boxplot(col="grey",outlier.shape = NA,width=.3) +
  labs(y="Hores de son", x = "Do you often feel tired...")+
  coord_flip()+
  theme_classic()

# Histogrames múltiples
df5x21 <- df5i21 %>% group_by(resp21) %>%
  summarise(mean=mean(resp5,na.rm=T))

ggplot(df5i21, aes(x=resp5, y=..density..)) +
  geom_histogram(binwidth = 1, fill="lightgrey", col="black") +
  geom_vline(aes(xintercept=mean),data=df5x21,
             col=rev(brewer.pal(3,"RdBu")[c(1,3)]),size=1)+
  geom_density(bw=.5) +
  labs(y=NULL, x="Hores de son")+
  facet_grid(resp21~.)+
  theme_classic()+
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


#### EXERCICIS 1 #### 
# Seguim amb el qüestionari 'Sleep in America 2013' 

# 1- Presenta els resultats de la pregunta 26
# 26. Do you drink alcoholic beverages?
#    01 Yes
#    02 No
#    98 DO NOT READ: Refused
#    99 DO NOT READ: Don’t know/Not sure


# 2- Presenta els resultats de la pregunta 30
# During the past two weeks, how would you rate your overall sleep quality? 
# Would you say…
#    01 Very good,
#    02 Fairly good,
#    03 Fairly bad, or
#    04 Very bad
#    98 DO NOT READ: Refused
#    99 DO NOT READ: Don’t know/Not sure


# 3- Presenta els resultats de la pregunta 42
# How much time per day did you spend sitting in the past 7 days? Your best 
# estimate is fine.
# DO NOT ACCEPT RANGES.
# __ __ Hours per day [Range: 00-24]
# __ __ Minutes per day [Range: 00-59]
# 98 DO NOT READ: Refused
# 99 DO NOT READ: Don’t know/Not sure


# 4- Estudia la relació entre la pregunta 17 i la pregunta 29
# ---
# 17. Thinking about the past two weeks, how many minutes, on most worknights or 
# weeknights, does it take you to fall asleep? Would you say…(READ LIST.)
#    01 Less than 5 minutes,
#    02 5 up to 10 minutes,
#    03 10 up to 15 minutes,
#    04 15 up to 30 minutes,
#    05 30 up to 45 minutes,
#    06 45 minutes up to 1 hour, or
#    07 1 hour or more?
#    96 DO NOT READ: Depends/Varies
#    98 DO NOT READ: Refused
#    99 DO NOT READ: Don’t know/Not sure
# ---
# 29. Thinking about the last two weeks, how many 12 ounce servings of caffeinated
# beverages, such as soda, soft drinks, coffee, tea, and energy drinks do you drink
# on an average weekday or workday …(READ LIST. RECORD NUMBER FOR EACH BELOW. 
# DO NOT ACCEPT RANGES. 98=REFUSED; 99=DON’T KNOW; 00=NONE; 97=LESS THAN ONE.)


#### ANÀLISI DEL QÜESTIONARI ####
# El més habitual és que els qüestionaris es construeixin per a mesurar
# variables no observables a través de les respostes dels participants 
# (actituds, emocions, comportaments, mesures psicològiques, capacitats...).

# Per fer-ho s'elabora un constructe i un conjunt d'indicadors que
# pretenen representar la variable d'interès.

# Com en d'altres àmbits del coneixement, és important comprovar que la mesura
# és adequada i establir criteris que permetin anar millorant els instruments
# de mesura.


#### FIABILITAT ####
# L'instrument mesura alguna cosa?
# Quatre procediments principals:
# - Test-retest
# - Formes paral·leles
# - Dues meitats
# - Homogeneïtat

### Test-retest ####
### Formes paral·leles ####
# La mesura de fiabilitat és la correlació entre les dues puntuacions del
# constructe per als mateixos subjectes.

# El document QSimulat.txt conté respostes simulades a un qüestionari de 4
# preguntes, puntuades de 0 a 5.

qsimul <- read.table("QSimulat.txt",sep="\t",header=TRUE)

# Test-retest per a la forma A
qsimul$ta <- qsimul$T_A_Q1 + qsimul$T_A_Q2 + qsimul$T_A_Q3 + qsimul$T_A_Q4
qsimul$ra <- qsimul$R_A_Q1 + qsimul$R_A_Q2 + qsimul$R_A_Q3 + qsimul$R_A_Q4

cor.test(qsimul$ta,qsimul$ra)

# Formes paralel·les per al test
qsimul$ta <- qsimul$T_A_Q1 + qsimul$T_A_Q2 + qsimul$T_A_Q3 + qsimul$T_A_Q4
qsimul$tb <- qsimul$T_B_Q1 + qsimul$T_B_Q2 + qsimul$T_B_Q3 + qsimul$T_B_Q4

cor.test(qsimul$ta,qsimul$tb)


### Dues meitats ####
# Consisteix en definir dues meitats equivalents (o no) del qüestionari
# i avaluar la fiabilitat com formes paral·leles. A continuació cal aplicar
# la fórmula de Spearman-Brown

m1 <- qsimul$T_A_Q1 + qsimul$T_A_Q3
m2 <- qsimul$T_A_Q2 + qsimul$T_A_Q4

r_m <- cor(m1,m2)
(fiab2m <- 2*r_m /(1+r_m))

r_m <- c(cor(m1,m2),cor.test(m1,m2)$conf.int)
(fiab2m <- 2*r_m /(1+r_m))


### Consistencia interna ####
# Consisteix en comparar la variabilitat de les preguntes amb la variabilitat
# dels resultats. Implica analitzar el qüestionari com si cada pregunta fos
# un test independent

# La mesura més habitual és l'alpha de Cronbach. D'altres són les fórmules
# de Kuder-Richardson, la lambda-6 de Guttman o l'omega de McDonald.

psych::alpha(qsimul[,2:5])
omega(qsimul[,2:5],nfactors = 1)

# La referència de la fiabilitat sol ser de 0.7.


#### VALIDESA ####
# Mesura el qüestionari el que volem que mesuri?
# Perspectives:
# - Validesa de contingut
# - Validesa concurrent (predictiva)
# - Validesa de constructe
# - Validesa aparent


### Validesa de contingut ####
# Sol avaluar-se qualitativament a partir de mètodes basats en d'experts


### Validesa concurrent ####
# S'avalua com la correlació entre la puntuació del constructe i el valor 
# per a un altre estimador de la variable d'interès, per als mateixos
# subjectes.

# Quan no es disposa d'aquest tipus d'informació, de vegades es critica la
# validesa, comparant els resultats de grups de subjectes amb els resultats
# esperats

# Quina és la validesa predictiva del test d'admissió als estudis de
# medicina (dataMedical)?

? dataMedical
med <- get(data("dataMedical"))

med$total <- rowSums(med[,1:100])
head(med)

str(med[,102:103])
med$StudySuccess <- factor(med$StudySuccess)
levels(med$StudySuccess)<-c("No","Yes")
med <- med[!is.na(med$StudySuccess),]
str(med[,102:103])

ggplot(med, aes(x=StudySuccess, y=total)) +
  geom_violin(col="black", bw=2) +
  geom_boxplot(col="grey",outlier.shape = NA,width=.3) +
  labs(y="Test", x = "Study Success")+
  coord_flip()+
  theme_classic()

polychor(med$StudySuccess,med$total,std.err = TRUE)
cor.test(as.numeric(med$StudySuccess),med$total)


### Validesa de constructe ####
# Consisteix a determinar la dimensionalitat de l'instrument i si es
# mantenen les estructures del constructe previstes en el seu disseny.

# Mirem el test del BFI...
? bfi
dades <- get(data(bfi))

# Dimensionalitat
(eigv <- eigen(cor(na.omit(dades[,1:25])))$values)
sum(eigv > 1)

ggplot(NULL, aes(x=1:25,y=eigv, group=1)) +
  geom_point() + geom_line() +
  geom_hline(aes(yintercept=1), linetype=2)+
  theme_classic()

# Correlograma amb anàlisi de conglomerats
corrplot(cor(na.omit(dades[,1:25])))
corrplot(abs(cor(na.omit(dades[,1:25]))),order="hclust",hclust.method="ward.D2",
         addrect=5)

# Per a anàlisis més acurats de la validesa de constructe, s'usen mètodes de CFA,
# sovint basats en equacions estructurals.


### Validesa aparent ####
# Consisteix a estudiar si els qui responent el qüestionari perceben 
# correctament el que se'ls vols preguntar. És una mesura de confiança
# en el qüestionari i s'interpreta a major validesa aparent millor respostes
# dels participants. Se sol estudiar a través d'entrevistes dirigides.


#### ANÀLISI D'ITEMS ####
# En el procés de millora d'un qüestionari és important identificar quins ítems
# són els que més contribueixen a una bona mesura i quin són els que, en canvi,
# no semblen aportar a la construcció d'un instrument vàlid i fiable.

# Índexs que s'usen de forma habitual per a l'anàlisi d'ítems són
# - l'índex de dificultat (per a indicadors que tenen una resposta correcta),
# - l'índex de discriminació,
# - l'índex d'homogeneïtat,
# - l'index de fiabilidad, i
# - l'índex de validesa

# En preguntes d'opció múltiple amb resposta correcta, també es realitza amb 
# l'anàlisi dels distractors.


# Reprendre'm per a aquestes anàlisis la forma A del qüestionari simulat
qsimul <- read.table("QSimulat.txt",sep="\t",header=TRUE)
qsimul$ta <- qsimul$T_A_Q1 + qsimul$T_A_Q2 + qsimul$T_A_Q3 + qsimul$T_A_Q4

### Índex de dificultat ####
# Correspon a la puntuació mitjana sobre la puntuació màxima teòrica.
# S'han d'evitar ítems massa fàcils i ítems massa dífícils.

(indDif <- colMeans(qsimul[,2:5]) / 5)

### Índex de discriminació ####
qsimul_ao <- qsimul[order(qsimul$ta),c(2:5,18)]

# Criteris habituals consisteixen a comparar meitats, terços extrems o
# 27 % extrems de la mostra, ordenada per la puntuació del constructe

mm <- round(nrow(qsimul_ao) * .27)
(indDis <- (colMeans(qsimul_ao[nrow(qsimul_ao):(nrow(qsimul_ao)-mm+1),1:4]) -
              colMeans(qsimul_ao[1:mm,1:4])) / 5)

mm <- round(nrow(qsimul_ao) * .5)
(indDis <- (colMeans(qsimul_ao[nrow(qsimul_ao):(nrow(qsimul_ao)-mm+1),1:4]) -
              colMeans(qsimul_ao[1:mm,1:4])) / 5)


### Índex d'homogeïnetat ####
# S'analitza a partir de la correlació amb el constructe, o la resta d'elements
# del constructe

(indHom <- apply(qsimul[2:5], 2, cor, qsimul$ta))
(indHom <- apply(qsimul[2:5], 2, function(x) cor(x,qsimul$ta-x)))


### Índex de fiabilitat ####
(indFia <- apply(qsimul[2:5], 2, cor, qsimul$ta) * 
   apply(qsimul[2:5], 2, sd) / 5)


### Índex de validesa ####
# Correspon a les correlacions de cada element amb una mesura externa de la
# variable d'interès.


### Anàlisi dels distractors ####
# Es tracta d'estudiar les freqüències relatives d'aparició de les respostes
# incorrectes. Es recomana revisar els distractors de freqüencia inferior al
# 5 % i aquells més freqüents que la resposta correcta.


#### SHINY PER A L'ANÀLISI DE QÜESTIONARIS ###
startShinyItemAnalysis()


#### EXERCICIS 2 #### 
# 
# 5- Analitza la fiabilitat, la dimensionalitat i les característiques dels ítems 
# del qüestionari Homeostasis Concept Inventory (HCI). El paquet ShinyItemAnalysis 
# inclou dades de respostes al mateix

help("HCI")
help("HCItest")
help("HCIkey")


#### REFERÈNCIES ####

# http://biostat.mc.vanderbilt.edu/wiki/pub/Main/StatGraphCourse/graphscourse.pdf
# https://cehs01.unl.edu/aalbano/intromeasurement/main.html
# https://www.personality-project.org/readings-measurement.html#psycho

