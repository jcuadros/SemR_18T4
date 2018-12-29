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


#### EXERCICIS 1 #### 
# Seguim amb el qüestionari 'Sleep in America 2013' 

dadesSiA <- read.table("2013sleep.txt", header=TRUE, stringsAsFactors = TRUE,
                       sep="\t", quote="")

# 1- Presenta els resultats de la pregunta 26
# 26. Do you drink alcoholic beverages?
# 01 Yes
# 02 No
# 98 DO NOT READ: Refused
# 99 DO NOT READ: Don’t know/Not sure

resp26 <- dadesSiA$q26
resp26 <- factor(resp26)
levels(resp26)
levels(resp26) <- c("Yes","No")

# Taula de freqüències
tfreq26 <- table(resp26, useNA="ifany")
tfreq26
sum(tfreq26)

# Diagrama de barres (mantenint l'ordre de les respostes)
ggplot(NULL,aes(x=resp26)) +
  geom_bar(col="black",fill="lightgrey") +
  scale_x_discrete(limits = rev(levels(resp26)))+
  theme_classic() +
  coord_flip() +
  labs(x=NULL, y="Freqüència absoluta") +
  theme(legend.position = "none")


# 2- Presenta els resultats de la pregunta 30
# During the past two weeks, how would you rate your overall sleep quality? 
# Would you say…
# 01 Very good,
# 02 Fairly good,
# 03 Fairly bad, or
# 04 Very bad
# 98 DO NOT READ: Refused
# 99 DO NOT READ: Don’t know/Not sure

resp30 <- factor(dadesSiA$q30, ordered=TRUE)
table(resp30)

levels(resp30) <- c("Very good","Fairly good",
                    "Fairly bad","Very bad")

# Taula de freqüències
(tfreq30 <- table(resp30, useNA="ifany"))
sum(tfreq30)
(tfreq30 <- tfreq30 / 10)


# Diagrama de barres
ggplot(NULL,aes(x=resp30)) +
  geom_bar(col="black",fill="lightgrey") +
  scale_x_discrete(limits = rev(levels(resp30)))+
  theme_classic() +
  coord_flip() +
  labs(x=NULL, y="Freqüència absoluta") +
  theme(legend.position = "none")


# Diagrama de barres apilades centrades
df30 <- data.frame(tfreq30)
df30$resp30 <- factor(df30$resp30, 
                           levels=rev(levels(resp30)),
                           ordered=TRUE)
df30 <- df30[order(df30$resp30),]
min_freq <- -sum(df30$Freq[1:2])
df30$ymin <- cumsum(c(min_freq,df30$Freq[1:(nrow(df30)-1)]))
df30$ymax <- df30$ymin+df30$Freq

ggplot() +
  geom_rect(aes(xmin=.5, xmax=1.5, ymin=ymin, ymax=ymax, fill=resp30),
           data=df30, col="black") +
  theme_classic() +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank())+
  labs(y="Freqüència relativa (%)")+
  scale_y_continuous(labels=abs) +
  scale_fill_manual(values=brewer.pal(4,"RdBu")) +
  guides(fill=guide_legend(title="",reverse = TRUE))


# 3- Presenta els resultats de la pregunta 42
# How much time per day did you spend sitting in the past 7 days? Your best 
# estimate is fine.
# DO NOT ACCEPT RANGES.
# __ __ Hours per day [Range: 00-24]
# __ __ Minutes per day [Range: 00-59]
# 98 DO NOT READ: Refused
# 99 DO NOT READ: Don’t know/Not sure

resp42 <- dadesSiA$q42hour + dadesSiA$q42min / 60    # en hores

# Estadístics paramètrics
summary(resp42)
mean(resp42, na.rm = TRUE)
sd(resp42, na.rm = TRUE)

# Histograma
ggplot(NULL, aes(x=resp42)) +
  geom_histogram(binwidth = 1, fill="lightgrey", col="black") +
  labs(y="Freqüencia absoluta", x="Hores assegut")+
  theme_classic()

ggplot(NULL, aes(x=resp42, y=..density..)) +
  geom_histogram(binwidth = 1, fill="lightgrey", col="black") +
  geom_density(bw=1) +
  labs(y=NULL, x="Hores assegut")+
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# Boxplot
ggplot(NULL, aes(x=1, y=resp42)) +
  geom_boxplot(col="black",outlier.shape = NA,width=.05) +
  geom_jitter(alpha=.2, shape=21, width=.2, height=.25, 
              color="green") +
  labs(y="Hores assegut", x = NULL)+
  coord_flip()+
  theme_classic() + 
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y= element_blank())


# 4- Estudia la relació entre la pregunta 17 i la pregunta 29
# ---
# 17. Thinking about the past two weeks, how many minutes, on most worknights or 
# weeknights, does it take you to fall asleep? Would you say…(READ LIST.)
# 01 Less than 5 minutes,
# 02 5 up to 10 minutes,
# 03 10 up to 15 minutes,
# 04 15 up to 30 minutes,
# 05 30 up to 45 minutes,
# 06 45 minutes up to 1 hour, or
# 07 1 hour or more?
# 96 DO NOT READ: Depends/Varies
# 98 DO NOT READ: Refused
# 99 DO NOT READ: Don’t know/Not sure
# ---
# 29. Thinking about the last two weeks, how many 12 ounce servings of caffeinated
# beverages, such as soda, soft drinks, coffee, tea, and energy drinks do you drink
# on an average weekday or workday …(READ LIST. RECORD NUMBER FOR EACH BELOW. 
# DO NOT ACCEPT RANGES. 98=REFUSED; 99=DON’T KNOW; 00=NONE; 97=LESS THAN ONE.)

resp17 <- dadesSiA$q17
table(resp17)

resp17[resp17=="99"]<-"96"
table(resp17)

resp17 <- factor(resp17, ordered=TRUE)
levels(resp17) <- c("Less than 5 minutes","5 up to 10 minutes", 
                    "10 up to 15 minutes", "15 up to 30 minutes",
                    "30 up to 45 minutes","45 minutes up to 1 hour",
                    "1 hour or more", "Not answered")
table(resp17)

resp29a <- dadesSiA$q29a
resp29b <- dadesSiA$q29b
resp29c <- dadesSiA$q29c

table(resp29a, useNA = "always")
table(resp29b, useNA = "always")
table(resp29c, useNA = "always")

resp29a[resp29a > 97] <- NA
resp29b[resp29b > 97] <- NA
resp29c[resp29c > 97] <- NA

resp29a[resp29a == 97] <- 0
resp29b[resp29b == 97] <- 0
resp29c[resp29c == 97] <- 0

table(resp29a, useNA = "always")
table(resp29b, useNA = "always")
table(resp29c, useNA = "always")

df17i29 <- data.frame(resp17, resp29a, resp29b, resp29c)
colnames(df17i29) <- c("Temps per adormir-se", "5:00-12:00",
                       "12:00-17:00",
                       "17:00-5:00")
df17i29 <- gather(df17i29, "Horari", "Begudes cafeïnades", -1)
df17i29[,2] <- factor(df17i29[,2], levels = c("5:00-12:00",
                  "12:00-17:00","17:00-5:00"), ordered=TRUE)

###############

# Boxplot
ggplot(df17i29, aes(y=`Begudes cafeïnades`,x=1)) +
  geom_boxplot(col="black",outlier.shape = NA,width=.3) +
  geom_jitter(alpha=.2, shape=21,width=.3, height=.25) +
  facet_grid(`Horari`~`Temps per adormir-se`)+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank())

# Violin plot
ggplot(df17i29, aes(y=`Begudes cafeïnades`,x=1)) +
  geom_violin(col="black", bw=2) +
  geom_boxplot(col="grey",outlier.shape = NA,width=.3) +
  facet_grid(`Horari`~`Temps per adormir-se`)+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank())

# Diagrama de línies

df17i29sum <- df17i29 %>% group_by(`Horari`,`Temps per adormir-se`) %>% 
  summarise(mitjana=mean(`Begudes cafeïnades`,na.rm=TRUE))

df <- df17i29[df17i29$`Temps per adormir-se`!="Not answered",]
dfs <- df17i29sum[df17i29sum$`Temps per adormir-se`!="Not answered",]

ggplot(df, aes(y=`Begudes cafeïnades`,x=`Temps per adormir-se`,color=`Horari`)) +
  geom_point(shape=21,size=3,alpha=.5,
             position=position_jitterdodge(jitter.width=.8,dodge.width=.8)) +
  geom_line(aes(y=mitjana,group=`Horari`),size=1,data=dfs) +
  scale_color_manual(values=brewer.pal(3,"Dark2")) +
  theme_classic() +
  theme(legend.position = "top")
  

#### EXERCICIS 2 #### 
# 
# 5- Analitza la fiabilitat, la dimensionalitat i les característiques dels ítems 
# del qüestionari Homeostasis Concept Inventory (HCI). El paquet ShinyItemAnalysis 
# inclou dades de respostes al mateix

help("HCI")
help("HCItest")
help("HCIkey")

## Fiabilitat
hci <- get(data(HCI))

# Dues meitats (parells vs senars)
m1 <- rowSums(hci[,((1:10)*2)-1])
m2 <- rowSums(hci[,((1:10)*2)])
rm12 <- cor(m1,m2)
(fiab <- 2*rm12/(1+rm12))

# Alpha de Cronbach
psych::alpha(hci[,1:20])

# El qüestionari és suficientment fiable, ja que la fiabilitat és superior al
# valor de 0,7 (referència habitual).


## Dimensionalitat
(eigv <- eigen(cor(na.omit(hci[,1:20])))$values)

ggplot(NULL, aes(x=1:20,y=eigv, group=1)) +
  geom_point() + geom_line() +
  geom_hline(aes(yintercept=1), linetype=2)+
  theme_classic()

# Les dades apunten a l'existència d'una mesura unidimensional.


## Anàlisi d'ítems

# Variabilitat
apply(hci[,1:20],2, sd)

# Tots els ítems presenten una variabilitat adequada.


# Índexs d'homogeneïtat
hci$tot <- rowSums(hci[,1:20])
apply(hci[,1:20],2, function(x) cor(hci$tot-x,x))

# Pot ser convenient revisar o millorar els ítems 17 i 7.


# Índexs de dificultat
(idif <- apply(hci[,1:20],2, mean))

stem(round(idif,2),scale=2)

# La dificultat dels elements del qüestionari no sembla presentar problemes, 
# ni per valor ni per distribució.
