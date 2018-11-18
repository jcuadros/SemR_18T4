##=========================================================================##
##                                                                         ##
##  INTRODUCCIÓ A LA VISUALITZACIÓ ANALÍTICA AMB R                         ##
##  @authors: Vanessa Serrano, Jordi Cuadros, Francesc Martori             ##
##                                                                         ##
##=========================================================================##

#### INSTAL·LACIÓ I CÀRREGA DE GGPLOT2 #### 
# ggplot2 està inclòs en el paquet tidyverse
library("tidyverse")

#### ELEMENTS PRINCIPALS: DADES, MAPATGE ESTÈTIC I GEOMETRIES #### 
# Treballarem amb el conjunts d'Anscombe
anscombe
str(anscombe)
summary(anscombe)

summaryXY <- function(x,y) {
  c(x.mean = mean(x), x.sd = sd(x), y.mean = mean(y), y.sd = sd(y),
    r = cor(x,y))
}

summaryXY(anscombe$x1, anscombe$y1)
summaryXY(anscombe$x2, anscombe$y2)
summaryXY(anscombe$x3, anscombe$y3)
summaryXY(anscombe$x4, anscombe$y4)

ggplot(data=anscombe, mapping=aes(x=x1,y=y1)) + geom_point()
#ggplot(anscombe, aes(x=x1,y=y1)) + geom_point()
ggplot(data=anscombe, mapping=aes(x=x2,y=y2)) + geom_point()
ggplot(data=anscombe, mapping=aes(x=x3,y=y3)) + geom_point()
ggplot(data=anscombe, mapping=aes(x=x4,y=y4)) + geom_point()

ggplot(data=anscombe) + geom_point(aes(x=x1,y=y1), col="red", shape=1, size=3) +
  geom_point(aes(x=x2,y=y2), col="darkgreen",shape=4, size=3) +
  geom_point(aes(x=x3,y=y3), col="blue", shape=2, size=3) +
  geom_point(aes(x=x4,y=y4), col="magenta", shape=5, size=3) +
  theme_classic()

#### EXERCICIS 1 #### 
# A partir del conjunt de dades mtcars 
mtcars
? "mtcars"
str(mtcars)

# 1- Crea una visualització que permeti discutir la relació
# entre la potència (hp) i l'acceleració(qsec)

# 2- Afegeix a aquesta visualització la variable pes (wt)

# 3- Estudia l'efecte del nombre de cilindres (cyl) en la relació entre
# potència i acceleració

# 4- Crea un histograma que et permeti estudiar la distribució de la
# potència

# 5- Crea un conjunt de diagrames de caixa tals que permetin estudiar
# la relació entre potència i nombre de cilindres


#### GRÀFICS MÉS COMUNS #### 
# ...fins i tot amb moltes dades
?diamonds
str(diamonds)
diaT <- diamonds
dia100 <- diamonds[sample(1:nrow(diamonds), 100),]
dia2000 <- diamonds[sample(1:nrow(diamonds), 2000),]

## Diagrames de dispersió
ggplot(dia100, aes(x=carat,y=price)) + geom_point()
ggplot(dia2000, aes(x=carat,y=price)) + geom_point()
ggplot(diaT, aes(x=carat,y=price)) + geom_point()  # No OK

ggplot(diaT, aes(x=carat,y=price)) +
  geom_point(shape=21,fill="green", alpha=0.1)+
  geom_smooth(color="darkgreen")

ggplot(diaT, aes(x=carat,y=price)) + 
  geom_density2d(bins=100,alpha=0.2,color="red")+
  geom_smooth(color="darkred",fill="pink",alpha=0.5)

?geom_smooth

# ggplot(diaT, aes(x=carat,y=price)) + 
#   geom_density2d(bins=100,alpha=0.2,color="red")+
#   geom_smooth(method="gam",
#               formula=y ~ s(x, bs = "cs"),
#               color="darkred",fill="pink",alpha=0.5)

ggplot(dia2000, aes(x=carat,y=price)) + 
  geom_density2d(bins=50,alpha=0.2,color="red")+
  geom_rug(alpha=0.01)

ggplot(dia100,aes(x=cut,y=color))+
  geom_point()                                     # No OK

?geom_point

ggplot(dia100,aes(x=cut,y=color))+
  geom_jitter(width=0.1,height=0.1,shape=21)

ggplot(diaT,aes(x=cut,y=color))+
  geom_jitter(shape=21,alpha=0.1)

ggplot(diaT,aes(x=cut,y=color))+
  geom_count()

## Diagrames de caixa
ggplot(diaT, aes(x=cut,y=price)) + geom_boxplot()

ggplot(diaT, aes(x=cut,y=price)) +
  geom_jitter(height = 0,alpha = 0.1, shape = 21, size = 1) + 
  geom_boxplot(outlier.shape = NA,notch = TRUE,alpha=0.5,fill="green",
               color="green4", width=0.5)
?geom_boxplot

## Histogrames
ggplot(diaT, aes(x=carat)) + 
  geom_histogram(binwidth = 0.1, boundary=0, fill="grey",color="black")

?geom_histogram

ggplot(diaT, aes(x=carat)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.1, boundary=0, fill="green",color="black", alpha=0.2)+
  geom_density(color="green4",size=1)

ggplot(diaT, aes(x=carat)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.1, boundary=0, fill="green",color="black", alpha=0.2)+
  geom_density(color="green4",size=1)+geom_rug(alpha=0.01)

## Diagrama de barres
ggplot(diaT, aes(x=clarity)) + geom_bar()

ggplot(diaT, aes(x=clarity, fill=cut)) + geom_bar()

ggplot(diaT, aes(x=clarity, fill=cut)) + geom_bar(position="dodge")

ggplot(diaT, aes(x=clarity, fill=cut)) + geom_bar(position="fill")

# diaT_byCC <-as.data.frame(table(diaT$clarity,diaT$cut))
# colnames(diaT_byCC)<- c("clarity","cut","freq")
# diaT_byCC$rel <- apply(diaT_byCC,1, function(x) as.numeric(x[3]) / 
#                          sum(as.numeric(diaT_byCC$freq[diaT_byCC$clarity==x[1]])))
# ggplot(diaT_byCC, aes(x=clarity, y=rel, fill=cut)) + 
#   geom_bar(stat="identity",position="dodge")
# 
# diaT_byCC$rel <- apply(diaT_byCC,1, function(x) as.numeric(x[3]) / 
#                          sum(as.numeric(diaT_byCC$freq[diaT_byCC$cut==x[2]])))
# ggplot(diaT_byCC, aes(x=clarity, y=rel, fill=cut)) + 
#   geom_bar(stat="identity",position="dodge")


#### EXERCICIS 2 #### 
# A partir del conjunt de dades SLID (Survey of Labour and Income Dynamics)
# del 1994 a Ontario (Canadá)
if(!("car" %in% library()$results[,1])) install.packages("car")
?SLID
dsSLID <- car::SLID
str(dsSLID)
help("SLID",package="car")

# 1- Crea una visualització que permeti discutir la relació
# entre l'edat i els anys d'escolarització

# 2- Aquesta relació és diferent en funció de l'idioma?

# 3- Mostra amb un gràfic si la proporció de població que parla un idioma
# depén de l'edad

# 4- Estudia la distribució de les persones enquestades per idioma

# 5- Representa la distribució d'anys d'escolarització

# 6- Hi ha alguna diferència entre les distribució d'anys
# d'escolarització en funció del gènere


#### ELEMENTS ADDICIONALS DE GGPLOT2 #### 

diaT <- diamonds
dia2000 <- diamonds[sample(1:nrow(diamonds), 2000),]

## Escales
# Ajust dels eixos
ggplot(dia2000, aes(x=carat,y=price)) +
  geom_point(shape=21,fill="green", alpha=0.1)+
  geom_smooth(color="darkgreen")

ggplot(dia2000, aes(x=carat,y=price)) +
  geom_point(shape=21,fill="green", alpha=0.1)+
  geom_smooth(color="darkgreen") +
  scale_x_continuous(breaks=seq(0.6,2,by=0.2))

# Escales de color
ggplot(diaT, aes(x=clarity, fill=cut)) + geom_bar(position="dodge")

pal <- colorRampPalette(c("#FF6666","#FFFF00","#66FF66"))
ggplot(diaT, aes(x=clarity, fill=cut)) + geom_bar(position="dodge") + 
  scale_fill_manual(values = pal(5))

ggplot(diaT, aes(x=clarity, fill=cut)) + geom_bar(position="dodge", color="black") + 
  scale_fill_brewer(type="div", palette="PuOr")
?scale_fill_brewer

# Escales no lineals
ggplot(dia2000, aes(x=carat,y=price)) +
  geom_point(shape=21,fill="green", alpha=0.1)+
  geom_smooth(color="darkgreen") +
  scale_y_continuous(breaks=c(500,1000,5000,10000),
                     minor_breaks=c(1:10*100,1:10*1000,1:3*10000),trans="log10")

## Coordenades
# Zoom
ggplot(dia2000, aes(x=carat,y=price)) +
  geom_point(shape=21,fill="green", alpha=0.1)+
  geom_smooth(color="darkgreen")+
  coord_cartesian(xlim=c(0.3,1.4))

# Rotació
ggplot(dia2000, aes(x=cut, y=price))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=21, alpha=0.1) + coord_flip()

# Escales iguales... coord_equal()

## Facetes
ggplot(dia2000, aes(x=carat,y=price)) +
  geom_point(shape=21, alpha=0.3)+
  geom_smooth(method="lm")+
  facet_grid(clarity ~ cut)

ggplot(dia2000, aes(x=carat,y=price)) +
  geom_density2d()+
  facet_grid(clarity ~ cut)

ggplot(dia2000, aes(x=cut, y=price, col=color))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=21, alpha=0.2) + coord_flip() +
  facet_wrap(~color)

ggplot(dia2000, aes(x=cut, y=price, col=color))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=21, alpha=0.2) + coord_flip() +
  facet_grid(.~color)


#### NON-DATA INK ####
## Temes
dia2000esf <- dia2000
dia2000esf$esphericity <- sqrt(dia2000$x ^ 2 + dia2000$y ^ 2 + dia2000$z ^ 2) /
  pmax(dia2000$x,dia2000$y,dia2000$z) / sqrt(3) - 1

graf_esf <- ggplot(dia2000esf, aes(x=esphericity,y=price/carat, col=cut)) +
  geom_point(alpha=0.3) +
  coord_cartesian(xlim=c(-0.13,-0.10), ylim=c(0,12000))+
  facet_wrap(~cut)

graf_esf
graf_esf + theme_bw()
graf_esf + theme_tufte()
graf_esf + theme_few()
graf_esf + theme_minimal()

graf_esf + theme(axis.text=element_blank())
graf_esf + theme(legend.position="bottom")
graf_esf + theme(axis.text.x=element_text(angle=90, size=rel(.8), vjust=0.5),
                 axis.text.y=element_text(size=rel(.8), hjust=0.5),
                 legend.text = element_text(size=rel(.8)),
                 legend.title = element_blank(),
                 strip.background = element_rect(fill="#000000"),
                 strip.text = element_text(color="white",size=rel(.8)),
                 panel.background = element_rect(fill="white"),
                 axis.line = element_line(color="grey"))
graf_esf2 <- graf_esf + theme(axis.text.x=element_text(angle=90, size=rel(.8), vjust=0.5),
                              axis.text.y=element_text(size=rel(.8), hjust=0.5),
                              legend.text = element_text(size=rel(.8)),
                              legend.title = element_blank(),
                              strip.background = element_rect(fill="#000000"),
                              strip.text = element_text(color="white",size=rel(.8)),
                              panel.background = element_rect(fill="white"),
                              axis.line = element_line(color="grey"))

## Títols
graf_esf2 + labs(x="deviation from esphericity", y="price per carat")

## Llegenda
# Per a eliminar tota la llegenda
graf_esf2 + theme(legend.position = "none")

# Per a eliminar la llegenda per a un mapatge concret (guides)
ggplot(dia2000, aes(x=carat,y=price,col=color,shape=cut)) +
  geom_point(alpha=.5) + guides(shape="none")

## Textos
# Es poden afegir amb el mapatge "label" a través de geom_text() o de
# annotate()
ggplot(dia2000, aes(x=carat,y=price)) + geom_point(alpha=.1,shape=21) +
  annotate("rect", xmin=3, xmax=5, ymin=1000, ymax=5000, 
           color = "black", fill = "#FFFF99", size = 1) + 
  annotate("text", x = 4, y = 3000, vjust = .5, hjust = .5,
           label = "El tamaño importa, pero debe\nhaber algo más",
           size=2)


#### ALTRES TIPUS DE GRÀFICS D'ÚS COMÚ #### 
if(!require("GGally")) {
  install.packages("GGally")
  library("GGally")
}
if(!require("corrplot")) {
  install.packages("corrplot")
  library("corrplot")
}
if(!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library("RColorBrewer")
}

## Matriu de gràfics de dispersió
ggpairs(dia2000[,c("price","cut","clarity","carat")], axisLabels = "none") + 
  theme_few()

## Gràfic de correlacions (corrgrams)
dia2000n <- dia2000
for(i in 1:ncol(dia2000n)) dia2000n[,i] <- as.numeric(unlist(dia2000n[,i]))
corrplot(cor(dia2000n,method="s"),order = "hclust",addrect = 3,
         mar = c(2, 1, 1, 1), cl.length = 5)
ggcorr(dia2000n)
ggcorr(dia2000n, geom="circle", nbreaks = 8, palette = "PuOr")

## Heatmap
diaT %>% group_by(cut,clarity) %>% summarise(mprice=quantile(price,.9)) %>%
  ggplot(aes(x=cut,y=clarity,fill=mprice))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')(100)) +
  theme(axis.text.x=element_text(angle=90,hjust=1))

diaT %>% group_by(cut,clarity) %>% summarise(mcarat=quantile(carat,.9)) %>%
  ggplot(aes(x=cut,y=clarity,fill=mcarat))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')(100)) +
  theme(axis.text.x=element_text(angle=90,hjust=1))

diaT %>% group_by(cut,clarity) %>% summarise(pxc=quantile(price/carat,.9)) %>%
  ggplot(aes(x=cut,y=clarity,fill=pxc))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')(100)) +
  theme(axis.text.x=element_text(angle=90,hjust=1))

## Waffle chart
nxcut <- diaT %>% group_by(cut) %>% summarise(number=length(cut))
nxcutpc <- nxcut
nxcutpc$pc <- nxcutpc$number / sum(nxcutpc$number) * 100
nxcutpc$percent <- floor(nxcutpc$pc)
nxcutpc <- nxcutpc[order(desc(nxcutpc$pc-nxcutpc$percent)),]
nxcutpc$percent <- nxcutpc$percent + ifelse(1:nrow(nxcutpc)>100-sum(nxcutpc$percent),0,1) 
nxcutpc <- nxcutpc[order(desc(nxcutpc$percent)),]

df <- expand.grid(x=1:10,y=1:10)
df$cut <- rep(nxcutpc$cut, nxcutpc$percent)

ggplot(df,aes(x=x,y=y,fill=cut))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

## Treemap
if(!require("devtools")) {
  install.packages("devtools")
  library("devtools")
}
if(!require("stringi")) {
  install.packages("stringi")
  library("stringi")
}
if(!require("ggfittext")) {
  install_github("wilkox/ggfittext")
  library("ggfittext")
}
if(!require("treemapify")) {
  install_github("wilkox/treemapify")
  library("treemapify")
}

nxcut <- diaT %>% group_by(cut) %>% summarise(number=length(cut))
ggplotify(treemapify(nxcut,area="number",fill="cut",label="cut"))

nxcut <- diaT %>% group_by(cut,clarity) %>% summarise(number=length(cut))
ggplotify(treemapify(nxcut,area="number",fill="cut",label="clarity",group="cut"),
          label.size=10, group.label.size = 18) +
  theme(legend.position = "none")


## Mapes
# Localitzacions
if(!require("rworldmap")) {
  install.packages("rworldmap")
  library("rworldmap")
}

mapWorld <- getMap(resolution = 'low')
ggplot(mapWorld,aes(x=long,y=lat,group=group)) + geom_polygon(fill="white",col="grey") +
  geom_point(data=NULL,inherit.aes = FALSE,aes(y=41.3851,x=2.1734),shape=16,
             col="red")+coord_equal()

ggplot(mapWorld,aes(x=long,y=lat,group=group)) + geom_polygon(fill="white",col="grey") +
  geom_point(data=NULL,inherit.aes = FALSE,aes(y=41.3851,x=2.1734),shape=16,
             col="#FF9999",alpha=0.01,size=5)+coord_equal(xlim=c(-10,5),ylim=c(35,45))


# Choropleth

# Municipis de Catalunya
# Font de dades
# - http://www.diva-gis.org/Data

if(!require("rgdal")) {
  install.packages("rgdal")
  library("rgdal")
}
if(!require("rgeos")) {
  install.packages("rgeos")
  library("rgeos")
}
if(!require("maptools")) {
  install.packages("maptools")
  library("maptools")
}
if(!require("mapproj")) {
  install.packages("mapproj")
  library("mapproj")
}

limitsES <- readOGR("ESP_adm/ESP_adm4.shp",encoding="UTF-8",use_iconv = T)
limitsES@data$NAME_4

mapdata <- fortify(limitsES, region="ID_4")
ccaa <- fortify(limitsES, region="ID_1")
ggplot(mapdata,aes(x=long,y=lat,group=group))+geom_polygon()
str(ccaa)

mun_ca<-data.frame(ca_id=limitsES@data$ID_1,ca_nom=limitsES@data$NAME_1,
                   mun_id=limitsES@data$ID_4,mun_nom=limitsES@data$NAME_4)
unique(mun_ca$ca_nom)
ids_munCA <- mun_ca$mun_id[mun_ca$ca_nom=="Cataluña"]
length(ids_munCA)
mapdataCA <- mapdata %>% filter(id %in% ids_munCA)

ggplot(mapdataCA,aes(x=long,y=lat,group=group)) + 
  geom_polygon(fill="white",col="black")

ggplot(mapdataCA,aes(x=long,y=lat,group=group)) + 
  geom_polygon(fill="white",col="black") + coord_map()

# ...marcant les vilanoves
ids_munVilanova <- mun_ca$mun_id[grepl("vilanova",
                                       mun_ca$mun_nom,ignore.case = T)]

ggplot(mapdataCA,aes(x=long,y=lat,group=group,fill=(id %in% ids_munVilanova))) + 
  geom_polygon(col="black")+
  coord_map()+theme_minimal()+
  theme(legend.position="none",axis.text = element_blank(),
        axis.title = element_blank(),panel.grid= element_blank())

#... amb etiquetes
if(!require("ggrepel")) {
  install.packages("ggrepel")
  library("ggrepel")
}

mapdataCA_ave <- mapdataCA %>% group_by(id) %>% 
  summarise(ave_long=mean(long),ave_lat=mean(lat))
mapdataCA_ave <- merge(mapdataCA_ave,mun_ca,by.x="id",by.y="mun_id")

ggplot(mapdataCA,aes(x=long,y=lat,group=group,fill=(id %in% ids_munVilanova))) + 
  geom_polygon(col="lightgrey")+
  geom_text_repel(data=mapdataCA_ave, mapping=aes(x=ave_long, y= ave_lat,
                                                  label=ifelse(mapdataCA_ave$id %in% ids_munVilanova,
                                                               as.character(mapdataCA_ave$mun_nom),"")),inherit.aes = FALSE,
                  col="black",size=3)+
  coord_map()+
  scale_fill_manual(values=c("white","cyan"))+
  theme_minimal()+
  theme(legend.position="none",axis.text = element_blank(),
        axis.title = element_blank(),panel.grid= element_blank())


# Més idees de gràfics es poden trobar a
# - http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# - http://www.r-graph-gallery.com/


#### EXERCICIS 2 #### 
# Big Mart Data: https://docs.google.com/spreadsheets/d/1PR5StHxg2jlMCb4IUilGSEwhylXn-3q3EJucSaVolCU/edit#gid=0
# 
# Descripció de les variables:
# *  Item_Identifier 	Unique product ID
# *  Item_Weight 	Weight of product
# *  Item_Fat_Content 	Whether the product is low fat or not
# *  Item_Visibility 	% of total display area in store allocated to this product
# *  Item_Type 	Category to which product belongs
# *  Item_MRP 	Maximum Retail Price (list price) of product
# *  Outlet_Identifier 	Unique store ID
# *  Outlet_Establishment_Year 	Year in which store was established
# *  Outlet_Size 	Size of the store
# *  Outlet_Location_Type 	Type of city in which store is located
# *  Outlet_Type 	Grocery store or some sort of supermarket
# *  Item_Outlet_Sales 	Sales of product in particular store


bgmd <- read.table("BigMartDataset.tsv", header=T, sep="\t",
                   quote="", dec=".", na.strings = "")
str(bgmd)
colnames(bgmd) <- c("item","weight", "fat", "visibility",
                    "type", "price", "outlet", "outletYear",
                    "outletSize", "outletLoc","outletType", "sales")

# 1- Usa una matriu de gràfics de dispersió per visualitzar quines relacions
# entre les variables quantitatives de la taula de dades referides als productes


# 2- Visualitza la similitud entre establiments com a correlació entre les
# vendes dels diferents productes


# 3- Crea una visualització que permeti comparar les vendes dels diferents productes
# en els diferents establiments


# 4- Crea una visualització que permeti comparar les vendes dels diferents 
# tipus de productes en els diferents tipus d'establiments


# 5- Visualitza la distribució dels tipus de productes que més es venen
# en un tipus d'establiment usant un Waffle chart


# 6- Mostra la relació entre preu i volum de vendes per 
# als diferents tipus de productes



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

