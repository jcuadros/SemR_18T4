#### EXERCICIS 1 #### 
# Treballarem amb el text de l'Auca del Senyor Esteve...
# Està en el fitxer "pg_AucaDelSenyorEsteve.txt"

# 1- Carrega el fitxer de text i ajusta'l de forma que només hi hagi
# el contingut de l'obra en una variable de text.
auca <- readLines("pg_AucaDelSenyorEsteve.txt", encoding = "UTF-8")
auca <- auca[92:6046]
head(auca,10)
tail(auca,10)

auca <- paste(auca, collapse="\n")
auca <- gsub("\n+", "\n", auca)

# 2- Quantes paraules hi ha a l'obra?
paraulesAuca <- unlist(strsplit(auca, split="[[:space:]:.——]"))
paraulesAuca <- gsub("[^[:alpha:]'·-]","",paraulesAuca)
paraulesAuca <- paraulesAuca[nchar(paraulesAuca) > 0]
length(paraulesAuca)

# 3- Quantes paraules diferents hi ha?
paraulesAuca <- tolower(paraulesAuca)
uniqueParaulesAuca <- unique(paraulesAuca)
length(uniqueParaulesAuca)

# 4- Llista (sense repeticions) les paraules que tenen una c trencada?
uniqueParaulesAuca[grepl("ç",uniqueParaulesAuca)]

# 5- Quina és la paraula més freqüent?
tabParaulesAuca <- table(paraulesAuca)
tabParaulesAuca <- tabParaulesAuca[order(tabParaulesAuca,decreasing=TRUE)]
head(tabParaulesAuca)

# 6- Quina és la paraula més llarga que apareix a l'obra?
uniqueParaulesAuca[which.max(nchar(uniqueParaulesAuca))]

paraulesSenseSimbols <- uniqueParaulesAuca[!grepl("[·'-]",uniqueParaulesAuca)]
paraulesSenseSimbols[which.max(nchar(paraulesSenseSimbols))]


#### EXERCICIS 2 #### 
# Continuem treballant amb el text de l'Auca del Senyor Esteve...
# (fitxer "pg_AucaDelSenyorEsteve.txt")

# 7- Crea un núvol de paraules que mostrin les paraules més freqüents
paraulesSignificatives <- paraulesAuca[!(paraulesAuca %in% stopwords("catalan"))]

corpus_auca <- VCorpus(VectorSource(paste(paraulesSignificatives, collapse=" ")))
tdm_auca <- TermDocumentMatrix(corpus_auca)
frequent <- findMostFreqTerms(tdm_auca, n = 80)
wordcloud(names(frequent$`1`),frequent$`1`,rot.per=0,
          colors=brewer.pal(4,"Spectral"),
          random.order = FALSE)


# 8- Crea un núvol de paraules que mostrin les paraules més freqüents de
#    4 o més lletres

paraulesAucaMes3 <- paraulesSignificatives[nchar(paraulesSignificatives) > 3]
paraulesAucaMes3 <- paraulesAucaMes3[!(paraulesAucaMes3 %in% stopwords("catalan"))]

corpus_auca <- VCorpus(VectorSource(paste(paraulesAucaMes3, collapse=" ")))
tdm_auca <- TermDocumentMatrix(corpus_auca)
frequent <- findMostFreqTerms(tdm_auca, n = 80)
wordcloud(names(frequent$`1`),frequent$`1`,rot.per=0,
          colors=brewer.pal(4,"Spectral"),
          random.order = FALSE)

# 9- Crea un gràfic de xarxa que mostri les relacions entre les principals
#    paraules que apareixen en el text

paraules2 <- gsub("['-]"," ",paraulesAuca)
paraules2 <- gsub("·","",paraules2)
paraules2 <- paste(paraules2, collapse = " ")
paraules2 <- unlist(strsplit(paraules2, " "))
paraules2 <- paraules2[nchar(paraules2) > 3]

paraulesSignificatives <- paraules2[!(paraules2 %in% stopwords("catalan"))]

corpus_auca <- VCorpus(VectorSource(paste(paraulesSignificatives, collapse=" ")))
tdm_auca <- TermDocumentMatrix(corpus_auca, control=list(tokenize=NumbergramTokenizer))
frequent <- findMostFreqTerms(tdm_auca, n = 100)

l <- strsplit(names(frequent$`1`),split=" ")
df <- data.frame(matrix(unlist(l), ncol=2, byrow=T),
                 stringsAsFactors=FALSE)
colnames(df) <- c("from", "to")
relations <- data.frame(df,count=round(log(frequent$`1`)))
igraph <- graph_from_data_frame(relations)

ggraph(igraph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = count, edge_width = count),
                 edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position="none")

