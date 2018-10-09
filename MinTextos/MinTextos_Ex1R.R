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

# 4- Quina és la paraula més freqüent?
tabParaulesAuca <- table(paraulesAuca)
tabParaulesAuca <- tabParaulesAuca[order(tabParaulesAuca,decreasing=TRUE)]
head(tabParaulesAuca)

# 5- Quina és la paraula més llarga que apareix a l'obra?
uniqueParaulesAuca[which.max(nchar(uniqueParaulesAuca))]

# 6- Crea un núvol de paraules que mostrin les paraules més freqüents de
#    4 o més lletres
paraulesAucaMes3 <- paraulesAuca[nchar(paraulesAuca) > 3]
paraulesAucaMes3 <- paraulesAucaMes3[!(paraulesAucaMes3 %in% stopwords("catalan"))]

corpus_auca <- VCorpus(VectorSource(paste(paraulesAucaMes3, collapse=" ")))
tdm_auca <- TermDocumentMatrix(corpus_auca)
frequent <- findMostFreqTerms(tdm_auca, n = 80)
wordcloud(names(frequent$`1`),frequent$`1`,rot.per=0,
          colors=brewer.pal(4,"Spectral"),
          random.order = FALSE)
