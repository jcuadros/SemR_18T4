##=========================================================================##
##                                                                         ##
##  Introdució a la mineria de textos amb R                                ##
##  @authors: Jordi Cuadros, Francesc Martori, Vanessa Serrano             ##
##                                                                         ##
##=========================================================================##

#### VARIABLES I FUNCIONS BÀSIQUES DE TEXT ####
# El text s'emmagatzemma en variables tipus caracter
text1 <- "Benvinguts al seminari d'introducció a la mineria de textos amb R"
class(text1)

# Si la informació textual està codificada com un factor cal convertir-la a
# caracters, usant la funció as.character()

## Determinar el nombre de caràcters ####
nchar(text1)

## Unir textos ####
vec1 <- c("Espero", "que", "us", "resulti", "molt", "profitós")
paste(paste(vec1, collapse=" "),"!", sep="")

## Fragmentar un text ####
strsplit(text1, " ")
paraules <- unlist(strsplit(text1, "[ ']"))
nchar(paraules)
longs <- nchar(paraules)
paraules[which.max(longs)]
lletres <- unlist(strsplit(paraules, ""))
tabLletres <- table(lletres)
tabLletres <- tabLletres[order(-tabLletres, names(tabLletres))]
tabLletres

## Extraure una subcadena ####
substr(text1, 1, 1)
substr(text1, 2, 2)
# Fixeu-vos que el tercer argument és posició final

substr(paraules, 2, 2)

## Canviar la mida de caixa ####
tolower(paraules)
toupper(paraules)

## Comparar cadenes ####
"a" == "A"
"a" == "a"
"a" == "a "
c("a") == c("a", "A")
letters[order(letters, decreasing = T)]
letters2 <- c(letters, toupper(letters), letters)
letters2[order(letters2, decreasing = T)]  # order uses lexicographical order

## Entendre la codificació de caràcters ####
Sys.getlocale()
l10n_info()

strtoi(charToRaw("a"),16)
rawToChar(as.raw(97))

Encoding(text1)
enc2utf8(text1)
text1utf <- enc2utf8(text1)
Encoding(text1utf)

unlist(strsplit(text1,""))
c(sapply(text1,charToRaw))
c(sapply(text1utf,charToRaw))

nchar(text1)
nchar(text1utf)
nchar(text1, type = "bytes")
nchar(text1utf, type = "bytes")

rawToChar(as.raw(0xf3))
rawToChar(as.raw(c(0xb3,0xc3)))

text1mal <- text1utf
Encoding(text1mal) <- "latin1"
text1mal

iconv(text1mal, from="UTF-8", to="latin1")

# Caràcters especials en les cadenes de text en R
# Salt de línia: \r\n (Windows), \n (Linux, UNIX)
text3 <- "hola\tadéu\r\n\xfe\t\"\t\\"
cat(text3)
Encoding(text3)

c(sapply(text3,charToRaw))

Encoding("hola")


#### INTRODUCCIÓ A LES EXPRESSIONS REGULARS ####
## Cerca i substitució a R ####
text1 <- "Benvinguts al seminari d'introducció a la mineria de textos amb R"
paraules <- unlist(strsplit(text1,split="[ ,.]"))
paraules

gsub("a", "_", text1, fixed=TRUE)
gsub("a", "_", paraules, fixed=TRUE)

grepl("amb", paraules, fixed=TRUE)    # grep és semblant
paraules == "amb"
grep("amb", paraules, fixed=TRUE)    

regexec("amb", text1, fixed=TRUE)     # regexpr i gregexpr són semblants
regexec("a", text1, fixed=TRUE)
gregexpr("a", text1, fixed=TRUE)

trobat <- regexec("amb", text1, fixed=TRUE)
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

trobat <- regexec("t", paraules, fixed=TRUE)
trobat
trobat <- sapply(trobat, getElement, 1) != -1
paraules[trobat]

## Expressions regulars ####
# Les expressions regulars permeten anar més enllà de les cerques de
# seqüències exactes i cercar a partir de patrons.

# - Expressions alternatives
regex <- "a|e"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

regex <- "[A-Z]"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

# - Diferent de
regex <- "[^ae]"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

# - Comodins
regex <- "i.e"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

# - Multiplicadors
regex <- "i.*e"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

regex <- "i.*?e"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

regex <- "e..+i"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

regex <- "i.?a"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

regex <- "e.{2,3}i"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

# - Ancoratges a text
regex <- "^.e"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

regex <- "i.*$"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

# - Ancoratges a paraula
regex <- "\\<.e"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

regex <- "i\\S*\\>"
gsub(regex, "_", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)

# - Grups i captures
regex <- "(i[aàeéèoóò])"
gsub(regex, "[\\1]", text1)
paraules[grepl(regex,paraules)]
trobat <- regexec(regex, text1)
trobat
substr(text1, trobat[[1]], trobat[[1]] + attr(trobat[[1]], "match.length") - 1)


#### EXERCICIS 1 #### 
# Treballarem amb el text de l'Auca del Senyor Esteve...
# Està en el fitxer "pg_AucaDelSenyorEsteve.txt"

# 1- Carrega el fitxer de text i ajusta'l de forma que només hi hagi
# el contingut de l'obra en una variable de text.

# 2- Quantes paraules hi ha a l'obra?

# 3- Quantes paraules diferents hi ha?

# 4- Llista (sense repeticions) les paraules que tenen una c trencada?

# 5- Quina és la paraula més freqüent?

# 6- Quina és la paraula més llarga que apareix a l'obra?


#### TÈCNIQUES BASADES EN LA BOSSA DE PARAULES ####
if (!require("tm")) {
  install.packages("tm")
  library("tm")
}
if (!require("wordcloud")) {
  install.packages("wordcloud")
  library("wordcloud")
}

## Crear la bossa de paraules ####
tirant <- readLines("pg_TheWhiteKnight.txt")
str(tirant)

Encoding(tirant) <- "UTF-8"
tirant <- tirant[413:18286]

tirant <- paste(tirant, collapse=" ")

corpus_tirant <- VCorpus(VectorSource(tirant))
tdm_tirant <- TermDocumentMatrix(corpus_tirant)

frequent <- findMostFreqTerms(tdm_tirant, n = 100)
frequent

## Representar un núvol de paraules ####
wordcloud(names(frequent$`1`),frequent$`1`)

## Precondicionar els documents ####
tirant2 <- tirant
tirant2 <- tolower(tirant2)
tirant2 <- gsub("[^[:print:]]"," ", tirant2)
tirant2 <- gsub("[[:punct:]]"," ", tirant2)
tirant2 <- gsub("\\<\\w{0,3}\\>"," ", tirant2)
tirant2 <- gsub("\\s{2,}"," ", tirant2)

tirant2 <- removeWords(tirant2, gsub("[[:punct:]]"," ", stopwords("SMART")))

corpus_tirant <- VCorpus(VectorSource(tirant2))
tdm_tirant <- TermDocumentMatrix(corpus_tirant)
frequent <- findMostFreqTerms(tdm_tirant, n = 100)
wordcloud(names(frequent$`1`),frequent$`1`,rot.per=0,
          colors=brewer.pal(4,"Spectral"),random.order = FALSE)

## Usar multigrames ####
if (!require("RWeka")) {
  install.packages("RWeka")
  library("RWeka")
}
NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
NumbergramTokenizer("hola, bon dia tingueu.")

corpus_tirant <- VCorpus(VectorSource(tirant2))
tdm_tirant <- TermDocumentMatrix(corpus_tirant, control=list(tokenize=NumbergramTokenizer))
frequent <- findMostFreqTerms(tdm_tirant, n = 100)
wordcloud(names(frequent$`1`),frequent$`1`,rot.per=0,
          colors=brewer.pal(4,"Spectral"),
          scale=c(2,.5),random.order = FALSE)

## Treballar amb diversos documents ####
tirant3 <- unlist(strsplit(tirant2, "chapter"))
tirant3 <- tirant3[2:length(tirant3)]

corpus_tirant <- VCorpus(VectorSource(tirant3))
tdm_tirant <- TermDocumentMatrix(corpus_tirant)
frequent <- findMostFreqTerms(tdm_tirant, n = 100)
wordcloud(names(frequent$`1`),frequent$`1`,rot.per=0,
          colors=brewer.pal(4,"Spectral"),random.order = FALSE)

corpus_tirant <- VCorpus(VectorSource(tirant3))
tdm_tirant <- TermDocumentMatrix(corpus_tirant, 
                                 control= list(weighting = weightTfIdf))
frequent <- findMostFreqTerms(tdm_tirant, n = 100)
wordcloud(names(frequent$`1`),frequent$`1`,rot.per=0,
          colors=brewer.pal(4,"Spectral"),random.order = FALSE)

commonality.cloud(as.matrix(tdm_tirant[,c(1,15)]), rot.per=0, max.word=100,
                  colors=brewer.pal(4,"Spectral"),random.order = FALSE)
comparison.cloud(as.matrix(tdm_tirant[,c(1,15)]), rot.per=0, max.word=200,
                 scale = c(2,.5), title.size = 2, 
                 colors=brewer.pal(4,"Spectral"),random.order = FALSE)

## Determinar relacions entre termes ####
corpus_tirant <- VCorpus(VectorSource(tirant2))
tdm_tirant <- TermDocumentMatrix(corpus_tirant, control=list(tokenize=NumbergramTokenizer))
frequent <- findMostFreqTerms(tdm_tirant, n = 100)

if (!require("igraph")) {
  install.packages("igraph")
  library("igraph")
}
if (!require("ggraph")) {
  install.packages("ggraph")
  library("ggraph")
}

l <- strsplit(names(frequent$`1`),split=" ")
df <- data.frame(matrix(unlist(l), ncol=2, byrow=T),
                 stringsAsFactors=FALSE)
colnames(df) <- c("from", "to")
relations <- data.frame(df,count=frequent$`1`)
igraph <- graph_from_data_frame(relations)

ggraph(igraph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = count, edge_width = count),
                 edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position="none")

## Modelar temes (Topic modelling) ####
if (!require("topicmodels")) {
  install.packages("topicmodels")
  library("topicmodels")
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library("tidyverse")
}

data(AssociatedPress)
? "AssociatedPress"
str(AssociatedPress)

ap_lda <- LDA(AssociatedPress, k = 4)
str(ap_lda)

betas <- as.data.frame(t(ap_lda@beta))
colnames(betas) <- 1:ncol(betas)
betas$term <- ap_lda@terms

ap_topics <- betas %>% gather("topic", "beta", -term, factor_key=TRUE)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = paste(strrep(" ",as.numeric(topic) - 1),term,sep="")) %>%
  mutate(beta = 10^beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip()

# Hi ha altres tècniques que permeten agrupar bosses de paraules, 
# per exemple skmeans.


#### FUNCIONS DE PROCESSAMENT DE LLENGUATGE NATURAL ####
## Identificació d'idioma ####
barc <- readLines("barcelona.txt", encoding = "UTF-8")

if (!require("textcat")) {
  install.packages("textcat")
  library("textcat")
}
textcat(barc)

if (!require("cld2")) {
  install.packages("cld2")
  library("cld2")
}
detect_language(barc)
lapply(barc,detect_language_mixed)

## Identificació de classes gramaticals ####
if (!require("openNLP")) {
  install.packages("openNLP")
  library("openNLP")
}
frase1 <- barc[1]
frase1
frase1 <- gsub("[(].*?[)]","",frase1)
frase1 <- gsub("[[].*?[]]","",frase1)
frase1 <- gsub("\\s{2,}"," ",frase1)
frase1

frase1 <- as.String(frase1)
anSent <- NLP::annotate(frase1,Maxent_Sent_Token_Annotator())
anSent
frase1[anSent]

anWord <- NLP::annotate(frase1,Maxent_Word_Token_Annotator(),anSent)
anWord
frase1[anWord]

anPOS <- NLP::annotate(frase1,Maxent_POS_Tag_Annotator(),anWord)
anPOSword <- anPOS[anPOS$type=="word"]

tags <- sapply(anPOSword$features, `[[`, "POS")
tags
table(tags)
paste(frase1[anPOSword],"/",tags,sep="")
frase1[anPOSword][tags=="NN"]
frase1[anPOSword][grepl("NN",tags)]

## Lematització ####
ex1 <- c("tree", "trial", "tried", "try", "trees", "be", "is", "are",
         "run", "ran", "running")

if (!require("tm")) {
  install.packages("tm")
  library("tm")
}
stemDocument(ex1)
stemCompletion(stemDocument(ex1),dictionary = ex1)

if (!require("SnowballC")) {
  install.packages("SnowballC")
  library("SnowballC")
}
wordStem(ex1)

if (!require("textstem")) {
  install.packages("textstem")
  library("textstem")
}
stem_words(ex1)
lemmatize_words(ex1)

# TreeTagger és una altra opció que pot ser d'interès
# - http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
# - https://stackoverflow.com/questions/28214148/how-to-perform-lemmatization-in-r


## Distancia d'edició ####
if (!require("stringdist")) {
  install.packages("stringdist")
  library("stringdist")
}

stringdist("ca","abc")
ex1[stringdist("treed",ex1)==
      min(stringdist("treed",ex1))]


## Usar diccionaris ####
if (!require("hunspell")) {
  install.packages("hunspell")
  library("hunspell")
}

words <- c("beer", "wiskey", "wine")
hunspell_check(words)

# El dictionari català es troba a 
# https://cgit.freedesktop.org/libreoffice/dictionaries/tree/
paraules <- c("cervesa", "bi", "caba")
hunspell_check(paraules, dict=dictionary("ca.dic"))
hunspell_suggest(paraules, dict=dictionary("ca.dic"))


#### EXERCICIS 2 #### 
# Continuem treballant amb el text de l'Auca del Senyor Esteve...
# (fitxer "pg_AucaDelSenyorEsteve.txt")

# 7- Crea un núvol de paraules que mostrin les paraules més freqüents de
#    4 o més lletres

# 8- Crea un núvol de paraules que mostrin les paraules més freqüents de
#    4 o més lletres

# 9- Crea un gràfic de xarxa que mostri les relacions entre les principals
#    paraules que apareixen en el text


#### REFERÈNCIES ####
# http://biostat.mc.vanderbilt.edu/wiki/pub/Main/RegularExpressionPrimer/regex-primer.pdf
# https://www.regular-expressions.info/tutorial.html
# https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
# https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# https://cran.r-project.org/web/views/NaturalLanguageProcessing.html
# https://www.tidytextmining.com/
# https://cran.r-project.org/web/packages/hunspell/vignettes/intro.html


