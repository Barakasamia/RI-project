
## Installation des packages : 

install.packages("reutils")
install.packages("tm")  # pour le text mining
install.packages("SnowballC") # pour le text stemming
install.packages("wordcloud") # générateur de word-cloud 
install.packages("RColorBrewer") # Palettes de couleurs
library("tm")
library("SnowballC")
library(wordcloud)
library(RColorBrewer)
library(reutils)


search_funct <- function(terms) {
  # faire une recherche sur pubmed : 
  pmid <- esearch(terms, "pubmed" , retmax = 100 , retstart = 0 , sort= "relevance",usehistory = TRUE )
  return(pmid)
}

fetch_title <- function(pmid) {
    # récuperer le contneue des id renvoyés 
    articles <- efetch(pmid , retmax = 100 , retstart = 0)
   
     # récuper les valeurs des titres
    titles <- articles$xmlValue("//ArticleTitle")
    
    #List pour les titres : 
    title_list <- list(titles = c(titles))
    
    return(title_list)
}

fetch_abstract <- function(pmid) {
  # récuperer le contneue des id renvoyés 
  articles <- efetch(pmid , retmax = 100 , retstart = 0)
  
  # récuper les valeurs des résumés 
  
  abstracts <- articles$xmlValue("//AbstractText")
  
  #List pour les résumés : 
  abstract_list <- list(abstract= c(abstracts))
  
  return(abstract_list)
}

TAL_funct <- function(document) {
  
    # Charger les données comme un corpus
    docs <- Corpus(VectorSource(document))
    
    #Transformation : 
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    
    # Convertir le texte en minuscule
    docs <- tm_map(docs, content_transformer(tolower))
    # Supprimer les nombres
    docs <- tm_map(docs, removeNumbers)
    # Supprimer les mots vides anglais
    docs <- tm_map(docs, removeWords, stopwords("english"))
    
    # Supprimer les ponctuations
    docs <- tm_map(docs, removePunctuation)
    # Supprimer les espaces vides supplémentaires
    docs <- tm_map(docs, stripWhitespace)
    # Text stemming
    docs <- tm_map(docs, stemDocument)
     
    return(docs)
}

#Partie test appel à des fpnctions 

# Demander à l'utilisateur de tapez les terms de la requete 
print("entrer votre terms")
terms <- scan(what = "")
terms

pmid <- search_funct(terms)
title_list <- fetch_title(pmid)
abstr_list <- fetch_abstract(pmid)
docs <- TAL_funct(title_list)

#construir matrice term-doc
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
matrice_corr <- m %*% t(m)
#Vocabulaire (liste des mots) : 
dic <- data.frame(word = names(v))
View(dic)


#Visualisation de la matrice term-doc  
findFreqTerms(dtm, lowfreq = 4)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

# à ajouter ...
#transformé terms en list ( append) 
#ajouter une boucle 

