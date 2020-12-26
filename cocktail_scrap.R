##Get the page

stop("calm bro")

library(rvest)
library(dplyr)
library(stringr)
library(purrr)

#Noms des catégories
noms_cate <- read_html("https://www.1001cocktails.com/") %>%
  html_nodes("a") %>% 
  html_attr("href") %>%
  str_subset("https://www.1001cocktails.com/recettes/selection") %>% 
  unique() %>% 
  str_sub(50,-6)

#liens des catégories
liens_cate <- read_html("https://www.1001cocktails.com/") %>%
  html_nodes("a") %>% 
  html_attr("href") %>%
  str_subset("https://www.1001cocktails.com/recettes/selection") %>% 
  unique()


#Liens à l'intérieur de chaque catégorie
liens_tout_cate <- map(liens_cate, function(x) read_html(x) %>%
  html_nodes(".recipe-card-link") %>% 
  html_attr("href"))

#Mettre dans data frame
my_cocktails <- data.frame()
for (i in seq_along(liens_cate)){
  my_cock <- data.frame(nom = noms_cate[i], 
                        lien = liens_tout_cate[[i]])
  my_cocktails <- rbind(my_cocktails, my_cock)}

my_cocktails$names <- str_sub(my_cocktails$lien, 48, -13 )

#Quantité Ingrédient
qte <- map(my_cocktails$lien, function(x) read_html(x) %>%
      html_nodes(".recipe-ingredient-qt") %>% 
      html_text())
qte <- setNames(qte, my_cocktails$names)

#Ingrédient
ingre <- map(my_cocktails$lien, function(x) read_html(x) %>%
                  html_nodes(".ingredient") %>% 
                  html_text())
ingre <- setNames(ingre, my_cocktails$names)


#Préparation
prep <- map(my_cocktails$lien, function(x) read_html(x) %>%
               html_nodes(".recipe-preparation__list__item") %>% 
               html_text()) 
prep <- setNames(prep, my_cocktails$names)


#df avec noms, qté, ingrédients et mode de préparation
full_cock <- data.frame()
for (i in seq_along(ingre)){
  full <- data.frame(names = my_cocktails$names[i], 
                     qty = qte[[i]],
                     ingre = ingre[[i]],
                     prep = paste(prep[[i]], collapse = " ")
                     )
  full_cock <- rbind(full_cock, full)
  rm(full)}


my_cocktails_un <- my_cocktails %>% filter(!duplicated(my_cocktails$names))

#Ajout de la première base
final_cock <- left_join(full_cock, my_cocktails_un) %>% 
  select(5,1,2,3,4,-6) %>% rename(Groupe = nom, 
                        Cocktail = names, 
                        Ingédients = ingre,
                        Quantité = qty,
                        Recette = prep) %>% 
  tidyr::unite("Ingrédients", 3:4, sep = " ")

rm(list = c("full_cock", "i", "ingre", 
     "liens_cate", "liens_tout_cate", "my_cock", "my_cocktails", "my_cocktails_un", 
     "noms_cate", "prep", "qte"))



#Text mining Sat Dec 26 22:00:52 2020----------

# Installer
# install.packages("tm")  # pour le text mining
# install.packages("SnowballC") # pour le text stemming
# install.packages("wordcloud") # générateur de word-cloud 
# install.packages("RColorBrewer") # Palettes de couleurs
# Charger
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- final_cock$Ingrédients

# Charger les données comme un corpus
docs <- Corpus(VectorSource(text))

# Remplacer “/”, “@” et “|” avec un espace
# 
# toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# docs <- tm_map(docs, toSpace, "/")
# docs <- tm_map(docs, toSpace, "@")
# docs <- tm_map(docs, toSpace, "\\|")


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
# docs <- tm_map(docs, stemDocument)
#Transformer citron en citrons
content_rep <- content_transformer(function(x, pattern, replacement) 
  str_replace_all(x, pattern, replacement))

docs <- tm_map(docs, content_rep, "citrons", "citron")
docs <- tm_map(docs, content_rep, "doeuf", "oeufs")
docs <- tm_map(docs, content_rep, "duff", "oeufs")
docs <- tm_map(docs, content_rep, "canne", "sucre")
docs <- tm_map(docs, content_rep, "dananas", "ananas")
docs <- tm_map(docs, content_rep, "dorange", "orange")
docs <- tm_map(docs, content_rep, "oranges", "orange")



#Etape 4: Construire la matrice des mots Sat Dec 26 22:10:50 2020----------
# Supprimer votre propre liste de mots non désirés
docs <- tm_map(docs, removeWords, c("cl", "de", "g", "/", "jus",
                                    "verts", "vert", "verte", "pincée", "sec",
                                    "blanc", "fraîche",
                                    "trait", "sirop",
                                    "citron", "triple")) 

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Etape 5: Générer le nuage de mots Sat Dec 26 22:11:30 2020----------

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=209, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
