
#Library loading Mon Apr 19 12:38:23 2021----------

library(rvest)
library(dplyr)
library(stringr)
library(purrr)

#Webscrap Mon Apr 19 12:38:40 2021----------

#Get categories names
noms_cate <- read_html("https://www.1001cocktails.com/") %>%
  html_nodes("a") %>% 
  html_attr("href") %>%
  str_subset("https://www.1001cocktails.com/recettes/selection") %>% 
  unique() %>% 
  str_sub(50,-6)

# Get catégories links
liens_cate <- read_html("https://www.1001cocktails.com/") %>%
  html_nodes("a") %>% 
  html_attr("href") %>%
  str_subset("https://www.1001cocktails.com/recettes/selection") %>% 
  unique()


liens_tout_cate <- map(liens_cate, function(x) read_html(x) %>%
  html_nodes(".recipe-card-link") %>% 
  html_attr("href"))

#To data frame
my_cocktails <- data.frame()
for (i in seq_along(liens_cate)){
  my_cock <- data.frame(nom = noms_cate[i], 
                        lien = liens_tout_cate[[i]])
  my_cocktails <- rbind(my_cocktails, my_cock)}

my_cocktails$names <- str_sub(my_cocktails$lien, 48, -13 )

# Ingredient qauntity
qte <- map(my_cocktails$lien, function(x) read_html(x) %>%
      html_nodes(".recipe-ingredient-qt") %>% 
      html_text())
qte <- setNames(qte, my_cocktails$names)

#Ingredient
ingre <- map(my_cocktails$lien, function(x) read_html(x) %>%
                  html_nodes(".ingredient") %>% 
                  html_text())
ingre <- setNames(ingre, my_cocktails$names)


# Recipes
prep <- map(my_cocktails$lien, function(x) read_html(x) %>%
               html_nodes(".recipe-preparation__list__item") %>% 
               html_text()) 
prep <- setNames(prep, my_cocktails$names)

## pictures links

liens_img <- map(my_cocktails$lien, function(x) read_html(x) %>%
                         html_nodes("img") %>% 
                         html_attr("data-srcset"))
get_links <- liens_img %>% map(magrittr::extract2, 11)

get_links <- setNames(get_links, my_cocktails$names) %>% 
  as.data.frame() %>% t() %>% as.data.frame()

get_links$names <- rownames(get_links)

get_links$V1 <- str_extract(get_links$V1, "https.+jpg")

#Data frame with names, quantity, ingredients and recipes
full_cock <- data.frame()
for (i in seq_along(ingre)){
  full <- data.frame(names = my_cocktails$names[i], 
                     qty = qte[[i]],
                     ingre = ingre[[i]],
                     prep = paste(prep[[i]], collapse = " "),
                     Images = get_links$V1[i]
                     )
  full_cock <- rbind(full_cock, full)
  rm(full)}


my_cocktails_un <- my_cocktails %>% filter(!duplicated(my_cocktails$names))

#First data frame
final_cock <- left_join(full_cock, my_cocktails_un) %>% 
  select(6,1,2,3,4,5,-7) %>% rename(Groupe = nom, 
                        Cocktail = names, 
                        Ingédients = ingre,
                        Quantité = qty,
                        Recette = prep) %>% 
  tidyr::unite("Ingrédients", 3,4, sep = " ")

rm(list = c("full_cock", "i", "ingre", 
     "liens_cate", "liens_tout_cate", "my_cock", "my_cocktails", "my_cocktails_un", 
     "noms_cate", "prep", "qte"))

write.csv(final_cock, file = "cocktails.csv")


#Text mining Sat Dec 26 22:00:52 2020----------

# Installer
# install.packages("tm")  # pour le text mining
# install.packages("SnowballC") # pour le text stemming
# install.packages("wordcloud") # générateur de word-cloud 
# install.packages("RColorBrewer") # Palettes de couleurs

# Load packages

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

text <- final_cock$Ingrédients

# Load text data as corpus
docs <- Corpus(VectorSource(text))

# Replace “/”, “@” et “|” with a space
# 
# toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# docs <- tm_map(docs, toSpace, "/")
# docs <- tm_map(docs, toSpace, "@")
# docs <- tm_map(docs, toSpace, "\\|")


# Convert to lower
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove stopwords in english
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuation
docs <- tm_map(docs, removePunctuation)
# Remove whitespace
docs <- tm_map(docs, stripWhitespace)

# Transform similar words
content_rep <- content_transformer(function(x, pattern, replacement) 
  str_replace_all(x, pattern, replacement))

docs <- tm_map(docs, content_rep, "citrons", "citron")
docs <- tm_map(docs, content_rep, "doeuf", "oeufs")
docs <- tm_map(docs, content_rep, "duff", "oeufs")
docs <- tm_map(docs, content_rep, "canne", "sucre")
docs <- tm_map(docs, content_rep, "dananas", "ananas")
docs <- tm_map(docs, content_rep, "dorange", "orange")
docs <- tm_map(docs, content_rep, "oranges", "orange")


# Suppress unwanted words
docs <- tm_map(docs, removeWords, c("cl", "de", "g", "/", "jus",
                                    "verts", "vert", "verte", "pincée", "sec",
                                    "blanc", "fraîche",
                                    "trait", "sirop",
                                    "citron", "triple")) 


#Word cloud Sat Dec 26 22:11:30 2020----------
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=209, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
