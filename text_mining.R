if (!require("stringi")) install.packages("stringi")
if (!require("stringr")) install.packages("stringr")
if (!require("qdap")) install.packages("qdap")
if (!require("tm")) install.packages("tm")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tokenizers")) install.packages("tokenizers")
if (!require("tidytext")) install.packages("tidytext")
if (!require("SnowballC")) install.packages("SnowballC")
if (!require("syuzhet")) install.packages("syuzhet")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("wordcloud")) install.packages("wordcloud")
if (!require("smacof")) install.packages("smacof")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("ggfortify")) install.packages("ggfortify")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("quanteda")) install.packages("quanteda")

restaurants <- read.csv("D:/Programowanie/R_projekty/Warsztaty/Restaurants.csv")

restaurants$X <- as.factor(restaurants$X)
restaurants$Restaurant <- as.factor(restaurants$Restaurant)
restaurants$Reviewer <- as.factor(restaurants$Reviewer)

df <- restaurants[!duplicated(restaurants$Review), ]
# Zadanie 1 ---------------------------------------------------------------


percent <- grepl('music | dance', df$Review, ignore.case=T)
sum(percent)/nrow(df) * 100
sprintf("slowo 'music' lub 'dance' wystepuje (przynajmniej raz) w %s procent recenzji", sum(percent)/nrow(df) * 100)


# Zadanie 2 ---------------------------------------------------------------

[+][Tt]ext *[Mm]ining[0-9]+
  

# Zadanie 3 ---------------------------------------------------------------
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

df$Review <- as.character(df$Review) %>%
  tryTolower() %>% 
  {gsub("[^[:alnum:][:blank:]!'*,-./:;?`]", "",.)} %>%
  {gsub("u2019", "'",.)} %>%
  {gsub("\\n", " ", .)} %>%
  {gsub("[?!]+",".", .)} %>%
  {gsub("[0-9]"," ", .)} %>%
  {gsub("-"," ", .)} %>%
  {gsub("\"+"," ", .)} %>%
  {gsub("\\.+","\\. ", .)} %>%
  {gsub(" +"," ", .)} %>%
  {gsub("\\. \\.","\\. ", .)}

stopwords_en <- as.data.frame(stopwords("en"))
names(stopwords_en)[1] <- "word"

j<-1
for (j in 1:nrow(df)) {
  temp_review <- anti_join(df[j,] %>% 
                             unnest_tokens(word, Review, drop=FALSE, to_lower=FALSE, token = "words"), stopwords_en)
  
  stemmed_review <- wordStem(temp_review[,"word"], language = "porter")
  
  df[j,"Review"] <- paste((stemmed_review), collapse = " ")}

cleaned_words <- df %>% unnest_tokens(word, Review, to_lower=FALSE, token = "words") 

cleaned_counts <- cleaned_words %>% count(word, sort=TRUE)

#najrzadze
infrequent <- cleaned_counts %>% filter(n<9)
j<-1 
for (j in 1:nrow(df)) {
  temp_review <- anti_join((df[j,] %>% 
                              unnest_tokens(word, Review, to_lower=FALSE, token = "words")), infrequent)
  
  df[j,"Review"] <- paste((temp_review[,"word"]),collapse = " ")
  
}
#najczęstsze
most_frequent <- cleaned_counts %>% filter(n > 6000)
j<-1 
for (j in 1:nrow(df)) {
  temp_review <- anti_join((df[j,] %>% 
                              unnest_tokens(word, Review, to_lower=FALSE, token = "words")), most_frequent)
  
  df[j,"Review"] <- paste((temp_review[,"word"]),collapse = " ")
  
}

#3 najczęstsze bigramy
bigramcount_final <- df %>%
  unnest_tokens(word, Review, to_lower=FALSE, token = "ngrams", n = 2) %>%
  count(word, sort=TRUE)
head(bigramcount_final, 3)


# Zadanie 4 ---------------------------------------------------------------

# 1 sposób
corpus1.1 <- corpus(df, docid_field = "X", text_field = "Review")
# 2 sposob 
df1.2 <- data.frame(doc_id = df$X, text = df$Review)
corpus1.2 <- VCorpus(DataframeSource(df1.2))

tdm1.2 <- TermDocumentMatrix(corpus1.2)
tdm1.2 <- as.matrix(tdm1.2)

dtm1.2 <- DocumentTermMatrix(corpus1.2)
dtm1.2 <- as.matrix(dtm1.2)


# Zadanie 5 ---------------------------------------------------------------
tdm1.2 <- TermDocumentMatrix(corpus1.2)

tast <- findAssocs(tdm1.2, 'tast', 0.2)

tast <- as.data.frame(tast)
tast$terms <- row.names(tast)

tast$terms <- factor(tast$terms,
                       levels=tast$terms)

ggplot(tast, aes(y=terms)) +
  geom_point(aes(x=tast), data=tast,size=2)+
  theme_gdocs()+ 
  geom_text(aes(x=tast,label=tast),colour="darkred",hjust=-.25,size=4)+
  theme(text=element_text(size=15),axis.title.y=element_blank())
#wykres pomaga znaleźć te słowa, z którymi dane słowo powiązane jest w większym stopniu (korelacja tych dwóch słów)
#im liczba jest większa, tym powiązananie jest silniejsze (częstsze w danej próbie)
#z wykresu można odczytać że słowo "tast"(-y) czyli smaczny najczęściej pojawia się wraz ze słowem "chicken"
#dość często kommenatory odznaczali pyszność dań - "dish" i porównówali smak z czymś innym - "like"

# Zadanie 6 ---------------------------------------------------------------

co_occurrence_matrix2 <- fcm(x = corpus1.1, context = "window", window=2, count = "frequency", tri=FALSE)

dfm <- dfm(corpus1.1)
counts <- colSums(as.matrix(dfm)) 

co_occurrence_matrix2 <- as.matrix(co_occurrence_matrix2)
diag(co_occurrence_matrix2) <- counts

sortedcounts <- counts %>% sort(decreasing=TRUE)
sortednames <- names(sortedcounts)
nwords <- 500

co_occurrence_matrix2 <- co_occurrence_matrix2[sortednames[1:nwords], sortednames[1:nwords]]
co_occurrence_matrix2[1:10,1:10]

distances <- sim2diss(co_occurrence_matrix2, method = "cooccurrence")
distances[1:10,1:10]

MDS_map <- smacofSym(distances)

ggplot(as.data.frame(MDS_map$conf), aes(D1, D2, label = rownames(MDS_map$conf))) +
  geom_text(check_overlap = TRUE) + theme_minimal(base_size = 15) + xlab('') + ylab('') +
  scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)
#lewa strona - sandwich, pork, mango, sweet, super, pasta, pizza, chicken, delicious, tasty, rice - recenzje dań
#prawy górny róg - hangout, dance, light, night, date, location, beer, music, friend, family - recenzje a propos atmosfery
#prawa środek - pozytywne: staff, team, quick, hospitable, help, friendly - serwis
#prawa dół - negatywne: wait, hour, rude, slow, delivery, long, poor - serwis
