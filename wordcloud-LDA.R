install.packages("tm") 
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("topicmodels")

library(tm)
library(wordcloud)
library(topicmodels)

voc.all <- read.csv("voc-comments.csv", header = TRUE, sep = ",")

voc <- Corpus(VectorSource(voc.all$Comment.Text))
inspect(voc)

### wordcloud function
wordcloud.fun <- function(df){
  tdm <- TermDocumentMatrix(df,
                            control = list(removePunctuation = TRUE,
                                           stopwords = stopwords("english"), 
                                           removeNumbers = TRUE, 
                                           tolower = TRUE))
  tdm.matrix <- as.matrix(tdm)
  
  # get word counts in decreasing order
  word_freqs <- sort(rowSums(tdm.matrix), decreasing = TRUE) 
  # create a data frame with words and their frequencies
  tdm.df <- data.frame( word = names(word_freqs), freq = word_freqs)
  
  wordcloud(tdm.df$word, tdm.df$freq, random.order=FALSE, max.words = 100, colors=brewer.pal(8, "Dark2"))
}

wordcloud.fun(voc)

### LDA topics modelling function
dtm.control <- list(tolower = TRUE, 
                   removePunctuation = TRUE, 
                   removeNumbers = TRUE, 
                   stopwords = c(stopwords("english")), 
                   stemming = FALSE, 
                   wordLengths = c(3, Inf), 
                   weighting = weightTf)

topic <- function(df) {
  dtm <- DocumentTermMatrix(df, control = dtm.control)
  dtm <- removeSparseTerms(dtm, 0.999)
  dtm <- dtm[rowSums(as.matrix(dtm))>0, ]
  
  k <- 8
  
  lda.model <- LDA(dtm, k)
  
  x <- terms(lda.model,10)
  print(x)
}

voc.topics <- topic(voc)

write.csv(voc.topics, "voc.topics.csv")

##topics comparison between Jul and Sep
voc.jul <- voc.all[voc.all$Month.Date == "Jul", "Comment.Text"]
voc.sep <- voc.all[voc.all$Month.Date == "Sep", "Comment.Text"]

voc.jul <- Corpus(VectorSource(voc.jul))
voc.sep <- Corpus(VectorSource(voc.sep))

jul <- topic(voc.jul)
sep <- topic(voc.sep)

par(mfrow = c(1, 2))
wordcloud.fun(voc.jul)
wordcloud.fun(voc.sep)
par(mfrow = c(1, 1))

write.csv(jul, "jul.topics.csv")
write.csv(sep, "sep.topics.csv")


##examining comments of OSAT <= 2
voc.osat2 <- voc.all[voc.all$OSAT.num <= 2, "Comment.Text"]

voc.osat2 <- Corpus(VectorSource(voc.osat2))

par(mfrow = c(1, 2))

wordcloud.fun(voc.osat2)

osat2 <- topic(voc.osat2)

write.csv(osat2, "osat2.topics.csv")

##examining comments of OSAT >= 4
voc.osat4 <- voc.all[voc.all$OSAT.num >= 4, "Comment.Text"]

voc.osat4 <- Corpus(VectorSource(voc.osat4))

wordcloud.fun(voc.osat4)
par(mfrow = c(1, 1))

osat4 <- topic(voc.osat4)

write.csv(osat4, "osat4.topics.csv")

##correlation between OSAT and Treated like a Guest
voc.cor <- read.csv("cor.csv")
cor(voc.cor)
