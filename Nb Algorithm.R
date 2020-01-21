## Initialize
library("tm")
library("plyr")
library("e1071")
library("tidyverse")
library("RcmdrPlugin.temis") ## ETC 5 minutes (installs lots of stuff)
options(stringsAsFactors = FALSE)

dir <- 'C:/Users/Grey/Desktop/Alex Work/Data/data_raw/Applications/Rankings'
ranks <- c("10", "15", "20", "25", "28", "31", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "48")
corpus_all<-Corpus(DirSource(directory = 'C:/Users/Grey/Desktop/Alex Work/Data/data_raw/Applications/_All'))

## Clean Text
custom_stop <- c("the","and")

cleanCorpus <- function(corpus) {
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("English"))
  corpus.tmp <- tm_map(corpus.tmp, removeWords, custom_stop)
  corpus.tmp <- tm_map(corpus.tmp, stemDocument)
  return(corpus.tmp)
}

## Build DTM

generateDTM <- function(rank, path){
  fullpath <- sprintf("%s/%s", path, rank)
  s.cor <- Corpus(DirSource(directory=fullpath))
  corpus.cl<-cleanCorpus(s.cor)
  dtm <- DocumentTermMatrix(corpus.cl)
  
  s.dtm <- removeSparseTerms(dtm, 0.7)
  result <- list(ranking= rank, dtm= s.dtm)
}

dtm <- lapply(ranks, generateDTM, path=dir)

## Attach Ranking
bindRankingtoDTM <- function(dtm){
  s.mat <- data.matrix(dtm[["dtm"]])
  s.df <- as.data.frame(s.mat, stringAsFactor=FALSE)
  
  s.df <- cbind(s.df, rep(dtm[["ranking"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "ranking"
  return(s.df)
}

rankings_dtm <- lapply (dtm, bindRankingtoDTM)

## Stack Data
dtm.stack <- do.call(rbind.fill, rankings_dtm)
dtm.stack[is.na(dtm.stack)] <- 0

## Extract ranking data
only_ranking <- as.data.frame(dtm.stack[,"ranking"])

## Find Frequent Words
fw_dtm <- DocumentTermMatrix(corpus_all)
frequent_words <- as.factor(findFreqTerms(fw_dtm, 10))

## DTM for frequent words
freq_words_all <- dtm.stack[, frequent_words]

## Function to label presence of a word

binary_YN <- function(x) {
  y <- ifelse(x>0, 1, 0)
  y <- factor(y, levels=c(0,1), labels= c("No", "Yes"))
  y
}

## columnNamer ensures that the data is in a format for which Naives Bayes can be run on
columnNamer <- function (s.df) {
  colnames(s.df)[ncol(s.df)] <- as.factor(colnames(s.df)[ncol(s.df)])
  return(s.df)
}

## Apply binary_YN and colNamer functions to data
all.label <- apply(freq_words_all, 2, binary_YN)
all_labels <- cbind(only_ranking, all.label)
colnames(all_labels)[1] <- "ranking"

factored <- as.data.frame(lapply(all_labels, columnNamer))

## Testing and Training indexes
## Currently selected 70% for training
train.idx <- sample(nrow(factored), ceiling(nrow(factored)*0.7))
test.idx <- (1:nrow(factored))[- train.idx]

train.label <- factored[train.idx,]
test.label <- factored[test.idx,]

## Model - Naive Bayes
app_classifier <- naiveBayes(ranking~., data=train.label)

app_pred <- predict(app_classifier, newdata=test.label, type="raw")

## Result
View(app_pred)

