## Initialize
library("tm")
library("plyr")
library("RcmdrPlugin.temis") ## ETC 5 minutes (installs lots of stuff)
options(stringsAsFactors = FALSE)

dir <- 'C:/Users/Grey/Desktop/Alex Work/Data/data_raw/Applications/Rankings'
ranks <- c("1.0", "1.5", "2.0", "2.5", "2.8", "3.1", "3.4", "3.5", "3.6", "3.7", "3.8", "3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5", "4.6", "4.8")
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

## Build TDM

generateTDM <- function(rank, path){
  fullpath <- sprintf("%s/%s", path, rank)
  s.cor <- Corpus(DirSource(directory=fullpath))
  corpus.cl<-cleanCorpus(s.cor)
  tdm <- TermDocumentMatrix(corpus.cl)
  
  s.tdm <- removeSparseTerms(tdm, 0.7)
  result <- list(ranking= rank, tdm= s.tdm)
}

tdm <- lapply(ranks, generateTDM, path=dir)

## Attach Ranks
bindRankingtoTDM <- function(tdm){
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringAsFactor=FALSE)
  
  s.df <- cbind(s.df, rep(tdm[["ranking"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "ranking"
  return(s.df)
}

rankings_tdm <- lapply (tdm, bindRankingtoTDM)

## Stack
tdm.stack <- do.call(rbind.fill, rankings_tdm)
tdm.stack[is.na(tdm.stack)] <- 0

## Testing and Training
## Currently selected 70% for training
train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack)*0.7))
test.idx <- (1:nrow(tdm.stack))[- train.idx]

## Model - KNN means
tdm.rank <- tdm.stack[, "ranking"]
tdm.stack.nl <- tdm.stack [, !colnames(tdm.stack) %in% "ranking"]

knn.pred <- knn(tdm.stack.nl[train.idx, ], tdm.stack.nl[test.idx, ], tdm.rank[train.idx])

## Accuracy
conf.mat <- table("Predictions" =knn.pred, Actual =tdm.rank[test.idx])
table("Predictions" =knn.pred, Actual =tdm.rank[test.idx])


