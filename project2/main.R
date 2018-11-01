library(tm)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)

train.mnb <- function (dtm,labels) 
{
  call <- match.call()
  V <- ncol(dtm)
  N <- nrow(dtm)
  prior <- table(labels)/N
  labelnames <- names(prior)
  nclass <- length(prior)
  cond.probs <- matrix(nrow=V,ncol=nclass)
  dimnames(cond.probs)[[1]] <- dimnames(dtm)[[2]]
  dimnames(cond.probs)[[2]] <- labelnames
  index <- list(length=nclass)
  for(j in 1:nclass){
    index[[j]] <- c(1:N)[labels == labelnames[j]]
  }
  
  for(i in 1:V){
    for(j in 1:nclass){
      cond.probs[i,j] <- (sum(dtm[index[[j]],i])+1)/(sum(dtm[index[[j]],])+V)
    }
  }
  list(call=call,prior=prior,cond.probs=cond.probs)    
}

predict.mnb <-
  function (model,dtm) 
  {
    classlabels <- dimnames(model$cond.probs)[[2]]
    logprobs <- dtm %*% log(model$cond.probs)
    N <- nrow(dtm)
    nclass <- ncol(model$cond.probs)
    logprobs <- logprobs+matrix(nrow=N,ncol=nclass,log(model$prior),byrow=T)
    classlabels[max.col(logprobs)]
  }

train.pos <- VCorpus(DirSource("./train/truthful_from_Web",
                                  encoding="UTF-8", recursive = TRUE))
train.neg <- VCorpus(DirSource("./train/deceptive_from_MTurk",
                                  encoding="UTF-8", recursive = TRUE))
# Join negative and positive train into a single Corpus
train.all <- c(train.pos,train.neg)
# create label vector (0=negative, 1=positive)
train.labels <- c(rep(1,320),rep(0,320))

train.all <- tm_map(train.all,removePunctuation)
# Make all letters lower case
train.all <- tm_map(train.all,content_transformer(tolower))
# Remove stopwords
train.all <- tm_map(train.all, removeWords,
                        stopwords("english"))
# Remove numbers
train.all <- tm_map(train.all,removeNumbers)
# Remove excess whitespace
train.all <- tm_map(train.all,stripWhitespace)

as.character(train.all[[1]])

terms <- DocumentTermMatrix(train.all,
                                 control=list(weighting=weightTf))
# remove sparse terms
terms <- removeSparseTerms(terms,0.95)


test.pos <- VCorpus(DirSource("./test/truthful_from_Web",
                              encoding="UTF-8", recursive = TRUE))
test.neg <- VCorpus(DirSource("./test/deceptive_from_MTurk",
                              encoding="UTF-8", recursive = TRUE))
# Join negative and positive train into a single Corpus
test.all <- c(test.pos,test.neg)
# create label vector (0=negative, 1=positive)
test.labels <- c(rep(1,80),rep(0,80))

test.all <- tm_map(test.all,removePunctuation)
# Make all letters lower case
test.all <- tm_map(test.all,content_transformer(tolower))
# Remove stopwords
test.all <- tm_map(test.all, removeWords,
                   stopwords("english"))
# Remove numbers
test.all <- tm_map(test.all,removeNumbers)
# Remove excess whitespace
test.all <- tm_map(test.all,stripWhitespace)


test.terms <- DocumentTermMatrix(test.all,
                                 control=list(dictionary = dimnames(terms)[[2]]))

# term frequencies on the test set
test3.dtm <- as.matrix(test.terms)

train.glmnet <- cv.glmnet(as.matrix(terms),train.labels,
                             family="binomial",type.measure="class")

train.logreg2.pred <- predict(train.glmnet,
                                newx=test3.dtm,s="lambda.min",type="class")
# show confusion matrix
log.res <- table(train.logreg2.pred,test.labels)

library(entropy)
# compute mutual information of each term with class label
train.mi <- apply(as.matrix(terms),2,
                    function(x,y){mi.plugin(table(x,y)/length(y))},train.labels)
# sort the indices from high to low mutual information
train.mi.order <- order(train.mi,decreasing=T)
# show the five terms with highest mutual information
train.mi[train.mi.order[1:5]]


reviews.mnb <- train.mnb(as.matrix(terms)[,train.mi.order[1:40]],train.labels)
# create document term matrix for test set
test.dtm <- DocumentTermMatrix(test.all,
                                 list(dictionary=dimnames(terms)[[2]]))
dim(test.dtm)
reviews.mnb.pred <- predict.mnb(reviews.mnb,as.matrix(test.dtm)[,train.mi.order[1:40]])
bay.res <- table(reviews.mnb.pred,test.labels)


reviews.rpart <- rpart(label~.,
                         data=data.frame(as.matrix(terms),label=train.labels),
                         cp=0,method="class")
# simple tree for plotting
reviews.rpart.pruned <- prune(reviews.rpart,cp=1.37e-02)
plot(reviews.rpart.pruned)
# tree with lowest cv error
reviews.rpart.pruned <- prune(reviews.rpart,cp=0.001)
# make predictions on the test set
reviews.rpart.pred <- predict(reviews.rpart.pruned,
                                newdata=data.frame(as.matrix(test.dtm)),type="class")
# show confusion matrix
tree.res <- table(reviews.rpart.pred,test.labels)

model.rf <- randomForest(as.factor(label)~.,
                         data=data.frame(as.matrix(terms),label=train.labels), importance = TRUE)

rf.predict <- predict(model.rf, newdata=data.frame(as.matrix(test.dtm)), type = "class")

rf.res <- table(rf.predict, test.labels)

result <- function(table){
  acc <- (table[1,1] + table[2,2])/sum(table)
  rec <- table[2,2] / (table[2,2] + table[2, 1])
  pre <- table[2,2] / (table[2,2] + table[1, 2])
  f1 <- 2 * ((rec * pre)/(rec + pre))
  print(acc)
  print(rec)
  print(pre)
  print(f1)
}

result(log.res)
result(bay.res)
