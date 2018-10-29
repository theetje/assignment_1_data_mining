library(tm)
library(glmnet)
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
                                 control=list(weighting=weightTfIdf))
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

as.character(test.all[[1]])

test.terms <- DocumentTermMatrix(test.all,
                                 control=list(dictionary = dimnames(terms)[[2]]))

train.glmnet <- cv.glmnet(as.matrix(terms),train.labels,
                             family="binomial",type.measure="class")

train3.dtm <- as.matrix(terms)
# convert term frequency counts to binary indicator
train3.dtm <- matrix(as.numeric(train3.dtm > 0),nrow=640,ncol=307)
# sum the columns of the training set
train3.idf <- apply(train3.dtm,2,sum)
# compute idf for each term (column)
train3.idf <- log2(640/train3.idf)
# term frequencies on the test set
test3.dtm <- as.matrix(test.terms)
# compute tf-idf weights on the test set
for(i in 1:307){test3.dtm[,i] <- test3.dtm[,i]*train3.idf[i]}

train.logreg2.pred <- predict(train.glmnet,
                                newx=test3.dtm,s="lambda.1se",type="class")
# show confusion matrix
table(train.logreg2.pred,test.labels)

