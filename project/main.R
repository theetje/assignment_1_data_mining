source("helpers.R")

e.2.0.data <- read.csv("./eclipse-metrics-packages-2.0.csv")
e.3.0.data <- read.csv("./eclipse-metrics-packages-3.0.csv")

e.2.0.data <- e.2.0.data[1:42]
e.3.0.data <- e.3.0.data[1:42]

e.2.0.data$post[e.2.0.data$post > 0] <- 1
e.3.0.data$post[e.3.0.data$post > 0] <- 1

e.2.0.class <- e.2.0.data$post
e.3.0.class <- e.3.0.data$post

e.2.0.data$post <- NULL
e.3.0.data$post <- NULL

pima <- read.csv("./pima.csv")
pimaData <- pima[1:8]
pimaClass <- pima[, 9]

root <-
  setRefClass("root", fields = list(left = "ANY", right = "ANY"))
node <-
  setRefClass(
    "node",
    fields = list(
      left = "ANY",
      right = "ANY",
      attribute = "ANY",
      value = "ANY"
    ),
    methods = list(
      classify = function(x) {
        if (x[attribute] > value)
          return(left$classify(x))
        else
          return(right$classify(x))
      }
    )
  )
leaf <-
  setRefClass(
    "leaf",
    fields = list(data_set = "ANY"),
    methods = list(
      classify = function(x) {
        return(data_set)
      }
    )
  )

tree.grow <- function(x, y, nmin, minleaf, nfeat) {
  impurityValue <- impurity(y)
  if (impurityValue == 0 || length(y) < nmin) {
    return(leaf(data_set = as.integer(names(
      which.max(table(y))
    ))))
  }
  attributeName <- NULL
  attribute <- NULL
  attributeValue <- NULL
  names <- colnames(x)
  predictors <- sample(names, nfeat)
  for (a in predictors) {
    uni <- unique(x[, a])
    if (length(uni) > 1) {
      s <- bestsplit(x[[a]], y, uni, impurityValue, minleaf)
      if (s[2] < impurityValue && s[3] >= minleaf) {
        impurityValue <- s[2]
        attribute <- x[, a]
        attributeName <- a
        attributeValue <- s[1]
      }
    }
  }
  if (!is.null(attribute))
  {
    return(
      node(
        attribute = attributeName,
        value = attributeValue,
        left = tree.grow(x[attribute >= attributeValue,], y[attribute >= attributeValue], nmin, minleaf, nfeat),
        right = tree.grow(x[attribute < attributeValue,], y[attribute < attributeValue], nmin, minleaf, nfeat)
      )
    )
  }
  return(leaf(data_set = as.integer(names(
    which.max(table(y))
  ))))
}

tree.classify <- function(x, tr) {
  tr$classify(x)
}

tree.grow.bag <- function(x, y, nmin, minleaf, nfeat, m) {
  trees <- c()
  for (i in 1:m) {
    samples <- sample(nrow(x), nrow(x), TRUE)
    trees <-
      append(trees, tree.grow(x[samples, ], y[samples], nmin, minleaf, nfeat))
  }
  return(trees)
}

tree.classify.bag <- function(x, tr) {
  classifications <- c()
  for (d in 1:nrow(x)) {
    guesses <- c()
    for (t in tr) {
      guesses <- append(guesses, t$classify(x[d,]))
    }
    classifications <-
      append(classifications, as.integer(names(which.max(table(
        guesses
      )))))
  }
  return(classifications)
}

#tree <- tree.grow(e.2.0.data, e.2.0.class, 15, 15, 41)
# print(tree)
#print(tree$classify(e.3.0.data[180,]))

tree <- tree.grow(pimaData, pimaClass, 20, 5, 8)
pred <- tree.classify.bag(pimaData, c(tree))
print("Pima")
print(table(pred, pimaClass))
#tree <- tree.grow(e.2.0.data, e.2.0.class, 15, 15, 41)

#print(tree$classify(e.3.0.data[180,]))
#pred <- tree.classify.bag(e.3.0.data, c(tree))
#print(table(pred, e.3.0.class))
#trees <- tree.grow.bag(e.2.0.data, e.2.0.class, 15, 15, 41, 4)
#preds <- tree.classify.bag(e.3.0.data, trees)
#print(table(preds, e.3.0.class))
#trees <- tree.grow.bag(e.2.0.data, e.2.0.class, 15, 15, 41, 100)
#preds <- tree.classify.bag(e.3.0.data, trees)
#print(table(preds, e.3.0.class))
# print(tree.classify.bag(data, c(tree))) 	# print(tree.classify.bag(data, c(tree)))

#trees <- tree.grow.bag(e.2.0.data, e.2.0.class, 15, 15, 6, 100)
#preds <- tree.classify.bag(e.3.0.data, trees)
#print(table(preds, e.3.0.class))

# print(tree.classify.bag(data, c(tree)))