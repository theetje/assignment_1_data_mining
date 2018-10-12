source("helpers.R")

e.2.0.data <- read.csv("./eclipse-metrics-packages-2.0.csv")
e.3.0.data <- read.csv("./eclipse-metrics-packages-3.0.csv")

e.2.0.data$post[e.2.0.data$post > 0] <- 1
e.3.0.data$post[e.3.0.data$post > 0] <- 1

e.2.0.class <- e.2.0.data$post
e.3.0.class <- e.3.0.data$post

e.2.0.data$post <- NULL
e.3.0.data$post <- NULL


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
  if (impurity(y) == 0 || length(y) < nmin) {
    return(leaf(data_set = as.integer(names(
      which.max(table(y))
    ))))
  }
  impurityValue <- impurity(y)
  attributeName <- NULL
  attribute <- NULL
  attributeValue <- NULL
  names <- colnames(x)
  predictors <- sample(names, nfeat)
  for (a in predictors) {
    if (length(unique(x[[a]])) > 1) {
      s <- bestsplit(x[[a]], y)
      if (s[2] < impurityValue && s[3] >= minleaf) {
        impurityValue <- s[2]
        attribute <- x[[a]]
        attributeName <- a
        attributeValue <- s[1]
      }
    }
  }
  if (!is.null(attribute))
  {
    df <- data.frame(x, value = y)
    s.star <- split(df, attribute < attributeValue)
    return(
      node(
        attribute = attributeName,
        value = attributeValue,
        left = tree.grow(s.star$F[1], s.star$F$value, nmin, minleaf, nfeat),
        right = tree.grow(s.star$T[1], s.star$T$value, nmin, minleaf, nfeat)
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
  for (i in 1:m){
    samples <- sample(nrow(x), length(x), TRUE)
    trees <- append(trees, tree.grow(x[samples,], y[samples,], nmin, minleaf, nfeat))
  }
}

tree.classify.bag <- function(x, tr){
  classifications <- c()
  for(d in 1:nrow(x)) {
    guesses <- c()
    for(t in tr){
      guesses <- append(guesses, t$classify(x[d,]))
    }
    classifications <- append(classifications, as.integer(names(which.max(table(guesses)))))
  }
  return(classifications)
}

tree <- tree.grow(e.2.0.data, e.2.0.class, 15, 15, 41)

print(tree)

# print(tree$classify(e.3.0.data))
# print(tree.classify.bag(data, c(tree)))