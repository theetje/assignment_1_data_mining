source("helpers.R")
credit.data <- read.csv("./credit.txt")
class <- credit.data[, 6]
data <- credit.data[,-6]

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
        print(attribute)
        print(x[attribute])
        print(value)
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
  print(x)
  if (impurity(y) == 0 || length(y) < nmin) {
    return(leaf(data_set = as.integer(names(
      which.max(table(y))
    ))))
  }
  impurityValue <- impurity(y)
  attributeName <- NULL
  attribute <- NULL
  attributeValue <- NULL
  names <- colnames(data)
  predictors <- sample(names, nfeat)
  print(data$"gender")
  print(predictors)
  for (a in predictors) {
    if (length(unique(x[[a]])) > 1) {
      s <- bestsplit(x[[a]], y)
      if (s[2] < impurityValue) {
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


tree <- tree.grow(data, class, 1, 1, 5)

print(tree)

print(tree$classify(data[7, ]))