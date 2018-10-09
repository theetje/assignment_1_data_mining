source("helpers.R")
credit.data <- read.csv("./credit.txt")
class <- credit.data[, 6]
data <- credit.data[, -6]

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
    methods = list(classify = function(x) {
      if(x[attribute] > value)
        return(left$classify(x))
      else
        return(right$classify(x))
    })
  )
leaf <-
  setRefClass(
    "leaf",
    fields = list(data_set = "ANY"),
    methods = list(classify = function(x) {
      return(data_set)
    })
  )

tree.grow <- function(x, y, nmin, minleaf, nfeat) {
  if (impurity(y) == 0 || length(y) < minleaf) {
    return(leaf(data_set = as.integer(names(which.max(table(y))))))
  }
  impurityValue <- impurity(y)
  attributeName <- NULL
  attribute <- NULL
  attributeValue <- NULL
  names <- colnames(data)
  i <- 1
  for (a in x) {
    if (length(unique(a)) > 1) {
      s <- bestsplit(a, y)
      if (s[2] < impurityValue) {
        impurityValue <- s[2]
        attribute <- a
        attributeName <- names[i]
        attributeValue <- s[1]
      }
    }
    i <- i + 1
  }
  if (!is.null(attribute))
  {
    df <- data.frame(x, value = y)
    s.star <- split(df, attribute < attributeValue)
    left.node <-
      tree.grow(s.star$F[1], s.star$F$value, nmin, minleaf, nfeat)
    right.node <-
      tree.grow(s.star$T[1], s.star$T$value, nmin, minleaf, nfeat)
    
    return(
      node(
        left = left.node,
        right = right.node,
        attribute = attributeName,
        value = attributeValue
      )
    )
  }
  return(leaf(data_set = as.integer(names(which.max(table(y))))))
}


tree <- tree.grow(data, class, 1, 1, 1)

print(tree)

print(tree$classify(data[7,]))