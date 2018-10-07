source("helpers.R")
credit.data <- read.csv("./credit.txt")
class <- credit.data[,6]
data <- subset(credit.data, select=c("age", "income"))

root <- setRefClass("root", fields = list(left = "ANY", right = "ANY"))
node <- setRefClass("node", fields = list(left = "ANY", right = "ANY", attribute ="ANY", value="ANY"))
leaf <- setRefClass("leaf", fields = list(data_set = "ANY"))

tree.grow <- function(x, y, nmin, minleaf, nfeat) {
  print("X AT START: ")
  print(x)
  if (impurity(y) == 0) {
    print("pure")
    return(leaf(data_set=x))
  } else {
    print("impure")
    attribute <- x[,1]
    x[1] <- NULL
    S <- bestsplit(x[,1], y)
    df <- data.frame(attribute=attribute, value=y)
    s.star <- split(df, df[,1] < S)
    print(s.star$T)
    left.node <- tree.grow(s.star$F[1], s.star$F[2], nmin, minleaf, nfeat)
    right.node <- tree.grow(s.star$T[1], s.star$T[2], nmin, minleaf, nfeat)
    
    return(node(left=left.node, right=right.node, attribute=x, value=y))
  }
}

tree <- tree.grow(data,class,1,1,1)
