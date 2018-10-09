source("helpers.R")
credit.data <- read.csv("./credit.txt")
class <- credit.data[,6]
data <- subset(credit.data, select=c("income", "age"))

root <- setRefClass("root", fields = list(left = "ANY", right = "ANY"))
node <- setRefClass("node", fields = list(left = "ANY", right = "ANY", attribute ="ANY", value="ANY"))
leaf <- setRefClass("leaf", fields = list(data_set = "ANY"))

tree.grow <- function(x, y, nmin, minleaf, nfeat) {
  for (attribute in x) {
    x[1] <- NULL

    if (impurity(y) == 0) {
      print("leaf")
      return(leaf(data_set=attribute))
    }
    
    S <- bestsplit(attribute, y)
    df <- data.frame(x, value=y)
    s.star <- split(df, attribute < S)
  
    left.node <- tree.grow(s.star$F[1], s.star$F$value, nmin, minleaf, nfeat)
    right.node <- tree.grow(s.star$T[1], s.star$T$value, nmin, minleaf, nfeat)
    
    return(node(left=left.node, right=right.node, attribute=attribute, value=y))
  }
  
  return(root(left=node$left, right=node$right))
}


tree <- tree.grow(data,class,1,1,1)

print(tree)