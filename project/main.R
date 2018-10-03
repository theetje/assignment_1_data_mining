source("helpers.R")
credit.data <- read.csv("./credit.txt")
class <- credit.dat[,6]
data <- subset(credit.dat, select=c("age", "income"))

root <- setRefClass("root", fields = list(left = "ANY", right = "ANY"))
node <- setRefClass("node", fields = list(left = "ANY", right = "ANY", attribute ="ANY", value="ANY"))
leaf <- setRefClass("leaf", fields = list(data_set = "ANY"))


tree.grow <- function(x, y, nmin, minleaf, nfeat) {
  node <- node(left=NULL, right=NULL, attribute=x, value=y)

  if (impurity(node$value) > 0) {
    attribute <- subset(node$attribute, select=1)
    
    S <- bestsplit(attribute[,1], node$value)
    print(S)
    # Split S make left and right node.
  }
  
  # for(attribute in node$attribute) {
  #   print()
  #   print(impurity(y))
  #   if (impurity(node) > 0) {
  #     S <- bestSplit(node, y)
  #     print(S)
  #     # left <- tree.grow(dataleft)
  #     # right <- tree.grow(dataRight)
  #   }
  # }

  # node <- function(left, right, data_set) {
  #   root(left = left, right = right)
  # }
   return(root)
}

tree.grow(data,class,1,1,1)