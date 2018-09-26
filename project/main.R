credit.dat <- read.csv("./credit.txt")
t <- c(1,0,1,1,1,0,0,1,1,0,1)

impurity <- function(vector){
  total <- length(vector)
  occ <- table(vector)
  occ1 <- occ[names(occ) == 1]
  if(identical(unname(occ1), integer(0)))
    occ1 <- 0
  return((occ1 / total) * (1 - (occ1 / total)))
}
impurity(t)

bestsplit <- function(x, y){
  x.sort <- sort(unique(x))
  l <- length(x.sort)
  ly <- length(y);
  x.split <- (x.sort[1:(l - 1)] + x.sort[2:l])/2
  impurityValue <- c()
  splitValue <- NULL
  impurityValue <- impurity(y)
  for(split in x.split){
    dataLeft <- y[x <= split]
    dataRight <- y[x > split]
    impuritySplit = (length(dataLeft) / ly) * impurity(dataLeft) + (length(dataRight) / ly) * impurity(dataRight)
    if(impuritySplit < impurityValue){
      splitValue <- split
      impurityValue <- impuritySplit
    }
  }
  return(splitValue)
}

bestsplit(credit.dat[,4],credit.dat[,6])