impurity <- function(vector) {
  total <- length(vector)
  occ <- table(vector)
  occ1 <- occ[names(occ) == 1]
  if (identical(unname(occ1), integer(0)))
    occ1 <- 0
  return((occ1 / total) * (1 - (occ1 / total)))
}


bestsplit <- function(x, y, uniX, impY, minleaf) {
  x.sort <- sort(uniX)
  l <- length(x.sort)
  ly <- length(y)
  
  x.split <- (x.sort[1:(l - 1)] + x.sort[2:l]) / 2
  splitValue <- impY
  impurityValue <- impY
  shortestLength <- ly
  for (split in x.split) {
    dataLeft <- y[x <= split]
    dataRight <- y[x > split]
    lDL <- length(dataLeft)
    lDR <- length(dataRight)
    if (lDL < lDR)
      shortest <- lDL
    else
      shortest <- lDR
    if (shortest >= minleaf) {
      impuritySplit <-
        (lDL / ly) * impurity(dataLeft) + (lDR / ly) * impurity(dataRight)
      if (impuritySplit < impurityValue) {
        splitValue <- split
        impurityValue <- impuritySplit
        shortestLength <- shortest
      }
    }
  }
  return(c(splitValue, impurityValue, shortestLength))
}

