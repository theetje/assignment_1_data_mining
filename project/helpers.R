impurity <- function(vector){
  total <- length(vector)
  occ <- table(vector)
  occ1 <- occ[names(occ) == 1]
  if(identical(unname(occ1), integer(0)))
    occ1 <- 0
  return((occ1 / total) * (1 - (occ1 / total)))
}

delta.impurity <- function(t, l, r) {
  p.l <- count(l)
  p.r <- count(r)
  imp.l <- impurity(l)
  imp.r <- impurity(r)
  i.t <- impurity(t)
  
  return(i.t - p.l * imp.l - p.r * imp.r)
}

make.s.star <- function(attribute, y) {
  S <- bestsplit(attribute, y)
  df <- data.frame(attribute, value=y)
  
  return(split(df, attribute < S))
}
