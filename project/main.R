# Node Class
# Properties: left; the left child (node or leaf)
#             right; the right child (node or leaf)
#             attribute; the attribute this node splits on
#             value; the value this node splits on
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
# Leaf class
# Properties: data_set; the class this leaf belongs to
# Methods: classify; returns the data_set variable for classification
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

# tree.grow
# Parameters: x; the input data used to create the tree, dataframe
#             y; the class labels for the data, dataframe
#             nmin; the amount of data points a node needs to split, integer
#             minleaf; the minimum amount of data points that need to be in a leaf, integer
#             nfeat; the amount of features from the total that have to be considered on each split, integer
# Result: A complete tree, node
# Description: Builds a tree based on the input data
tree.grow <- function(x, y, nmin, minleaf, nfeat) {
  impurityValue <- impurity(y)
  # If the data is pure or there aren't enough data points, this will be a leaf.
  if (impurityValue == 0 || length(y) < nmin) {
    return(leaf(data_set = as.integer(names(
      which.max(table(y))
    ))))
  }
  # Create placeholder attributes for loop
  attributeName <- NULL
  attribute <- NULL
  attributeValue <- NULL
  # Take all colnames from the data
  names <- colnames(x)
  # Get random attributes to try to split on
  predictors <- sample(names, nfeat)
  for (a in predictors) {
    uni <- unique(x[, a])
    # Check if there is more than 1 value for this attribute, as if there isn't there is no point in trying to split
    if (length(uni) > 1) {
      # Find the best split for this attribute
      s <- bestsplit(x[[a]], y, uni, impurityValue, minleaf)
      # Check if the best split on this attribute is an improvement
      if (s[2] < impurityValue) {
        # Assign values to relevant placeholder variables
        impurityValue <- s[2]
        attribute <- x[, a]
        attributeName <- a
        attributeValue <- s[1]
      }
    }
  }
  if (!is.null(attribute))
  {
    # Recursive call to grow tree, with data split. Higher values going to the left, lower to the right.
    # Assign attribute and value to node to use in classification
    return(
      node(
        attribute = attributeName,
        value = attributeValue,
        left = tree.grow(x[attribute >= attributeValue, ], y[attribute >= attributeValue], nmin, minleaf, nfeat),
        right = tree.grow(x[attribute < attributeValue, ], y[attribute < attributeValue], nmin, minleaf, nfeat)
      )
    )
  }
  # If none of the above returns, this is a leave node for the class that's seen in the y vector most
  return(leaf(data_set = as.integer(names(
    which.max(table(y))
  ))))
}

# tree.classify
# Parameters: x; the data point to classify, dataframe (single value)
#             tr; the tree to use in the classification, node
# Result: The predicted class, integer
# Description: Takes data point x and classifies it using the tree.
tree.classify <- function(x, tr) {
  # Call the reference class classify function
  tr$classify(x)
}

# tree.grow.bag
# Parameters: x; the input data used to create the tree, dataframe
#             y; the class labels for the data, dataframe
#             nmin; the amount of data points a node needs to split, integer
#             minleaf; the minimum amount of data points that need to be in a leaf, integer
#             nfeat; the amount of features from the total that have to be considered on each split, integer
#             m; the amount of trees that should be created
# Result: A complete tree, node
# Description: Builds a list of trees, containing m trees, based on the input data

tree.grow.bag <- function(x, y, nmin, minleaf, nfeat, m) {
  # Vector for trees
  trees <- c()
  # Loop as many times as wanted
  for (i in 1:m) {
    # Take samples from the data to use in tree growing
    samples <- sample(nrow(x), nrow(x), TRUE)
    # Grow tree using selected samples
    trees <-
      append(trees, tree.grow(x[samples,], y[samples], nmin, minleaf, nfeat))
  }
  # Return all trees
  return(trees)
}

# tree.classify.bag
# Parameters: x; the data that has to be classified, dataframe
#             tr; the trees to use in the classification, vector of nodes
# Result: The predicted classes, vector of integers
# Description: Takes data and classifies it using the provides trees, taking the majority vote
tree.classify.bag <- function(x, tr) {
  # Vector for classifications of all data
  classifications <- c()
  # Loop over all data
  for (d in 1:nrow(x)) {
    # Vector for guesses per tree
    guesses <- c()
    # Loop over all trees
    for (t in tr) {
      # Classify on the tree and append to guesses
      guesses <- append(guesses, t$classify(x[d, ]))
    }
    # Take the class that was given by most trees (from guesses) and add to classifications
    classifications <-
      append(classifications, as.integer(names(which.max(table(
        guesses
      )))))
  }
  # Return all classifications
  return(classifications)
}

# impurity
# Parameters: vector; the vector of classes with impurity, vector of integers
# Result: The impurity, float
# Description: Takes vector of integers and calculates impurity using Gini index
impurity <- function(vector) {
  # Get the total amount of data points
  total <- length(vector)
  # Group the data per value
  occ <- table(vector)
  # Grab the amount of 1 values there are
  occ1 <- occ[names(occ) == 1]
  # Check to prevent an error in case there isn't a single 1
  if (identical(unname(occ1), integer(0)))
    occ1 <- 0
  # Calculate Gini index and return
  return((occ1 / total) * (1 - (occ1 / total)))
}

# This method has more parameters for efficiency, this way unique values and the impurity of Y don't have to be
# calculated again in this method, and no calculations that will result in a rejection down the line due to minleaf
# being too low have to be made
# bestsplit
# Parameters: x; the vector that needs to be split on, any numeric vector
#             y; the class labels, vector of integers
#             uniX; unique version of vector x, any numeric vector
#             impY; impurity of vector y, float
#             minleaf; the minimum amount of data points that need to be in a leaf, integer
# Result: A vector of the value that should be split on (numeric) and the impurity (float)
# Description: Calculates the best split on data x which minimises impurity in y
bestsplit <- function(x, y, uniX, impY, minleaf) {
  # Sort the values of x
  x.sort <- sort(uniX)
  # Get amount of unique data points in X
  l <- length(x.sort)
  # Get amount of data points in y
  ly <- length(y)
  
  # Calculate all the values that can be split on
  x.split <- (x.sort[1:(l - 1)] + x.sort[2:l]) / 2
  # Define place holder variables
  splitValue <- impY
  impurityValue <- impY
  shortestLength <- ly
  # Loop over all possible splits
  for (split in x.split) {
    # Split the data
    dataLeft <- y[x <= split]
    dataRight <- y[x > split]
    # Get length of both pieces of data
    lDL <- length(dataLeft)
    lDR <- length(dataRight)
    # Check what is the shortest piece of data
    if (lDL < lDR)
      shortest <- lDL
    else
      shortest <- lDR
    # Check if this split will pass minleaf constraint
    if (shortest >= minleaf) {
      # Calculate impurity after split
      impuritySplit <-
        (lDL / ly) * impurity(dataLeft) + (lDR / ly) * impurity(dataRight)
      # If this is better than current best impurity, use this
      if (impuritySplit < impurityValue) {
        # Assign to placeholder variables
        splitValue <- split
        impurityValue <- impuritySplit
        shortestLength <- shortest
      }
    }
  }
  # Return relevant data to grow function
  return(c(splitValue, impurityValue))
}



# Load in training and test data
e.2.0.data <- read.csv("./eclipse-metrics-packages-2.0.csv")
e.3.0.data <- read.csv("./eclipse-metrics-packages-3.0.csv")

# Take relevant columns from data
e.2.0.data <- e.2.0.data[1:42]
e.3.0.data <- e.3.0.data[1:42]

# Make boolean classes (either there are post release bugs or there aren't)
e.2.0.data$post[e.2.0.data$post > 0] <- 1
e.3.0.data$post[e.3.0.data$post > 0] <- 1

# Create class labels
e.2.0.class <- e.2.0.data$post
e.3.0.class <- e.3.0.data$post

# Remove class label from data
e.2.0.data$post <- NULL
e.3.0.data$post <- NULL

# Load in pima verification set
pima <- read.csv("./pima.csv")
pimaData <- pima[1:8]
pimaClass <- pima[, 9]

# Verification data from the assignment (pima dataset)
#tree <- tree.grow(pimaData, pimaClass, 20, 5, 8)
#pred <- tree.classify.bag(pimaData, c(tree))
#print("Pima")
#print(table(pred, pimaClass))

# Single tree, with classification on one piece of data to confirm working and on the full data
tree <- tree.grow(e.2.0.data, e.2.0.class, 15, 15, 41)
#print(tree$classify(e.3.0.data[180,]))
#pred <- tree.classify.bag(e.3.0.data, c(tree))
#print("Single")
#print(table(pred, e.3.0.class))

# Bagging with classication on the full data
#trees <- tree.grow.bag(e.2.0.data, e.2.0.class, 15, 15, 41, 4)
#preds <- tree.classify.bag(e.3.0.data, trees)
#print("Bag")
#print(table(preds, e.3.0.class))

# Random forest with classification on the full data
#trees <- tree.grow.bag(e.2.0.data, e.2.0.class, 15, 15, 6, 100)
#preds <- tree.classify.bag(e.3.0.data, trees)
#print("Rand For")
#print(table(preds, e.3.0.class))