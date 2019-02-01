#**************************
#return the rules of a tree
#**************************
getConds <- function(tree) {
  #store all conditions into a list
  conds <- list()
  #start by the terminal nodes and find previous conditions
  id.leafs <- which(tree$status == -1)
  j <- 0
  for (i in id.leafs) {
    j <- j + 1
    prevConds <- prevCond(tree, i)
    conds[[j]] <- prevConds$cond
    while (prevConds$id >= 1) {
      if (prevConds$id == 1) {
        conds[[j]] <- paste(conds[[j]], " => ", tree$prediction[i])
        break()
      }
      prevConds <- prevCond(tree, prevConds$id)
      conds[[j]] <- paste(conds[[j]], " & ", prevConds$cond)
    }
  }
  
  return(conds)
}

#**************************
#find the previous conditions in the tree
#**************************
prevCond <- function(tree, i) {
  if (i %in% tree$right_daughter) {
    id <- which(tree$right_daughter == i)
    cond <- paste(tree$split_var[id], ">", tree$split_point[id])
  }
  if (i %in% tree$left_daughter) {
    id <- which(tree$left_daughter == i)
    cond <- paste(tree$split_var[id], "<=", tree$split_point[id])
  }
  
  return(list(cond = cond, id = id))
}

#remove spaces in a word
space_replace <- function(x) {
  x <- sub(" ", "_", x)
  
  return(x)
}


#EXAMPLE

#preparation the random forest model quickly
data(iris)
require(randomForest)

set.seed(71)
iris.rf <- randomForest(Species ~ ., data = iris, importance = TRUE,
                        proximity = TRUE)

#how does it work?

tree <- getTree(iris.rf, k = 1, labelVar = TRUE) #which tree are you interested in (here is first (k=1) tree)

colnames(tree) <- sapply(colnames(tree), space_replace) #rename the name of the column
tree <- as.data.frame(tree)

rules <- getConds(tree) #using the created function
print(rules) #rules for the engineer not R familiar
tree
