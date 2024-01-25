# Lab 2 - artificial neural networks
# source of 3 different data sources (different from the examples in the lab/result).
# Develop 3 different classifiers on an artificial neural network (one for each data source):
# by initial data set into training data and test data final methods,
# network structure access,
# basis of the training process,
# assessment of the quality of the classifier based on the obtained tested curve,
# The resulting classifier and error obtained for the test data.
#
#
# Classifier 3 - artificial neural networks
# Training model that was improved based on the given data about irises (data other than those raised in classes),
# determination of the type (species) of the flower.

library(AMORE)

# Load dataset
dataset <- read.csv("/Users/przemyslawozga/Desktop/Study/Sztuczna inteligencja - poprawa/lab2/datasets/IRIS.csv")

# Preparing test and training data
set.seed(3000)
howMuch = nrow(dataset)
idxTraining <- sample(1:howMuch, 2*howMuch/3)
idxTesting <- setdiff(1:howMuch, idxTraining)

# nitialize function
target <- function(x) {
  n <- length(x)
  vals <- levels(x)
  valueLength <- length(vals)
  T <- matrix(0, nrow = n, ncol = valueLength)
  for(i in 1:valueLength) {
    T[,i] <- (x == vals[i])
  }
  colnames(T) <- vals
  return(T)
}
x = factor(dataset$species)
assignedValues <- target(x)
assignedValues

# Initialize neural network
neuralNetwork <- newff(n.neurons = c(6,12,3),
                       learning.rate.global = 0.05,
                       momentum.global = 0.5,
                       hidden.layer = "sigmoid",
                       output.layer = "purelin",
                       method = "ADAPTgdwm",
                       error.criterium = "LMS")

# Train model, prepare and show results
results <- train(neuralNetwork,
                 dataset[idxTraining, -5],
                 assignedValues[idxTraining,],
                 error.criterium = "LMS",
                 report = TRUE,
                 show.step = 5,
                 n.shows = 800)

# Create plots
plot(results$Merror, type="l", xlab="Iteration", ylab="Value", col="darkred")

y <- sim(results$net, dataset[idxTesting, -5])
y

clasificationTests <- function(assign, wy) {
  assigned <- max.col(assign)
  recognized <- max.col(wy)
  print(table(assigned, recognized))
}
results <- clasificationTests(assignedValues[idxTesting,], y)

cat("Classification accuracy:",
    sum(diag(results)) / sum(results) * 100, "%\n")
