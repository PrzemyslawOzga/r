# Lab 2 - artificial neural networks
# Find 3 different data sources (different from the examples in the lab/lecture).
# Develop 3 different classifiers based on an artificial neural network (one for each data source):
# initially divide the data into training data and test data according to the selected method,
# propose a network structure,
# carry out the training process,
# evaluate the quality of the classifier based on the obtained training curve,
# test the obtained classifier and determine the classification error obtained for the test data.
#
#
# Classifier 2 - artificial neural networks
# Model training that classifies its quality based on the entered information about the milk.

library(AMORE)

# Load dataset
dataset <- read.csv("/Users/przemyslawozga/Desktop/Study/Sztuczna inteligencja - poprawa/lab2/datasets/milknew.csv")

# Preparing test and training data
set.seed(1500)
howMuch = nrow(dataset)
idxTraining <- sample(1:howMuch, 4*howMuch/5)
idxTesting <- setdiff(1:howMuch, idxTraining)

# Initialize function
target <- function(x) {
  n <- length(x)
  vals <- levels(x)
  sexValuelength <- length(vals)
  T <- matrix(0, nrow = n, ncol = sexValuelength)
  for(i in 1:sexValuelength) {
    T[,i] <- (x == vals[i])
  }
  colnames(T) <- vals
  return(T)
}
x = factor(dataset$Grade)
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
                 dataset[idxTraining, -8],
                 assignedValues[idxTraining,],
                 error.criterium = "LMS",
                 report = TRUE,
                 show.step = 5,
                 n.shows = 500)

# Create plots
plot(results$Merror, type="l", xlab="Iteration", ylab="Value", col="darkred")

y <- sim(results$net, dataset[idxTesting, -8])
y

clasificationTests <- function(assign, wy) {
  assigned <- max.col(assign)
  recognized <- max.col(wy)
  print(table(assigned, recognized))
}
results <- clasificationTests(assignedValues[idxTesting,], y)

cat("Classification accuracy:",
    sum(diag(results)) / sum(results) * 100, "%\n")
