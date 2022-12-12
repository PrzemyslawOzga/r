#Labolatorium 2 - sztuczne sieci neuronowe
#Znajdź 3 różne źródła danych (inne niż w przykładach na laboratorium/wykładzie).
#Opracować 3 różne klasyfikatory bazujące na sztucznej sieci neuronowej (po jednym dla każdego źródła danych):
#   dokonać wstępnego podziału danych na dane trenujące i dane testowe według wybranej metody,
#   zaproponować strukturę sieci,
#   przeprowadzić proces trenowania,
#   ocenić jakość klasyfikatora na podstawie uzyskanej krzywej trenowania,
#   przetestować uzyskany klasyfikator i określić błąd klasyfikacji, uzyskany dla danych testowych.
#
#
#Klasyfikator 3 - sztuczne sieci neuronowe
#Trening modelu, który na podstawie wprowadzonych danych o irysach (innych danych niż poruszone na zajęciach), 
#określa rodzaj (gatunek) kwiata.

library(AMORE)

#Load dataset
dataset <- read.csv("/Users/przemyslawozga/Desktop/Study/Sztuczna inteligencja - poprawa/lab2/datasets/IRIS.csv")

#Preparing test and training data
set.seed(3000)
howMuch = nrow(dataset)
idxTraining <- sample(1:howMuch, 2*howMuch/3)
idxTesting <- setdiff(1:howMuch, idxTraining)

#Initialize function
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

#Initialize neural network
neuralNetwork <- newff(n.neurons = c(6,12,3),
                       learning.rate.global = 0.05,
                       momentum.global = 0.5,
                       hidden.layer = "sigmoid",
                       output.layer = "purelin",
                       method = "ADAPTgdwm",
                       error.criterium = "LMS")

#Train model, prepare and show results
results <- train(neuralNetwork,
                 dataset[idxTraining, -5],
                 assignedValues[idxTraining,],
                 error.criterium = "LMS",
                 report = TRUE,
                 show.step = 5,
                 n.shows = 800)

#Create plots
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
