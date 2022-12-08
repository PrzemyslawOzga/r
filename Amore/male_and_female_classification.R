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
#Klasyfikator 1 - sztuczne sieci neuronowe
#Trening modelu, który na podstawie wagi oraz wzrostu człowieka, oszacowuje czy są to parametry
#odpowiadające płci męskiej czy żeńskiej.

library(AMORE)

#Load dataset
dataset <- read.csv("Desktop/Study/Sztuczna inteligencja - poprawa/lab2/datasets/training_set.csv")

#Preparing test and training data
set.seed(1000)
howMuch = nrow(dataset)
idxTraining <- sample(1:howMuch, 2*howMuch/3)
idxTesting <- setdiff(1:howMuch, idxTraining)

#Initialize function
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
x = factor(dataset$Sex)
assignedValues <- target(x)
assignedValues

#Initialize neural network
neuralNetwork <- newff(n.neurons = c(4,8,2),
              learning.rate.global = 0.05,
              momentum.global = 0.5,
              hidden.layer = "sigmoid",
              output.layer = "purelin",
              method = "ADAPTgdwm",
              error.criterium = "LMS")

#Train model, prepare and show results
results <- train(neuralNetwork,
                 dataset[idxTraining,-3],
                 assignedValues[idxTraining,],
                 error.criterium = "LMS",
                 report = TRUE,
                 show.step = 5,
                 n.shows = 800)

#Create plots
plot(results$Merror, type="l", xlab="Iteration", ylab="Value", col="darkred")

y <- sim(results$net, dataset[idxTesting, -3])
y

clasificationTests <- function(assign, wy) {
  assigned <- max.col(assign)
  recognized <- max.col(wy)
  print(table(assigned, recognized))
}
results <- clasificationTests(assignedValues[idxTesting,], y)

cat("Classification accuracy:",
    sum(diag(results)) / sum(results) * 100, "%\n")
