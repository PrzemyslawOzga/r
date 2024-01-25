# Laboratory 1 - example of using the evolutionary algorithm in R (real-valued type)
# Task description: Find the maximum of the function f(x) = (x^2 + 5*(-x)) * cos(2*x) for the interval <-5;10>

library(GA)

fp = function(x) {
  return ((x^2 + 5*(-x)) * cos(2*x))
} 

results = ga (type = "real-valued", fitness = fp, lower = -5, upper = 10)
summary(results)
plot(results)

fm = function(obj) {
  curve (fp, -5, 10, main = paste ("iteration = ", obj@iter))
  points (obj@population, obj@fitness, pch = 20, col="red")
}

maxIter = 50
rPopulation = 10
results = ga (type = "real-valued",
              fitness = fp, 
              lower = -5,
              upper = 10,
              popSize = rPopulation, 
              pcrossover = 0.95,
              pmutation = 0.05,
              elitism = rPopulation * 0.1,
              monitor = fm,
              maxiter = maxIter,
              keepBest = TRUE,
              seed = 10)

summary(results)
plot(results)