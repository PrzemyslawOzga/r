# Lab 1 - example of using the genetic algorithm in R
# Task description: Postman's job - an algorithm based on several locations of a small town
# is designed to find the fastest way for the postman to deliver the letters.

library(GA)
library(xlsx)

# Load postman adress data
postmanDeliveryAdress <- read.xlsx(file = "Desktop/Study/Sztuczna inteligencja - poprawa/lab1/postmanAdress.xlsx",  
                                    header = TRUE, row.names = TRUE, sheetIndex = 1)
data <- as.matrix(postmanDeliveryAdress)

# Initialize adaptation function
f.adaptation <- function(postmanWay) {
  postmanWay <- c(postmanWay, postmanWay[1])
  sections <- embed(postmanWay, 2)[, 2:1]
  return (2000/sum(data[sections]))
}

# GA algorithm usage
GA <- ga(type = "permutation", fitness = f.adaptation,
         lower = 1, upper = attr(eurodist, "Size"), popSize = 40, maxiter = 2000,
         run = 500, pmutation = 0.05, pcrossover = 0.6, elitism=5, seed=1975)

# Results analyze
summary(GA)
plot(GA)

# Draw plot 
drawPlot=function(postmanWay)
{
  mds <- cmdscale(postmanDeliveryAdress)
  x <- -mds[, 1]
  y <- -mds[, 2]
  plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
  abline(h = pretty(range(x), 10), v = pretty(range(y), 10),
         col = "light gray")
  postmanWay <- c(postmanWay, postmanWay[1])
  n <- length(postmanWay)
  arrows(x[postmanWay[-n]], y[postmanWay[-n]], x[postmanWay[-1]], y[postmanWay[-1]],
         length = 0.15, angle = 25, col = "steelblue", lwd = 2)
}
drawPlot(GA@solution[1,])

# Show results in command line (console)
decode=function(postmanWay)
{
  postmanWay <- c(postmanWay, postmanWay[1])
  sections <- embed(postmanWay, 2)[, 2:1]
  shortWay <- sum(data[sections])
  cat(paste("How much adresses?:", attr(eurodist, "Size"),"\n") )
  cat(paste("The short found way length:", shortWay, "\n") )
  cat(paste("Cities on postman way:",""))
  labels <- colnames(data)
  for(index in postmanWay) {
    cat(paste("->", labels[index]),"")
  }
}
decode(GA@solution[1,])