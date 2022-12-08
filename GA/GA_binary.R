#Labolatorium 1 - przykład użycia algorytmu genetycznego w R (binary type)
#Opis zadania: Rekrut wyjeżdża na szkołę przetrwania. Każdy zabrany przez niego przedmiot ma odpowiednią 
#liczbę punktów odzwierciedlających szansę na przetrwanie, jednakże pojemność jego bagażu jest ograniczona 
#do 30. Jakie przedmioty dadzą rekrutowi największą szansę na przetrwanie? 

library(GA)

sizeOfBaggage = data.frame(
  items = c("scyzoryk", "noz", "siekierka", "namiot", "spiwor", "konserwy", "lina", 
                "zapałki", "drewno", "siatka", "apteczka", "latarka", "lopata", "kocTermiczny", "bidon"),
  survivalChance = c(5, 10, 10, 20, 15, 20, 10, 5, 5, 10, 10, 5, 10, 10, 20),
  itemWeight = c(1, 2, 4, 10, 6, 8, 2, 1, 5, 3, 3, 3, 5, 2, 1))
maxItemsInBaggage = 30

chromosome = c(0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0)
sizeOfBaggage[chromosome == 1, ]

cat(chromosome %*% sizeOfBaggage$survivalChance)
fitnessFunc = function(chr) {
  allValueChromosome = chr %*% sizeOfBaggage $ survivalChance
  allWeightChromosome = chr %*% sizeOfBaggage $ itemWeight
  if (allWeightChromosome > maxItemsInBaggage) 
    return (-allValueChromosome) 
  else 
    return (allValueChromosome)
}

results = ga(type = "binary", nBits=15, fitness=fitnessFunc, popSize=100,
            pcrossover=0.95, pmutation=0.02, elitism=5, maxiter=50, seed=10)

summary(results)
plot(results)

decode=function(chr){
  print ("Results summary: ")
  print (sizeOfBaggage[chr == 1, ])
  print (paste("Baggage weight = ", chr %*% sizeOfBaggage $ itemWeight))
  print (paste("Items value =",chr %*% sizeOfBaggage $ survivalChance))
}
decode(results@solution[1,])