setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data <- read.table("input_day01_Test.txt", quote = "\"")
data <- read.table("input_day01.txt", quote = "\"")

# Part 1

print( sum( abs(sort(data[,1]) - sort(data[,2])) ) ) # Part 1: 1530215

# Part 2

print( sum( sapply(data[,1], function(x) { x * sum(data[,2] == x)}) ) ) # Part 2: 26800609