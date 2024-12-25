setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data_orig <- readLines("input_day25_Test.txt")
data_orig <- readLines("input_day25.txt")

data <- stringr::str_split(data_orig, "")

Locks <- NULL
Keys <- NULL
Shema <- 1
readin <- TRUE

while(readin) {
  Temp <- do.call(rbind, data[Shema:(Shema+6)])
  Temp_num <- colSums(Temp == "#")
  if (Temp[1,1] == "#") {
    Locks <- rbind(Locks, Temp_num)
  } else {
    Keys <- rbind(Keys, Temp_num)
  }
  Shema <- Shema + 8
  if (Shema > length(data)) {
    readin <- FALSE
  }
}

Fits <- 0
for (Lock in 1:nrow(Locks)) {
  for (Key in 1:nrow(Keys)) {
    if (max(Locks[Lock,] + Keys[Key,]) <= 7) {
      Fits <- Fits + 1
    }
  }
}

print(Fits) # Part 1: 3451
