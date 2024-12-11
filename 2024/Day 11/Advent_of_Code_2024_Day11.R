setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data_orig <- readLines("input_day11_Test.txt")
data_orig <- readLines("input_day11.txt")
data <- as.numeric(unlist(stringr::str_extract_all(data_orig, "\\d+")))

options(digits = 22L)


# Part 1

Stones <- data
for (Loop in 1:25) {
  Blink <- vector("numeric", length = 2*length(Stones))
  Pos_new <- 1
  Stones_len <- floor(log10(Stones)) + 1
  for (Pos in 1:length(Stones)) {
    Temp <- Stones[Pos]
    if (Temp == 0) { # First rule
      Blink[Pos_new] <- 1
      Pos_new <- Pos_new + 1
    } else if (Stones_len[Pos] %% 2 == 0) { # Second rule
      Split <- floor(Stones_len[Pos] / 2)
      Blink[Pos_new] <- Temp %/% (10 ** Split)
      Blink[Pos_new+1] <- Temp %% (10 ** Split)
      Pos_new <- Pos_new + 2
    } else {
      Blink[Pos_new] <- Temp * 2024
      Pos_new <- Pos_new + 1
    }
  }
  Stones <- Blink[1:(Pos_new-1)]
}
print(as.character(length(Stones))) # Part 1: 186996


# Part 2

Count_Stones <- function(Deep, Stones_in) {
  Stones <- as.numeric(names(table(Stones_in)))
  Stones_Count <- as.vector(table(Stones_in)) * 1.0 # to get numeric instead of integer
  for (Loop in 1:Deep) {
    New_Stones <- NULL
    New_Stones_Count <- NULL
    for (Pos in 1:length(Stones)) {
      if (Stones[Pos] == 0) { # First rule
        Next <- 1
      } else if ((floor(log10(Stones[Pos])) + 1) %% 2 == 0) { # Second rule
        Split <- floor((floor(log10(Stones[Pos])) + 1) / 2)
        Next <- c(Stones[Pos] %/% (10 ** Split), Stones[Pos] %% (10 ** Split))
      } else {
        Next <- Stones[Pos] * 2024
      }
      for (Stone in Next) {
        if (Stone %in% New_Stones) {
          Pos_new <- which(Stone == New_Stones)
          New_Stones_Count[Pos_new] <- New_Stones_Count[Pos_new] + Stones_Count[Pos]
        } else {
          New_Stones <- c(New_Stones, Stone)
          New_Stones_Count <- c(New_Stones_Count, Stones_Count[Pos])
        }
      }
    }
    Stones <- New_Stones
    Stones_Count <- New_Stones_Count
  }
  Rueckgabe <- NULL
  
  return(sum(Stones_Count))
}

print(as.character(Count_Stones(25, data))) # Part 1 - again: 186996
print(as.character(Count_Stones(75, data))) # Part 2: 221683913164898

