setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data <- readLines("input_day02_Test.txt")
data <- readLines("input_day02.txt")

# Part 1

Total <- 0
for (Zeile in seq_along(data)) {
  Correct <- 0
  series <- as.numeric(unlist(stringr::str_split(data[Zeile], " ")))
  series_diff <- diff(series)
  
  if ((abs(sum(sign(series_diff))) == length(series_diff)) & (max(abs(series_diff)) <= 3)) {
    Correct <- 1
  }
  Total <- Total + Correct
}
print(Total) # Part 1: 572

# Part 2

Total <- 0
for (Zeile in seq_along(data)) {
  Correct <- 0
  series <- as.numeric(unlist(stringr::str_split(data[Zeile], " ")))
  for (Lauf in seq_along(series)) {
    series_short <- series[-Lauf]
    series_diff <- diff(series_short)

    if ((abs(sum(sign(series_diff))) == length(series_diff)) & (max(abs(series_diff)) <= 3)) {
      Correct <- 1
    }
  }
  Total <- Total + Correct
}
print(Total) # Part 2:612
