setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)

#data <- readLines("input_day05_Test.txt")
data <- readLines("input_day05.txt")
Split <- which(data == "")

Rules_orig <- data[1:(Split-1)]
Rules <- NULL
for (Lauf in 1:length(Rules_orig)) {
  Rules <- rbind(Rules, as.numeric(unlist(stringr::str_extract_all(Rules_orig[Lauf], "\\d+"))))
}
colnames(Rules) <- c("x", "y")
Rules <- data.frame(Rules)
List_update <- data[(Split+1):length(data)]


# Part 1 + Part 2

Correct <- function(Rules_Temp, Series) {
  Series_corrected <- Series
  Rules_Temp <- Rules_Temp %>%
    dplyr::arrange(x, y)
  for (Rule in 1:nrow(Rules_Temp)) {
    x <- Rules_Temp[Rule, "x"]
    y <- Rules_Temp[Rule, "y"]
    loc_x <- which(Series_corrected == x)
    loc_y <- which(Series_corrected == y)
    if (loc_y < loc_x) {
      Series_corrected[loc_x] <- y
      Series_corrected[loc_y] <- x
    }
  }
  return(Series_corrected)
}

Total <- 0
Total_corrected <- 0
for (Lauf in 1:length(List_update)) {
  Series <- as.numeric(unlist(stringr::str_extract_all(List_update[Lauf], "\\d+")))
  Rules_Temp <- subset(Rules, (x %in% Series) & (y %in% Series))
  
  Prev_Series_corrected <- Series
  Same <- 0
  while (Same == 0) { # Iteration until the sequence no longer changes
    Series_corrected <- Correct(Rules_Temp, Prev_Series_corrected)
    if (sum(Prev_Series_corrected != Series_corrected) == 0) {
      Same <- 1
    } else {
      Prev_Series_corrected <- Series_corrected
    }
  }
  n <- ceiling(length(Series) / 2)
  
  if (sum(Series != Series_corrected) == 0) {
    Total <- Total + Series_corrected[n]
  } else {
    Total_corrected <- Total_corrected + Series_corrected[n]
  }
}
print(Total) # Part 1: 6260
print(Total_corrected) # Part 2: 5346

