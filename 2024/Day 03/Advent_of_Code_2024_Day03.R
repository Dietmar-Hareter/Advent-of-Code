setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data <- readLines("input_day03_Test.txt")
data <- readLines("input_day03.txt")
data <- paste0(data, collapse = "")

# Part 1

Split <- unlist(stringr::str_extract_all(data, "mul\\(\\d+,\\d+\\)"))
sum( sapply(stringr::str_extract_all(Split, "\\d+"), function(x) {
  prod(as.numeric(unlist(x)))
}) )

Locate <- data.frame(stringr::str_locate_all(data, "mul\\(\\d+,\\d+\\)")[[1]])
Total <- 0
for (Lauf in 1:nrow(Locate)) {
  x <- Locate[Lauf,]
  Temp <- stringr::str_sub(data, start = x$start+4, end = x$end-1)
  Total <- Total + prod(as.numeric(unlist(stringr::str_split(Temp, ","))))
}
print(Total) # Part 1: 179834255


# Part 2

do <- vector("numeric", nchar(data)) + 1
Total <- 0
active <- 1
for (Lauf in 1:nchar(data)) {
  if (stringr::str_sub(data, start = Lauf, end = Lauf + 3) == "do()") {
    active <- 1
  }
  if (stringr::str_sub(data, start = Lauf, end = Lauf + 6) == "don't()") {
    active <- 0
  }
  do[Lauf] <- active
}

Total <- 0
for (Lauf in 1:nrow(Locate)) {
  x <- Locate[Lauf,]
  if (do[x$start] == 1) {
    Temp <-  stringr::str_sub(data, start = x$start+4, end = x$end-1)
    Total <- Total + prod(as.numeric(unlist(stringr::str_split(Temp, ","))))
  }
}
print(Total) # Part 2: 80570939
