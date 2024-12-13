setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data <- readLines("input_day13_Test.txt")
data <- readLines("input_day13.txt")
options(digits = 22L)


# Part 1 + Part 2

Count_Tokens <- function(Add) {
  Game <- 1
  Play <- TRUE
  Tokens <- 0
  while (Play == TRUE) {
    Button_A <- as.numeric(unlist(stringr::str_extract_all(data[Game], "\\d+")))
    Button_B <- as.numeric(unlist(stringr::str_extract_all(data[Game+1], "\\d+")))
    Prize <- as.numeric(unlist(stringr::str_extract_all(data[Game+2], "\\d+"))) + Add
    Machine <- as.matrix(data.frame(A = Button_A, B = Button_B))
    Push <- round(solve(Machine, Prize), 0)
    if (identical(as.vector(Machine %*% Push), Prize)) {
      Tokens <- Tokens + 3 * Push[1] + Push[2]
    }
    Game <- Game + 4
    if (Game > length(data)) {
      Play <- FALSE
    }
  }
  return(Tokens)
}

print(as.character(Count_Tokens(0))) # Part 1: 37128
print(as.character(Count_Tokens(10000000000000))) # Part 2: 74914228471331
