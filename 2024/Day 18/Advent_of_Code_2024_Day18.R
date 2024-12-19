setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Test <- FALSE
if (Test) {
  data_orig <- readLines("input_day18_Test.txt")
  Size <- 7 
  Bytes <- 12
} else {
  data_orig <- readLines("input_day18.txt")
  Size <- 71
  Bytes <- 1024
}

data <- NULL
for (ro in seq_along(data_orig)) {
  data <- rbind(data, as.numeric(unlist(stringr::str_extract_all(data_orig[ro], "\\d+"))))
}
data_orig <- data
data <- data + 1

Steps <- data.frame(
  Direction = 1:4,
  ro_add = c(-1,0,1,0),
  co_add = c(0,1,0,-1))
Steps$Add <- Steps$ro_add * (Size+2) + Steps$co_add

n <- (Size+2)^2


# Part 1

Adjacent <- function(Field, Free) {
  Temp <- Field - Steps$Add
  return(Temp[Temp %in% Free])
}

Create_Field <- function(Bytes_in) {
  Fields <- matrix(0, nrow = Size, ncol = Size)
  Fields[data[1:Bytes_in,c(2,1)]] <- 1
  Fields <- rbind(1, cbind(1, Fields, 1), 1) # Limitation of the field 
  Fields <- c(t(Fields))
  Free <- seq_along(Fields)[Fields == 0]
  return(Free)
}

# Using Dijkstra's algorithm
Check_Path <- function(Bytes_in) {
  Free <- Create_Field(Bytes_in)
  
  Start <- min(Free)
  End <- max(Free)
  
  priority_queue <- collections::priority_queue(Start, priorities = 0L)
  Score <- rep(100000, n) 
  Score[Start] <- 0
#  Paths <- vector("list", n)
  
  while (priority_queue$size() > 0) {
    Current <- priority_queue$pop()
    if (Current %in% Free) {
      Curr_Dist <- Score[Current]
      for (Next in Adjacent(Current, Free)) {
        Next_Dist <- Curr_Dist + 1
        if (Score[Next] >= Next_Dist) {
#          Paths[[Next]] <- c(Current, Paths[[Next]])
          if (Score[Next] > Next_Dist) {
            Score[Next] <- Next_Dist
            priority_queue$push(Next, priority = -Next_Dist)
          }
        }
      }
    }
  }
  
  return(Score[End])
}

print(Check_Path(Bytes)) # Part 1: 264


# Part 2

Down <- Bytes
Up <- nrow(data)

while(Down < Up) {
  Middle <- (Up + Down) %/% 2
  Temp <- Check_Path(Middle)
  
  if (Temp == 100000) { # no solution
    Up <- Middle - 1
  } else {
    Down <- Middle + 1
  }
}

Check_Path(Up)
Check_Path(Up+1)

print(stringr::str_c(data_orig[Up+1,], collapse = ",")) # Part 2: 42,27
