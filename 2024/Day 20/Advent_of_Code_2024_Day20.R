setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data_orig <- readLines('input_day20_Test.txt')
data_orig <- readLines('input_day20.txt')

data <- NULL
for (Ro in 1:length(data_orig)) {
  data <- rbind(data, unlist(strsplit(data_orig[Ro], split = '')))
}

Size <- nrow(data)

Steps <- data.frame(
  Direction = 1:4,
  ro_add = c(-1,0,1,0),
  co_add = c(0,1,0,-1))
Steps$Add <- Steps$ro_add * Size + Steps$co_add

n <- Size^2
Fields <- c(t(data))
Free <- seq_along(Fields)[Fields != "#"]


# Part 1 + Part 2

Adjacent <- function(Field) {
  Temp <- Field - Steps$Add
  return(Temp[Temp %in% Free])
}

Start <- which(Fields == "S")
End <- which(Fields == "E")

priority_queue <- collections::priority_queue(Start, priorities = 0L)
Score <- rep(100000, n) 
Score[Start] <- 0
Paths <- vector("list", n)

# Using Dijkstra's algorithm
while (priority_queue$size() > 0) {
  Current <- priority_queue$pop()
  if (Current %in% Free) {
    Curr_Dist <- Score[Current]
    for (Next in Adjacent(Current)) {
      Next_Dist <- Curr_Dist + 1
      if (Score[Next] >= Next_Dist) {
        Paths[[Current]] <- c(Current, Next)
        if (Score[Next] > Next_Dist) {
          Score[Next] <- Next_Dist
          priority_queue$push(Next, priority = -Next_Dist)
        }
      }
    }
  }
}

Path <- vector("integer", length = Score[End] + 1)
Field <- Start
Path[1] <- Start

Pos <- 1
while(length(Paths[[Field]]) > 0) {
  Pos <- Pos + 1
  Field <- Paths[[Field]][2]
  Path[Pos] <- Field
}

Path_Info <- data.frame(Path = Path, Row = Path %/% Size, Col = Path %% Size + 1)

Save_Val <- function(Pos, Max_Cheat, Min_Save) {
  Row <- Path_Info$Row[Pos]
  Col <- Path_Info$Col[Pos]
  Temp <- Path_Info[(Pos+1):nrow(Path_Info),]
  L1_Dist <- abs(Temp$Row - Row) + abs(Temp$Col - Col)
  return(sum(L1_Dist <= Max_Cheat & (seq_along(L1_Dist) - L1_Dist) >= Min_Save))
}

Sum_Save <- 0
Max_Cheat <- 2
for (Pos in 1:(length(Path)-1)) {
  Sum_Save <- Sum_Save + Save_Val(Pos, Max_Cheat, 100)
}
print(Sum_Save) # Part 1: 1497

Sum_Save <- 0
Max_Cheat <- 20
for (Pos in 1:(length(Path)-1)) {
  Sum_Save <- Sum_Save + Save_Val(Pos, Max_Cheat, 100)
}
print(Sum_Save) # Part 2: 1030809
