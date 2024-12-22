setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data_orig <- readLines('input_day16_Test.txt')
#data_orig <- readLines('input_day16_Test_2.txt')
data_orig <- readLines('input_day16.txt')

data <- NULL
for (Ro in 1:length(data_orig)) {
  data <- rbind(data, unlist(strsplit(data_orig[Ro], split = '')))
}

Size <- nrow(data)

Steps <- data.frame(
  Direction = c(0,1,0,1), # 0 ... Step in row direction, step in col direction
  ro_add = c(-1,0,1,0),
  co_add = c(0,1,0,-1))
Steps$Add <- Steps$ro_add * Size + Steps$co_add

Fields <- c(t(data)) # The data is entered line by line
n <- length(Fields)
Free <- seq_along(Fields)[Fields != "#"]
Free <- c(Free, Free+n)
n_dir <- n * 2 # Fields for viewing direction step row (1 to n) respectively col (n+1) to 2n


# Part 1

Adjacent <- function(Field) {
  Dir <- Field %/% n
  Temp_Dir <- Field %% n + Steps$Add[Steps$Direction == Dir] + Dir * n
  Temp_Rot <- Field %% n + Steps$Add[Steps$Direction != Dir] + (1 - Dir) * n
  Temp <- c(Temp_Dir, Temp_Rot)  
  Temp <- Temp[Temp %in% Free]
  return(Temp)
}

Start <- which(Fields == "S") + n
End <- (which(Fields == "E") + (0:1)*n) # End for every direction

priority_queue <- collections::priority_queue(Start, priorities = 0L)
Score <- rep(100000, n_dir) 
Score[Start] <- 0
Paths <- vector("list", n_dir)

Costs <- function(Current, Next) {
  # Determine the direction of the next field 
  Dir_Next_Field <- (abs((Next %% n) - (Current %% n)) == 1) * 1 # 0 ... Step one row, 1 ... Step one col 
  Cost <- 1
  if (Current %/% n != Dir_Next_Field) {
    Cost <- Cost + 1000
  }
  return(Cost)
}

# Using Dijkstra's algorithm
while (priority_queue$size() > 0) {
  Current <- priority_queue$pop()
  Curr_Dist <- Score[Current]
  if (Curr_Dist >= min(Score[End])) {
    break
  }
  for (Next in Adjacent(Current)) {
    Next_Dist <- Curr_Dist + Costs(Current, Next)
    if (Score[Next] >= Next_Dist) {
      Paths[[Next]] <- c(Current, Paths[[Next]])
      if (Score[Next] > Next_Dist) {
        Score[Next] <- Next_Dist
        priority_queue$push(Next, priority = -Next_Dist)
      }
    }
  }
}
print(min(Score[End])) # Part 1: 72400


# Part 2

All_Path <- sort(unique(unlist(Paths[End[which.min(Score[End])]])))
Prev_All_Path <- NULL
while(!identical(All_Path, Prev_All_Path)) {
  Prev_All_Path <- All_Path
  All_Path <- sort(unique(c(Prev_All_Path, unlist(Paths[Prev_All_Path]))))
}

# Count every field only once (if both directions) + 1 for start-field
print(length(unique(All_Path %% n)) + 1) # Part 2: 435
