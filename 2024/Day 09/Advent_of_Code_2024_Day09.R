setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data_input_orig <- readLines('input_day09_Test.txt')
data_input_orig <- readLines('input_day09.txt')
data_input <- as.numeric(stringr::str_split_1(data_input_orig, ""))
n_data <- length(data_input)


# Part 1

Block <- vector("numeric", length = sum(data_input))
ID <- 0
File <- 1
pos <- 1

for (Digit in 1:n_data) {
  Step <- data_input[Digit]
  if (Step > 0) {
    Block[(pos):(pos+Step-1)] <- ifelse(File == 1, ID, -1)
    if (File == 1) {
      ID <- ID + 1
    }
    pos <- pos + Step
  }
  File <- (File + 1) %% 2
}

Block_orig <- Block

up <- 1
down <- length(Block)
Checksum <- 0
Rerange <- TRUE

while (Rerange) {
  if (Block[up] != -1) {
    Checksum <- Checksum + Block[up] * (up -1)
    up <- up + 1
  } else {
    Block[up] <- Block[down]
    down <- down - 1
  }
  if (up > down) {
    Rerange <- FALSE
  }
}
print(as.character(Checksum)) # Part 1: 6471961544878


# Part 2

Block <- Block_orig

Blockstarts <- c(1, 1 + cumsum(data_input))
Length_Files <- data_input[seq(1, length(data_input), 2)]
Start_Files <- Blockstarts[seq(1, length(data_input), 2)]
Length_Free <- data_input[seq(2, length(data_input), 2)]
Start_Free <- Blockstarts[seq(2, length(data_input), 2)]

ID_max <- max(Block)
for (ID in ID_max:0) {
  len_ID <- Length_Files[ID+1]
  start_ID <- Start_Files[ID+1]
  
  Free_pos <- Start_Free[Start_Free <= start_ID]
  Loop <- TRUE
  pos <- 1
  while(Loop) {
    if (Length_Free[pos] >= len_ID) {
      Start_In <- Start_Free[pos]
      Start_Out <- Start_Files[ID+1]
      Block[Start_Out:(Start_Out+len_ID-1)] <- -1
      Block[Start_In:(Start_In+len_ID-1)] <- ID
      Start_Free[pos] <- Start_Free[pos] + len_ID
      Length_Free[pos] <- Length_Free[pos] - len_ID
      Loop <- FALSE
    } else {
      pos <- pos + 1
      if (pos > length(Free_pos)) {
        Loop <- FALSE
      }
    }
  }
}

Checksum <- 0
for (pos in 1:length(Block)) {
  if (Block[pos] != -1) {
    Checksum <- Checksum + Block[pos] * (pos - 1)
  }
}
print(as.character(Checksum)) # Part 2: 6511178035564
