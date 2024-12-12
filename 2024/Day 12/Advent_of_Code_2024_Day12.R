setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data_input <- readLines('input_day12_Test.txt')
data_input <- readLines('input_day12.txt')

data <- NULL
for (i in seq_along(data_input)) {
  data <- rbind(data, unlist(strsplit(data_input[i], split = '')))
}

Ro_max <- nrow(data)
Co_max <- ncol(data)
Ro_mul <- (10 ** (floor(log10(Ro_max)) + 2))

Steps <- data.frame(
  Direction = 1:4,
  ro_add = c(-1,0,1,0),
  co_add = c(0,1,0,-1))


# Part 1 + Part 2

Examined <- matrix(0, nrow = Ro_max, ncol = Co_max)

Partitioning <- function(Nr, Start, Ro_in, Co_in) {
  for (Dir in 1:4) {
    Ro_step <- Ro_in + Steps$ro_add[Dir]
    Co_step <- Co_in + Steps$co_add[Dir]
    if (!((Ro_step < 1) | (Ro_step > Ro_max) | (Co_step < 1) | (Co_step > Co_max))) {
      if ((Examined[Ro_step, Co_step] == 0) & (data[Ro_step, Co_step] == Start)) {
        Examined[Ro_step, Co_step] <<- Nr
        Partitioning(Nr, Start, Ro_step, Co_step)
      } 
    }
  }
}

Count <- function(Nr) {
  Area <- data.frame(which(Examined == Nr, arr.ind = TRUE))
  Area$key <- Area$row * Ro_mul + Area$col
  Field <- nrow(Area)
  Fence <- 0
  Fence_part <- 0
  for (Dir in 1:4) {
    Area_New <- Area
    Area_New$row <- Area_New$row + Steps$ro_add[Dir]
    Area_New$col <- Area_New$col + Steps$co_add[Dir]
    Area_New$key <- Area_New$row * Ro_mul + Area_New$col
    Fence <- Fence + sum(!(Area_New$key %in% Area$key))
    Outside <- Area_New[!(Area_New$key %in% Area$key),]
    if (nrow(Outside) > 0) {
      if (Steps$ro_add[Dir] != 0) { # Investigation of rows
        for (Ro in unique(Outside$row)) {
          Col <- sort(Outside[Outside$row == Ro, "col"])
          Col <- diff(c(-Ro_max, Col, 2*Ro_max))
          Fence_part <- Fence_part + sum(Col > 1) - 1
        }
      }
      if (Steps$co_add[Dir] != 0) { # Investigation of rows
        for (Co in unique(Outside$col)) {
          Row <- sort(Outside[Outside$col == Co, "row"])
          Row <- diff(c(-Co_max, Row, 2*Co_max))
          Fence_part <- Fence_part + sum(Row > 1) - 1
        }
      }
    }
  }
  return(c(Field * Fence, Field * Fence_part))
}

Nr <- 0
Price <- 0
Price_part <- 0
for (Ro in 1:Ro_max) {
  for (Co in 1:Co_max) {
    if (Examined[Ro, Co] == 0) {
      Nr <- Nr + 1
      Examined[Ro, Co] <- Nr
      Partitioning(Nr, data[Ro, Co], Ro, Co)
      Count_Temp <- Count(Nr)
      Price <- Price + Count_Temp[1]
      Price_part <- Price_part + Count_Temp[2]
    }
  }
}
print(as.character(Price)) # Part 1: 1477924
print(as.character(Price_part)) # Part 1: 841934

