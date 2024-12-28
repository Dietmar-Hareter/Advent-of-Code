setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data <- readLines('input_day22_Test.txt')
data <- readLines('input_day22.txt')


# Part 1 + Part 2

Step <- function(Value) {
  Value <- bitwXor(Value %% 16777216, (Value * 64) %% 16777216)
  Value <- bitwXor(Value %% 16777216, (Value %/% 32) %% 16777216)
  Value <- bitwXor(Value %% 16777216, (Value * 2048) %% 16777216)
  return(Value)
}

Total <- 0
Seq_Total <- list()

for(Secret in 1:length(data)) {
  if (Secret %% 10 == 0) {
    print(paste0("Analyze ", Secret, " in ", length(data)))
  }
  Buyer <- vector("numeric", length = 2001)
  Secret_num <- as.integer(data[Secret]) 
  Buyer[1] <- Secret_num %% 10
  for (Run in 1:2000) {
    Secret_num <- Step(Secret_num)
    Buyer[Run+1] <- Secret_num %% 10
  }
  Total <- Total + Secret_num
  
  Buyer_diff <- diff(Buyer)

  Seq_Secret <- vector("character", length = 2000)
  for (Run in 1:(length(Buyer_diff) - 3)) {
    Temp <- paste0(Buyer_diff[Run:(Run+3)], collapse = ",")   

    if (Temp %in% Seq_Secret) {
      next
    }
    Seq_Secret <- c(Seq_Secret, Temp)

    if (!(Temp %in% names(Seq_Total))) {
      Seq_Total[[Temp]] <- 0
    }
    Seq_Total[[Temp]] <- Seq_Total[[Temp]] + Buyer[Run+4]
  }
}

print(Total) # Part 1: 20068964552

Max_Value <- max(unlist(Seq_Total))
print(Max_Value) # Part 2: 2246
