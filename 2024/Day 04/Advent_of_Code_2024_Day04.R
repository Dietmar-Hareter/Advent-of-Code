setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data_orig <- readLines('input_day04_Test.txt')
data_orig <- readLines('input_day04.txt')

data <- NULL
for (i in seq_along(data_orig)) {
  data <- rbind(data, unlist(strsplit(data_orig[i], split = '')))
}

Ro_max <- nrow(data)
Co_max <- ncol(data)


# Part 1

Total <- 0
for (Ro in 1:Ro_max) {
  for (Co in 1:Co_max) {
    if (data[Ro, Co] == "X") { # Only fields with “X” are examined
      for (Ro_step in c(-1,0,1)) {
        for (Co_step in c(-1,0,1)) {
          if ((Ro + Ro_step * 3 >= 1) & (Ro + Ro_step * 3 <= Ro_max) & (Co + Co_step * 3 >= 1) & (Co + Co_step * 3 <= Co_max)) {
            Ro_akt <- Ro + Ro_step * 0:3
            Co_akt <- Co + Co_step * 0:3
            if (identical(data[cbind(Ro_akt, Co_akt)], c("X", "M", "A", "S"))) {
              Total <- Total + 1
            }
          }
        }
      }
    } 
  }
}
print(Total) # Part 1: 2406


# Part 2

Total <- 0
for (Ro in 2:(Ro_max-1)) {
  for (Co in 2:(Co_max-1)) {
    if (data[Ro, Co] == "A") { # Only fields with “A” are examined
      Anz <- 0
      if ((data[Ro - 1, Co - 1] == "M") & (data[Ro + 1, Co + 1] == "S")) {Anz <- Anz + 1}
      if ((data[Ro - 1, Co + 1] == "M") & (data[Ro + 1, Co - 1] == "S")) {Anz <- Anz + 1}
      if ((data[Ro + 1, Co - 1] == "M") & (data[Ro - 1, Co + 1] == "S")) {Anz <- Anz + 1}
      if ((data[Ro + 1, Co + 1] == "M") & (data[Ro - 1, Co - 1] == "S")) {Anz <- Anz + 1}
      if (Anz == 2) {
        Total <- Total + 1
      }
    } 
  }
}
print(Total) # Part 2: 1807
