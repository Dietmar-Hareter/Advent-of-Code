setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data_input <- readLines('input_day10_Test.txt')
data_input <- readLines('input_day10.txt')

data <- NULL
for (Lauf in seq_along(data_input)) {
  data <- rbind(data, as.numeric(stringr::str_split_1(data_input[Lauf], "")))
}

Ro_max <- nrow(data)
Co_max <- ncol(data)

Steps <- data.frame(
  Direction = 1:4,
  ro_add = c(-1,0,1,0),
  co_add = c(0,1,0,-1))

# Part 1

Trail <- function(Act_Num, Ro_in, Co_in, Dir) {
  Ro_step <- Ro_in + Steps$ro_add[Dir]
  Co_step <- Co_in + Steps$co_add[Dir]
  if ((Ro_step < 1) | (Ro_step > Ro_max) | (Co_step < 1) | (Co_step > Co_max)) {
    return(" ")
  } else {
    if ((Act_Num == 8) & (data[Ro_step, Co_step] == 9)) {
      return(paste0(Ro_step, "-", Co_step))
    } else if (data[Ro_step, Co_step] == Act_Num + 1) {
      return(c(Trail(Act_Num + 1, Ro_step, Co_step, 1),
               Trail(Act_Num + 1, Ro_step, Co_step, 2),
               Trail(Act_Num + 1, Ro_step, Co_step, 3),
               Trail(Act_Num + 1, Ro_step, Co_step, 4)))
    } else {
      return(" ")
    }
  }
}

Score_sum <- 0
for (Ro in 1:Ro_max) {
  for (Co in 1:Co_max) {
    if (data[Ro, Co] == 0) {
      Score <- unique(c(Trail(0, Ro, Co, 1), Trail(0, Ro, Co, 2), Trail(0, Ro, Co, 3), Trail(0, Ro, Co, 4)))
      Score <- Score[-which(Score == " ")]
      Score_sum <- Score_sum + length(Score)
    }
  }
}
print(as.character(Score_sum)) # Part 1: 514


# Part 2

Trail_num <- function(Act_Num, Ro_in, Co_in, Dir) {
  Ro_step <- Ro_in + Steps$ro_add[Dir]
  Co_step <- Co_in + Steps$co_add[Dir]
  if ((Ro_step < 1) | (Ro_step > Ro_max) | (Co_step < 1) | (Co_step > Co_max)) {
    return(0)
  } else {
    if ((Act_Num == 8) & (data[Ro_step, Co_step] == 9)) {
      return(1)
    } else if (data[Ro_step, Co_step] == Act_Num + 1) {
      return(Trail_num(Act_Num + 1, Ro_step, Co_step, 1) +
               Trail_num(Act_Num + 1, Ro_step, Co_step, 2) +
               Trail_num(Act_Num + 1, Ro_step, Co_step, 3) +
               Trail_num(Act_Num + 1, Ro_step, Co_step, 4))
    } else {
      return(0)
    }
  }
}

Score_sum <- 0
for (Ro in 1:Ro_max) {
  for (Co in 1:Co_max) {
    if (data[Ro, Co] == 0) {
      Score <- Trail_num(0, Ro, Co, 1) + Trail_num(0, Ro, Co, 2) + Trail_num(0, Ro, Co, 3) + Trail_num(0, Ro, Co, 4)
      Score_sum <- Score_sum + Score
    }
  }
}
print(as.character(Score_sum)) # Part 2: 1162



