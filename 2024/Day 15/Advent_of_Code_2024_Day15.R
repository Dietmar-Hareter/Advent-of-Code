setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data <- readLines('input_day15_Test.txt')
data <- readLines('input_day15.txt')
Split <- which(data == "")

Field_orig <- data[1:(Split-1)]
Field_Start <- NULL
for (Ro in 1:length(Field_orig)) {
  Field_Start <- rbind(Field_Start, unlist(strsplit(Field_orig[Ro], split = '')))
}
Temp <- stringr::str_c(data[(Split+1):length(data)], collapse = "")
Temp <- stringr::str_replace_all(Temp, "\\^", "1")
Temp <- stringr::str_replace_all(Temp, ">", "2")
Temp <- stringr::str_replace_all(Temp, "v", "3")
Temp <- stringr::str_replace_all(Temp, "<", "4")
Directions <- as.numeric(unlist(strsplit(Temp, split = '')))

Field_Start_2 <- NULL
for (Ro in 1:length(Field_orig)) {
  Temp <- unlist(strsplit(Field_orig[Ro], split = ''))
  Temp <- stringr::str_replace_all(Temp, "#", "##")
  Temp <- stringr::str_replace_all(Temp, "O", "[]")
  Temp <- stringr::str_replace_all(Temp, "\\.", "..")
  Temp <- stringr::str_replace_all(Temp, "@", "@.")
  Temp <- stringr::str_c(Temp, collapse = "")
  Field_Start_2 <- rbind(Field_Start_2, unlist(strsplit(Temp, split = '')))
}

Steps <- data.frame(
  Direction = 1:4,
  ro_add = c(-1,0,1,0),
  co_add = c(0,1,0,-1))


# Part 1

Field <- Field_Start
Start <- as.vector(which(Field == "@", arr.ind = TRUE))
Ro <- Start[1]
Co <- Start[2]

Check_move <- function(Deep, Ro_in, Co_in, Ro_add, Co_add) {
  Ro_new <- Ro_in + Ro_add
  Co_new <- Co_in + Co_add
  
  if (Field[Ro_new, Co_new] == "#") {
    return(c(Deep, -1, -1, -1, -1))
  }
  if (Field[Ro_new, Co_new] == ".") {
    return(c(Deep, Ro_in, Co_in, Ro_new, Co_new))
  } 
  return(rbind(c(Deep, Ro_in, Co_in, Ro_new, Co_new),
               Check_move(Deep + 1, Ro_new, Co_new, Ro_add, Co_add)))
}

for (Dir in Directions) {
  Ro_add <- Steps$ro_add[Dir]
  Co_add <- Steps$co_add[Dir]
  
  Temp <- Check_move(1, Ro, Co, Ro_add, Co_add)
  if (sum(Temp == -1) == 0) {
    if (is.null(dim(Temp))) {
       Field[Temp[4], Temp[5]] <- Field[Temp[2], Temp[3]] 
       Field[Ro, Co] <- "." 
    } else {
      for (Lauf in nrow(Temp):1) {
        Field[Temp[Lauf, 4], Temp[Lauf, 5]] <- Field[Temp[Lauf, 2], Temp[Lauf, 3]] 
      }
      Field[Ro, Co] <- "." 
    }
    Ro <- Ro + Ro_add
    Co <- Co + Co_add
  }
}

Pos <- which(Field == "O", arr.ind = TRUE) - 1
print(sum(Pos[,1] * 100 + Pos[,2])) # Part 1: 


# Part 2

Field <- Field_Start_2
Start <- as.vector(which(Field == "@", arr.ind = TRUE))
Ro <- Start[1]
Co <- Start[2]

Check_move_2 <- function(Deep, Ro_in, Co_in, Ro_add, Co_add) {
  Ro_new <- Ro_in + Ro_add
  Co_new <- Co_in + Co_add
  if (Field[Ro_new, Co_new] == "#") {
    return(c(Deep, -1, -1, -1, -1))
  } 
  if (Field[Ro_new, Co_new] == ".") {
    return(c(Deep, Ro_in, Co_in, Ro_new, Co_new))
  } 
  if (Ro_add == 0) { # Shift in row
    return(rbind(c(Deep, Ro_in, Co_in, Ro_new, Co_new),
                 Check_move_2(Deep + 1, Ro_new, Co_new, Ro_add, Co_add)))
  } 
  if (Field[Ro_new, Co_new] == "[") {
    return(rbind(c(Deep, Ro_in, Co_in, Ro_new, Co_new),
                 Check_move_2(Deep + 1, Ro_new, Co_new+1, Ro_add, Co_add),
                 Check_move_2(Deep + 1, Ro_new, Co_new, Ro_add, Co_add)))
  } 
#  print(rbind(c(Deep, Ro_in, Co_in, Ro_new, Co_new),
#              Check_move_2(Deep + 1, Ro_new, Co_new-1, Ro_add, Co_add),
#              Check_move_2(Deep + 1, Ro_new, Co_new, Ro_add, Co_add)))
  return(rbind(c(Deep, Ro_in, Co_in, Ro_new, Co_new),
               Check_move_2(Deep + 1, Ro_new, Co_new-1, Ro_add, Co_add),
               Check_move_2(Deep + 1, Ro_new, Co_new, Ro_add, Co_add)))
}

for (Dir in Directions) {
  Ro_add <- Steps$ro_add[Dir]
  Co_add <- Steps$co_add[Dir]
  
  Temp <- Check_move_2(1, Ro, Co, Ro_add, Co_add)
#  print(Temp)
  if (sum(Temp == -1) == 0) {
    if (is.null(dim(Temp))) {
      Field[Temp[4], Temp[5]] <- Field[Temp[2], Temp[3]] 
      Field[Ro, Co] <- "." 
    } else {
      Temp <- unique(Temp)
      Temp <- Temp[order(Temp[,1]),]
      for (Lauf in nrow(Temp):1) {
        Field[Temp[Lauf, 4], Temp[Lauf, 5]] <- Field[Temp[Lauf, 2], Temp[Lauf, 3]]
        Field[Temp[Lauf, 2], Temp[Lauf, 3]] <- "."
      }
      Field[Ro, Co] <- "." 
    }
    Ro <- Ro + Ro_add
    Co <- Co + Co_add
  }
}

Pos_L <- which(Field == "[", arr.ind = TRUE) - 1
print(sum(Pos_L[,1] * 100 + Pos_L[,2])) # Part 2: 1432781
