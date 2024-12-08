setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data_input <- readLines('input_day06_Test.txt')
data_input <- readLines('input_day06.txt')

data_orig <- NULL
for (i in seq_along(data_input)) {
  data_orig <- rbind(data_orig, unlist(strsplit(data_input[i], split = '')))
}

data_orig[data_orig == "^"] <- "1"

Ro_max <- nrow(data_orig)
Co_max <- ncol(data_orig)

Start_num <- which(data_orig == "1")
start <- as.vector(which(data_orig == "1", arr.ind = TRUE))

Schritt <- data.frame(
  Richtung = 1:4,
  ro_add = c(-1,0,1,0),
  co_add = c(0,1,0,-1))


# Part 1

Ablauf <- function(data, Ro, Co) {
  Richtung <- 1
  AmFeld <- 1
  Schritte <- 0
  Max_Schritte <- 10000
  Schleife <- 0
  Weg <- Ro * 10000 + Co * 10 + Richtung
  
  while((AmFeld == 1) & (Schleife == 0) & (Schritte < Max_Schritte)) {
    Frei <- 0
    while(Frei == 0) {
      ro_add <- Schritt$ro_add[Richtung]
      co_add <- Schritt$co_add[Richtung]
      if ((Ro + Schritt$ro_add[Richtung] < 1) | (Ro + Schritt$ro_add[Richtung] > Ro_max) | 
          (Co + Schritt$co_add[Richtung] < 1) | (Co + Schritt$co_add[Richtung] > Co_max)) {
        Frei <- 1
      } else if (data[Ro + ro_add, Co + co_add] != "#") {
        Frei <- 1
      } else {
        Richtung <- Richtung + 1
        if (Richtung == 5) {
          Richtung <- 1
        }
      }
    }
    Ro <- Ro + Schritt$ro_add[Richtung]
    Co <- Co + Schritt$co_add[Richtung]
    if ((Ro < 1) | (Ro > Ro_max) | (Co < 1) | (Co > Co_max)) {
      AmFeld <- 0
    } else {
      data[Ro, Co] <- "1"
      Schritte <- Schritte + 1
      Weg <- c(Weg, Ro * 10000 + Co * 10 + Richtung)
    }
    if (sum(duplicated(Weg)) > 0) {
      Schleife <- 1
    }
  }
  
  Rueckgabe <- list()
  Rueckgabe$Felder <- sum(data == "1")
  Rueckgabe$Felder_num <- (1:length(data))[data == "1"]
  Rueckgabe$Schleife <- Schleife
  Rueckgabe$Weg <- Weg
  
  return(Rueckgabe) 
}

Info <- Ablauf(data = data_orig, Ro = start[1], Co = start[2])
print(Info$Felder) # Part 1: 4696


# Part 2

Felder_num <- Info$Felder_num
Anzahl_Schleifen <- 0
for (Lauf in 1:length(Felder_num)) {
  if (Lauf %% 10 == 0) {
    print(Lauf)
  }
  if (Felder_num[Lauf] != Start_num) {
    data_temp <- data_orig
    data_temp[Felder_num[Lauf]] <- "#"
    Temp <- Ablauf(data = data_temp, Ro = start[1], Co = start[2])
    Anzahl_Schleifen <- Anzahl_Schleifen + Temp$Schleife
  }
}
print(Anzahl_Schleifen) # Part 2: 1443
