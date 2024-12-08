setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data_input <- readLines('input_day08_Test.txt')
data_input <- readLines('input_day08.txt')

data <- NULL
for (i in seq_along(data_input)) {
  data <- rbind(data, unlist(strsplit(data_input[i], split = '')))
}

Ro_max <- nrow(data)
Co_max <- ncol(data)


# Part 1

Antenna <- matrix(0, nrow = Ro_max, ncol = Co_max)

Types <- unique(paste0(data))
Types <- Types[Types != "."]

for (Type in Types) {
  Fields <- which(data == Type, arr.ind = TRUE)
  for (First in 1:nrow(Fields)) {
    for (Second in 1:nrow(Fields)) {
      if (First != Second) {
        Ro <- Fields[First, 1] + 2 * (Fields[Second, 1] - Fields[First, 1])
        Co <- Fields[First, 2] + 2 * (Fields[Second, 2] - Fields[First, 2])
        if ((Ro >= 1) & (Ro <= Ro_max) & (Co >= 1) & (Co <= Co_max)) {
          Antenna[Ro, Co] <- 1
        }
        
      }
    }
  }
}
print(as.character(sum(Antenna))) # Part 1: 305


# Part 2

Antenna <- matrix(0, nrow = Ro_max, ncol = Co_max)

Types <- unique(paste0(data))
Types <- Types[Types != "."]

for (Type in Types) {
  Fields <- which(data == Type, arr.ind = TRUE)
  for (First in 1:nrow(Fields)) {
    for (Second in 1:nrow(Fields)) {
      if (First != Second) {
        In_Field <- 1
        Dist <- 0
        while (In_Field) {
          Ro <- Fields[First, 1] + Dist * (Fields[Second, 1] - Fields[First, 1])
          Co <- Fields[First, 2] + Dist * (Fields[Second, 2] - Fields[First, 2])
          if ((Ro >= 1) & (Ro <= Ro_max) & (Co >= 1) & (Co <= Co_max)) {
            Antenna[Ro, Co] <- 1
            Dist <- Dist + 1
          } else {
            In_Field <- 0
          }
        }
      }
    }
  }
}
print(as.character(sum(Antenna))) # Part 2: 1150
