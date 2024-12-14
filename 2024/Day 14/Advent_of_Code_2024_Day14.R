setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Test <- FALSE
if (Test) {
  data_orig <- readLines("input_day14_Test.txt")
} else {
  data_orig <- readLines("input_day14.txt")
}
#options(digits = 22L)

data <- NULL
for (ro in seq_along(data_orig)) {
  data <- rbind(data, as.numeric(unlist(stringr::str_extract_all(data_orig[ro], "-?\\d+"))))
}
data <- data.frame(data)
colnames(data) <- c("x", "y", "x_v", "y_v")

if (Test) {
  wide <- 11 
  tall <- 7 
} else {
  wide <- 101 
  tall <- 103 
  
}
wide_half <- wide %/% 2
tall_half <- tall %/% 2


# Part 1 

data$x_end <- (data$x + 100 * data$x_v) %% wide
data$y_end <- (data$y + 100 * data$y_v) %% tall

Q1 <- sum((data$x_end < wide_half) & (data$y_end < tall_half))
Q2 <- sum((data$x_end < wide_half) & (data$y_end > tall_half))
Q3 <- sum((data$x_end > wide_half) & (data$y_end > tall_half))
Q4 <- sum((data$x_end > wide_half) & (data$y_end < tall_half))
Safe_factor <- Q1 * Q2 * Q3 * Q4

print(as.character(Safe_factor))# Part 1: 219512160


# Part 2

Show <- function(data_in) {
  Picture <- matrix(0, nrow = tall, ncol = wide)
  Picture[as.matrix(data_in[,c(6,5)])+1] <- 1
  Picture_Text <- NULL
  for (Ro in 1:nrow(Picture)) {
    Picture_Text <- c(Picture_Text, stringr::str_c(Picture[Ro,], collapse = ""))
  }
  Picture_Text <- stringr::str_replace_all(Picture_Text, "0", " ")
  return(Picture_Text)
}

Tree_search_guess <- function(data_in, Test_String) {
  Picture <- matrix(0, nrow = tall, ncol = wide)
  Picture[as.matrix(data_in[,c(6,5)])+1] <- 1
  Is_Tree <- stringr::str_detect(stringr::str_c(Picture, collapse = ""), Test_String)
  
  return(Is_Tree)
}
  
Test_String <- "11111111111111111111"
Tree_search <- TRUE
Seconds <- 0
while (Tree_search & (Seconds <= 10000)) {
  Seconds <- Seconds + 1
  if (Seconds %% 100 == 0) {
    print(Seconds)
  }
  data$x_end <- (data$x + Seconds * data$x_v) %% wide
  data$y_end <- (data$y + Seconds * data$y_v) %% tall
  if (Tree_search_guess(data, Test_String)) {
    Tree_search <- FALSE
    print(Show(data))
  }
}
print(Seconds) # Part 2: 6398
