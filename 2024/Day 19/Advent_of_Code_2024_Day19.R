setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data <- readLines("input_day19_Test.txt")
data <- readLines("input_day19.txt")
Split <- which(data == "")

Towels <- stringr::str_trim(unlist(stringr::str_split(data[1:(Split-1)], ",")))
Towels_Start <- paste0("^", Towels)
Pattern <- data[(Split+1):length(data)]


# Part 1 + Part 2

check <- memoise::memoise(function(str_in, Towels_test) {
  if (nchar(str_in) == 0) {
    return(1)
  }
  Test <- Towels_test[stringr::str_detect(str_in, Towels_test)]
  if(length(Test) == 0) {
    return(0)
  }
  Possible <- 0
  for (Towel in Test) {
    Possible <- Possible + check(stringr::str_remove(str_in, Towel), Towels_test)
  }
  return(Possible)
})

Results <- NULL
for (Pat in seq_along(Pattern)) {
  if (Pat %% 10 == 0) 
    print(paste0("Analyze ", Pat, " from ", length(Pattern)))
  Towels_test <- Towels_Start[stringr::str_detect(Pattern[Pat], Towels)]
  Results <- c(Results, check(Pattern[Pat], Towels_test))        
}

print(sum(Results > 0)) # Part 1: 324
print(as.character(sum(Results))) # Part 2: 575227823167869
