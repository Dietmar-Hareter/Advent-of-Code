setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

start_time <- Sys.time()
options(digits = 22L)

#data_orig <- readLines("input_day07_Test.txt")
data_orig <- readLines("input_day07.txt")

data <- do.call(rbind, stringr::str_split(data_orig, ""))

n_row <- nrow(data)
n_col <- ncol(data)

# Part 1

start <- which(data[1,] == "S")

sum_beams <- 0

beams <- rep(FALSE, n_col)
beams[start] <- TRUE

for (ir in 2:nrow(data)) {
  splits <- (data[ir,] == "^") & beams
  sum_beams <- sum_beams + sum(splits)
  if (sum(splits) > 0) {
    beams_split <- c(FALSE, splits[1:(n_col-1)]) | c(splits[2:n_col], FALSE) 
    beams <- beams | beams_split
    beams[splits] <- FALSE
  }
}

#beams <- matrix(0, nrow = n_row, ncol = n_col)
#beams[1, start] <- 1 

#for (ir in 2:n_row) {
#  for (ic in 1:n_col) {
#    field <- data[ir, ic]
#    beam <- beams[ir-1, ic]
#    
#    if (field != "^") {
#      if (beam == 1) {
#        beams[ir, ic] <- 1
#      }
#    }
#    
#    if (field == "^") {
#      if (beam == 1){
#        sum_beams <- sum_beams + 1
#        beams[ir, ic-1] <- 1
#        beams[ir, ic+1] <- 1
#      }
#    }
#  }
#}

print(paste0("Part 1: ", sum_beams))


# Part 2

timelines <- matrix(0, nrow = n_row, ncol = n_col)
timelines[1, start] <- 1 
  
for (ir in 2:n_row) {
  for (ic in 1:n_col) {
    field <- data[ir, ic]
    times <- timelines[ir-1, ic]
    
    if (field != "^") {
      if (times > 0) {
        timelines[ir, ic] <- timelines[ir, ic] + times
      }
    }
    
    if (field == "^") {
      if (times > 0){
        timelines[ir, ic-1] <- timelines[ir, ic-1] + times
        timelines[ir, ic+1] <- timelines[ir, ic+1] + times
      }
    }
  }
}

print(paste0("Part 2: ", sum(timelines[n_row,])))

print(paste0("Runtime in sec: ", round(as.numeric(Sys.time() - start_time, units = "secs"), 5)))
