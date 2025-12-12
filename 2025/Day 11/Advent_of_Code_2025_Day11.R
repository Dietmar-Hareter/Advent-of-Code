setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library("memoise")

start_time <- Sys.time()
options(digits = 22L)

#data_orig <- readLines('input_day11_Test.txt')
#data_orig <- readLines('input_day11_Test2.txt')
data_orig <- readLines('input_day11.txt')

data <- do.call(rbind, strsplit(data_orig, ":"))
input <- data[,1]
output <- strsplit(trimws(gsub(" +", " ", data[,2])), " ")
names(output) <- data[,1]


# Part 1 + Part 2

connections <- memoise(function(input, end) {
  if (input == end) {
    return(1)
  }
  if (input == "out") {
    return(0)
  }
  sum(sapply(output[[input]], function(x) connections(x, end)))
})

print(paste0("Part 1: ", connections("you", "out")))

# There can only be connections from fft to dac, or only connections from dac to fft, as otherwise loops would occur.

test <- connections("fft", "dac") 
if (test > 0) {
  part_2 <- connections("svr", "fft") * test * connections("dac", "out") 
} else {
  part_2 <- connections("svr", "dac") * connections("dac", "fft") * connections("fft", "out")
}

print(paste0("Part 2: ", part_2))
print(paste0("Runtime in sec: ", round(as.numeric(Sys.time() - start_time, units = "secs"), 5)))