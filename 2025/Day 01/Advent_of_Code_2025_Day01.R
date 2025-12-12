setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

start_time <- Sys.time()

#data_orig <- readLines("input_day01_Test.txt")
data_orig <- readLines("input_day01.txt")

position <- 50
code_p1 <- 0
code_p2 <- 0

# Part 1 + 2

for (loop in 1:length(data_orig)) {
  instruction <- data_orig[loop]
  direction <- stringr::str_sub(instruction, start = 1, end = 1)
  number <- as.integer(stringr::str_sub(instruction, start = 2))
  prev_pos <- position
  position <- position + ifelse(direction == "R", 1, -1) * number
  
  if (position < prev_pos) {
    code_help <- sum((position:(prev_pos-1)) %% 100 == 0)
  }
  if (prev_pos < position) {
    code_help <- sum(((prev_pos+1):position) %% 100 == 0)
  }
  code_p2 <- code_p2 + code_help
  
  position <- position %% 100 
  if (position == 0) {
    code_p1 <- code_p1 + 1
  }
}

print(paste0("Part 1: ", code_p1))
print(paste0("Part 2: ", code_p2))

print(paste0("Runtime in sec: ", round(as.numeric(Sys.time() - start_time, units = "secs"), 5)))