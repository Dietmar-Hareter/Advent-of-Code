setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

start_time <- Sys.time()

#data <- readLines("input_day03_Test.txt")
data <- readLines("input_day03.txt")
options(digits = 22L)

# Part 1 + 2

check_bank <- function(bank, digits) {
  max_vec <- NULL
  if (digits > 0) {
    candidates <- bank[1:(length(bank)-digits+1)]
    first_max <- which.max(candidates)
    max_vec <- c(max_vec, bank[first_max], check_bank(bank[(first_max+1):length(bank)], digits = digits - 1))
  }
  return(max_vec)
}

sum_voltage <- function(digits) {
  joltage <- 0
  
  for (bank in data) {
    num <- as.integer(stringr::str_split(bank, "", simplify = TRUE))
    tmp <- check_bank(num, digits)
    joltage <- joltage + as.numeric(stringr::str_c(tmp, collapse = ""))
  }
  
  return(joltage)
}

print(paste0("Part 1: ", sum_voltage(2)))
print(paste0("Part 2: ", sum_voltage(12)))

print(paste0("Runtime in sec: ", round(as.numeric(Sys.time() - start_time, units = "secs"), 5)))