setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

start_time <- Sys.time()
options(digits = 22L)

#data <- readLines('input_day06_Test.txt')
data <- readLines('input_day06.txt')
numbers_orig <- data[1:(length(data)-1)] 

numbers <- data.frame(lapply(data.frame(do.call(rbind, stringr::str_extract_all(numbers_orig, "\\d+"))), as.numeric))
operator <- unlist(strsplit(gsub(" ", "", data[length(data)]), split = ""))


# Part 1

total <- sum(sapply(1:length(operator), function(x) {
  if (operator[x] == "+") {
    sum(numbers[,x])
  } else {
    prod(numbers[,x])
  }
}))

print(paste0("Part 1: ", total))


# Part 2

number <- do.call(rbind, stringr::str_split(numbers_orig, ""))
sep_columns <- colSums(number == " ") == nrow(number)
parts <- cumsum(sep_columns) + 1
parts[sep_columns] <- 0

total <- 0
for (part in 1:max(parts)) {
  str_tmp <- number[, parts == part]
  num_vec <- NULL
  for (ic in ncol(str_tmp):1) {
    num_vec <- c(num_vec, as.numeric(gsub(" ", "", stringr::str_c(str_tmp[,ic], collapse = ""))))
  }
  if (operator[part] == "+") {
    total <- total + sum(num_vec)
  } else {
    total <- total + prod(num_vec)
  }
}

print(paste0("Part 2: ", total))

print(paste0("Runtime in sec: ", round(as.numeric(Sys.time() - start_time, units = "secs"), 5)))
