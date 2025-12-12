setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

start_time <- Sys.time()

#data_orig <- readLines("input_day02_Test.txt")
data_orig <- readLines("input_day02.txt")

data <- unlist(stringr::str_split(stringr::str_c(data_orig, collapse = ""), ","))
options(digits = 22L)


# Part 1

inval_id <- 0

for (id in data) {
  range <- as.numeric(unlist(stringr::str_split(id, "-")))
  ids_num <- range[1]:range[2]
  ids <- as.character(ids_num)
  tot_len <- stringr::str_length(ids)
  mid <- tot_len %/% 2
  left <- stringr::str_sub(ids, 1, mid)
  right <- stringr::str_sub(ids, mid + 1, tot_len)
  inval_id <- inval_id + sum(ids_num[left == right])
}

print(paste0("Part 1: ", inval_id))


# Part 2

inval_id <- 0

for (id in data) {
  print(id)
  range <- as.numeric(unlist(stringr::str_split(id, "-")))
  ids_num <- range[1]:range[2]
  ids <- as.character(ids_num)
  tot_len_all <- stringr::str_length(ids)
  
  invalid_all <- NULL
  for (tot_len in sort(unique(tot_len_all))) {
    ids_part <- ids[tot_len_all == tot_len]
    # Find a divisor of tot_len
    divisors <- which(tot_len %% 1:tot_len == 0)
    divisors <- divisors[-1]
    
    invalid <- rep(FALSE, length(ids_part))
    for (divisor in divisors) {
#      print(divisor)
      size <- tot_len %/% divisor
      start <- seq(1, tot_len, size)
      if (tot_len == 1) {
        end <- start
      } else {
        end <- seq(size, tot_len, size)
      }
      
      part <- NULL
      for (sp in 1:length(start)) {
        part <- cbind(part, stringr::str_sub(ids_part, start[sp], end[sp]))
      }
      row_all_equal <- apply(part, 1, function(x) length(unique(x)) == 1)
      invalid <- invalid | row_all_equal
    }
    invalid_all <- c(invalid_all, invalid)
  }
  inval_id <- inval_id + sum(ids_num[invalid_all])
}

print(paste0("Part 2: ", inval_id))

print(paste0("Runtime in sec: ", round(as.numeric(Sys.time() - start_time, units = "secs"), 5)))