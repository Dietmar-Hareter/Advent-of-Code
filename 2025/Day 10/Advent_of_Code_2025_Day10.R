setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(lpSolve) # for part 2

start_time <- Sys.time()
#options(digits = 22L)

#data_orig <- readLines('input_day10_Test.txt')
data_orig <- readLines('input_day10.txt')


# Part 1 + Part 2

to_Binary <- function(x, bits = 6) {
  bin <- rev(as.integer(intToBits(x))[1:bits])
  return(bin)
}

press_part_1 <- 0
press_part_2 <- 0

for (ir in 1:length(data_orig)) {
  data <- unlist(strsplit(data_orig[ir], "[]|{]"))

  diagram_orig <- gsub("#", "1", gsub("\\.", "0", gsub("\\[", "", data[1])))
  diagram <- as.integer(unlist(strsplit(diagram_orig, "")))
  
  schema_orig <- data[2]
  schema <- unlist(stringr::str_extract_all(schema_orig, "(\\d+,)*\\d+"))
  schema <- lapply(schema, function(x) {as.integer(unlist(stringr::str_extract_all(x, "\\d+"))) + 1})
  
  requi_orig <- gsub("}", "", data[3])
  requi <- as.integer(unlist(strsplit(requi_orig, ",")))
  
  mat <- matrix(0, nrow = length(schema), ncol = length(diagram))
  
  for (ir in 1:length(schema)) {
    mat[ir, schema[[ir]]] <- 1
  }
  
  num_comb <- 2**nrow(mat)
  
  binary_matrix <- t(sapply(1:num_comb, to_Binary, bits = nrow(mat)))
  
  comb <- (binary_matrix %*% mat) %% 2
  
  match_rows <- apply(comb, 1, function(x) all(x == diagram))
  
  if (sum(match_rows) == 1) {
    press_part_1 <- press_part_1 + sum(binary_matrix[match_rows,])
  } else {
    press_part_1 <- press_part_1 + min(rowSums(binary_matrix[match_rows,]))
  }
  
  A <- t(mat)
  n <- ncol(A)
  press <- lp(
    direction = "min",
    objective.in = rep(1, n),
    const.mat = A,
    const.dir = rep("=", nrow(A)),    
    const.rhs = requi,
    all.int = TRUE
  )
  
  press_part_2 <- press_part_2 + sum(press$solution)
}

print(paste0("Part 1: ", press_part_1))
print(paste0("Part 2: ", press_part_2))

print(paste0("Runtime in sec: ", round(as.numeric(Sys.time() - start_time, units = "secs"), 5)))
