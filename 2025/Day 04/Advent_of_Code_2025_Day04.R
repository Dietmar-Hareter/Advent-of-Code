setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

start_time <- Sys.time()

#data_orig <- readLines('input_day04_Test.txt')
data_orig <- readLines('input_day04.txt')

data <- do.call(rbind, strsplit(data_orig, split = ""))
data <- ifelse(data == "@", 1L, 0L)

nr <- nrow(data)
nc <- ncol(data)

data_edge <- cbind(0, rbind(0, data, 0), 0)


# Part 1

paper <- data * 0 + 9

for (ir in 1:nr) {
  for (ic in 1:nc) {
    if (data_edge[ir+1, ic+1] == 1) {
      kernel <- data_edge[ir:(ir+2), ic:(ic+2)]
      paper[ir, ic] <- sum(kernel) - 1
    }
  }
}

removed <- sum(paper <= 3)
print(paste0("Part 1: ", removed))


# Part 2

total_removed <- 0
data_removed <- data
removed <- 1

while(removed > 0) {
  data_edge <- cbind(0, rbind(0, data_removed, 0), 0)
  paper <- data_removed * 0 + 9
  
  for (ir in 1:nr) {
    for (ic in 1:nc) {
      if (data_edge[ir+1, ic+1] == 1) {
        kernel <- data_edge[ir:(ir+2), ic:(ic+2)]
        paper[ir, ic] <- sum(kernel) - 1
      }
    }
  }
  
  removed <- sum(paper <= 3)
  data_removed[paper <= 3] <- 0
  total_removed <- total_removed + removed
}

print(paste0("Part 2: ", total_removed))

print(paste0("Runtime in sec: ", round(as.numeric(Sys.time() - start_time, units = "secs"), 5)))