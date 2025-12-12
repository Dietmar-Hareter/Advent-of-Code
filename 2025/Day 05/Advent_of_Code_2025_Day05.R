setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

start_time <- Sys.time()
options(digits = 22L)

#data <- readLines('input_day05_Test.txt')
data <- readLines('input_day05.txt')
split <- which(data == "")

fresh_id <- data.frame(lapply(data.frame(do.call(rbind, strsplit(data[1:(split-1)], split = "-"))), as.numeric))
ingred <- as.numeric(data[(split+1):length(data)])
fresh_list <- NULL

# Part 1

for (id in 1:nrow(fresh_id)) {
  fresh_ingred <- ingred[ingred >= fresh_id[id,1] & ingred <= fresh_id[id,2]]
  new_ingred <- setdiff(fresh_ingred, fresh_list)
  fresh_list <- c(fresh_list, new_ingred)
}

available  <- length(fresh_list)
print(paste0("Part 1: ", available))


# Part 2

reduce_intervals <- function(fresh_list) {
  starts <- fresh_list[,1]
  ends <- fresh_list[,2]
  
  ord <- order(starts, ends)
  starts <- starts[ord]
  ends   <- ends[ord]
  
  out_starts <- NULL
  out_ends   <- NULL
  
  cur_start <- starts[1]
  cur_end   <- ends[1]
  
  for (ir in 2:length(starts)) {
    start_tmp <- starts[ir]
    end_tmp <- ends[ir]
    
    if (start_tmp <= cur_end + 1) { # Interval overlaps or touches the current one
      cur_end <- max(cur_end, end_tmp)
    } else {
      # Save completed interval
      out_starts <- c(out_starts, cur_start)
      out_ends   <- c(out_ends, cur_end)
      
      # Start new interval
      cur_start <- start_tmp
      cur_end   <- end_tmp
    }
  }
  
  # Save last interval
  out_starts <- c(out_starts, cur_start)
  out_ends   <- c(out_ends, cur_end)
  
  return(data.frame(start = out_starts, end = out_ends))
}

available <- 0
part_list <- reduce_intervals(fresh_id)

for (id in 1:nrow(part_list)) {
  available <- available + part_list[id,2] - part_list[id,1] + 1
}

print(paste0("Part 2: ", available))

print(paste0("Runtime in sec: ", round(as.numeric(Sys.time() - start_time, units = "secs"), 5)))
