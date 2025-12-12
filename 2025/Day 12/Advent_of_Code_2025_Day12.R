setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

start_time <- Sys.time()
#options(digits = 22L)

#data_orig <- readLines('input_day12_Test.txt')
data_orig <- readLines('input_day12.txt')

forms <- list()
forms_size <- NULL
for (ir in 1:6) {
  name <- gsub("\\:", "", data_orig[1 +(ir-1)*5])
  tmp <- gsub("\\.", "0", gsub("#", "1", data_orig[2:4 +(ir-1)*5]))
  forms[[name]] <- data.frame(lapply(data.frame(do.call(rbind, stringr::str_split(tmp, ""))), as.integer))
  forms_size <- c(forms_size, sum(forms[[name]]))
}

shapes <- data_orig[31:length(data_orig)]


# Part 1

sum_can_pack <- 0

for (ir in 1:length(shapes)) {
  tmp <- unlist(strsplit(shapes[ir], ":"))
  board_size <- prod(as.integer(unlist(stringr::str_split(tmp[1], "x"))))
  piece_list <- as.integer(unlist(stringr::str_extract_all(tmp[2], "\\d+")))
  
  piece_size <- sum(forms_size * piece_list)
  # A solution is only possible if less space is needed for the gifts than is available.
  if (piece_size <= board_size) { 
    num_piece <- sum(piece_list)
    # Lists all cases where no simple solution (placing all gifts on a separate 3x3 field) is possible.
    if (board_size %/% 9 < num_piece) {
      print(c(ir, num_piece - board_size %/% 9, round(piece_size / board_size * 100, 2)))
      # => None of these cases occur.
    } else {
      sum_can_pack <- sum_can_pack + 1
    }
  }
}

print(paste0("Part 1: ", sum_can_pack))

print(paste0("Runtime in sec: ", round(as.numeric(Sys.time() - start_time, units = "secs"), 5)))

