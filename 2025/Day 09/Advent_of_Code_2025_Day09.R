setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

start_time <- Sys.time()
#options(digits = 22L)

#data_orig <- readLines('input_day09_Test.txt')
data_orig <- readLines('input_day09.txt')

data <- data.frame(lapply(data.frame(do.call(rbind, stringr::str_split(data_orig, ","))), as.numeric))
colnames(data) <- c("x", "y")
n_row <- nrow(data)

data_nr <- cbind(nr = 1:n_row, data)

comb <- dplyr::cross_join(data_nr, data_nr, suffix = c("_1", "_2"))
comb <- comb[comb[,"nr_1"] < comb[,"nr_2"],]


# Part 1

comb$area <- (abs(comb$x_1 - comb$x_2) + 1) * (abs(comb$y_1 - comb$y_2) + 1)
area <- max(comb$area)

print(paste0("Part 1: ", area))


# Part 2

# In order to examine smaller dimensions, the individual x and y values are mapped to values of 1:dim.
x_unique <- sort(unique(data[, "x"]))
y_unique <- sort(unique(data[, "y"]))

# Define the mapping
x_map <- setNames(1:length(x_unique), x_unique)
y_map <- setNames(1:length(y_unique), y_unique)

data_map <- cbind(
  x = unname(x_map[as.character(data[, "x"])]),
  y = unname(y_map[as.character(data[, "y"])])
)
data_map <- rbind(data_map, data_map[1,])

x_max <- max(data_map[, 1])
y_max <- max(data_map[, 2])

#  min and max x-value for each y-coordinate
shape_x_seen_from_y <- data.frame(cbind(min = rep(0, y_max), max = rep(x_max, y_max)))

# min and max y-value for each x-coordinate
shape_y_seen_from_x <- data.frame(cbind(min = rep(0, x_max), max = rep(y_max, x_max)))

# shape of arrays
for (ir in 1:n_row) {
  x1 <- data_map[ir, 1]
  y1 <- data_map[ir, 2]
  x2 <- data_map[ir+1, 1]  
  y2 <- data_map[ir+1, 2]
  
  if (x1 == x2) {  # vertical linie
    if (y1 > y2) {
      tmp <- y1 
      y1 <- y2 
      y2 <- tmp
    }
    idx <- (y1:y2)
    
    shape_x_seen_from_y$max[idx] <- pmin(shape_x_seen_from_y$max[idx], x1)
    shape_x_seen_from_y$min[idx] <- pmax(shape_x_seen_from_y$min[idx], x1)
  } else if (y1 == y2) {  # horizontally line
    if (x1 > x2) {
      tmp <- x1
      x1 <- x2
      x2 <- tmp
    }
    idx <- (x1:x2)
    
    shape_y_seen_from_x$max[idx] <- pmin(shape_y_seen_from_x$max[idx], y1)
    shape_y_seen_from_x$min[idx] <- pmax(shape_y_seen_from_x$min[idx], y1)    
  }
}

max_area <- 0

for (ir in 1:n_row) {
  for (ir2 in (ir+1):n_row) {
    if (ir2 > n_row) {
      break
    }
    
    x1 <- data_map[ir, 1]
    y1 <- data_map[ir, 2]
    x2 <- data_map[ir2, 1]
    y2 <- data_map[ir2, 2]
    
    if (x1 > x2) {
      tmp <- x1
      x1 <- x2
      x2 <- tmp
    }
    if (y1 > y2) {
      tmp <- y1
      y1 <- y2
      y2 <- tmp
    }
    
    y_idx <- (y1:y2)
    x_idx <- (x1:x2)
    
    if (all(shape_x_seen_from_y$max[y_idx] <= x1) && all(shape_x_seen_from_y$min[y_idx] >= x2) &&
        all(shape_y_seen_from_x$max[x_idx] <= y1) && all(shape_y_seen_from_x$min[x_idx] >= y2)) {  
      area <- (x_unique[x2] - x_unique[x1] + 1) * (y_unique[y2] - y_unique[y1] + 1)
      
      if (area > max_area) {
        max_area <- area
      }
    }
  }
}

print(paste0("Part 2: ", max_area))

print(paste0("Runtime in sec: ", round(as.numeric(Sys.time() - start_time, units = "secs"), 5)))
