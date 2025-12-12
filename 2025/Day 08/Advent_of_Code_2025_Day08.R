setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

start_time <- Sys.time()
#options(digits = 22L)

#data_orig <- readLines('input_day08_Test.txt')
data_orig <- readLines('input_day08.txt')

data <- data.frame(lapply(data.frame(do.call(rbind, strsplit(data_orig, ","))), as.numeric))

dist_all <- dist(data)
dist_mat <- as.matrix(dist_all)
df_dist <- data.frame(
  x = row(dist_mat)[upper.tri(dist_mat)],
  y = col(dist_mat)[upper.tri(dist_mat)],
  dist = dist_mat[upper.tri(dist_mat)]
)
df_dist <- df_dist[order(df_dist$dist),]

tree <- hclust(dist_all, method = "single")


# Part 1

steps <- 1000
cut_hight <- df_dist$dist[steps]
clusters <- cutree(tree, h = cut_hight)

cluster_sizes <- sort(table(clusters), decreasing = TRUE)

print(paste0("Part 1: ", prod(cluster_sizes[1:3])))


# Part 2

max_dist <- max(tree$height)
last_merge <- sum(df_dist$dist <= max_dist)
box1 <- df_dist[last_merge,1]
box2 <- df_dist[last_merge,2]

print(paste0("Part 2: ", data[box1,1] * data[box2,1]))

print(paste0("Runtime in sec: ", round(as.numeric(Sys.time() - start_time, units = "secs"), 5)))
