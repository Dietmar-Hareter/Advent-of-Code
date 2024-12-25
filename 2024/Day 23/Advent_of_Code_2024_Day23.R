setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data_orig <- readLines("input_day23_Test.txt")
data_orig <- readLines("input_day23.txt")

data <- NULL
for (Ro in 1:length(data_orig)) {
  data <- rbind(data, unlist(strsplit(data_orig[Ro], split = '-')))
}


# Part 1 

data_graph <- igraph::graph_from_edgelist(data, directed=FALSE)
network_3 <- igraph::cliques(data_graph, min = 3, max = 3)

print(sum(sapply(network_3, function(x) {sum(stringr::str_detect(x$name, "^t")) > 0}))) # Part 1: 1083


# Part 2

largest <- igraph::largest_cliques(data_graph)
print(stringr::str_c(sort(largest[[1]]$name), collapse = ",")) # Part 2: as,bu,cp,dj,ez,fd,hu,it,kj,nx,pp,xh,yu
