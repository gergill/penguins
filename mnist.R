library(mappeR)
library(RCy3)

digits = read.csv("optdigits.tra", header = FALSE)
digits_unlabeled = subset(digits, select = -c(V65))
digits$id = 1:nrow(digits)
digits_dists = dist(digits_unlabeled, method = "euclidean")
mnist_ball_mapper = create_ball_mapper_object(digits, digits_dists, 25)
mnist_balls = lapply(mnist_ball_mapper[[1]]$data, function(x) digits[unlist(strsplit(x, ", ")),])

mnist_nodes = mnist_ball_mapper[[1]]
mnist_edges = mnist_ball_mapper[[2]]

mnist_nodes$one_count = sapply(mnist_balls, function(mnist_ball) sum(digits[mnist_ball$id, "V65"] == 1, na.rm = TRUE))
mnist_nodes$two_count = sapply(mnist_balls, function(mnist_ball) sum(digits[mnist_ball$id, "V65"] == 2, na.rm = TRUE))
mnist_nodes$three_count = sapply(mnist_balls, function(mnist_ball) sum(digits[mnist_ball$id, "V65"] == 3, na.rm = TRUE))
mnist_nodes$four_count = sapply(mnist_balls, function(mnist_ball) sum(digits[mnist_ball$id, "V65"] == 4, na.rm = TRUE))
mnist_nodes$five_count = sapply(mnist_balls, function(mnist_ball) sum(digits[mnist_ball$id, "V65"] == 5, na.rm = TRUE))
mnist_nodes$six_count = sapply(mnist_balls, function(mnist_ball) sum(digits[mnist_ball$id, "V65"] == 6, na.rm = TRUE))
mnist_nodes$seven_count = sapply(mnist_balls, function(mnist_ball) sum(digits[mnist_ball$id, "V65"] == 7, na.rm = TRUE))
mnist_nodes$eight_count = sapply(mnist_balls, function(mnist_ball) sum(digits[mnist_ball$id, "V65"] == 8, na.rm = TRUE))
mnist_nodes$nine_count = sapply(mnist_balls, function(mnist_ball) sum(digits[mnist_ball$id, "V65"] == 9, na.rm = TRUE))
mnist_nodes$zero_count = sapply(mnist_balls, function(mnist_ball) sum(digits[mnist_ball$id, "V65"] == 0, na.rm = TRUE))

createNetworkFromDataFrames(mnist_nodes, mnist_edges)
