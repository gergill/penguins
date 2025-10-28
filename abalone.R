library(mappeR)
library(RCy3)

abalone = read.csv("abalone.data", header = FALSE)
abalone$id = 1:nrow(abalone)
colnames(abalone) = c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings", "id")
normal_abalone = as.data.frame(apply(subset(abalone, select = -c(sex, rings)), 2, function(x) x/max(x)))
abalone_dists = dist(normal_abalone)
abalone_ball_mapper = create_ball_mapper_object(abalone, abalone_dists, .1)
abalone_balls = lapply(abalone_ball_mapper[[1]]$data, function(x) abalone[unlist(strsplit(x, ", ")),])

abalone_nodes = abalone_ball_mapper[[1]]
abalone_edges = abalone_ball_mapper[[2]]

abalone_nodes$average_rings = sapply(abalone_balls, function(abalone_ball) mean(abalone[abalone_ball$id, "rings"]))
abalone_nodes$male_count = sapply(abalone_balls, function(abalone_ball) sum(abalone[abalone_ball$id, "sex"] == "M", na.rm = TRUE))
abalone_nodes$female_count = sapply(abalone_balls, function(abalone_ball) sum(abalone[abalone_ball$id, "sex"] == "F", na.rm = TRUE))
abalone_nodes$infant_count = sapply(abalone_balls, function(abalone_ball) sum(abalone[abalone_ball$id, "sex"] == "I", na.rm = TRUE))

createNetworkFromDataFrames(abalone_nodes, abalone_edges)
