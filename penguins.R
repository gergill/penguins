library(mappeR)
library(RCy3)
library(igraph)

clean_penguins = na.omit(penguins)
clean_penguins$id = 1:nrow(clean_penguins)
penguin_numbers = subset(clean_penguins, select = c(bill_len, bill_dep, flipper_len, body_mass))
normal_penguin_numbers = apply(penguin_numbers, 2, function(column) column/max(column))
penguin_dists = dist(normal_penguin_numbers)
penguin_ball_mapper = create_ball_mapper_object(clean_penguins, penguin_dists, .1)
penguin_balls = lapply(penguin_ball_mapper[[1]]$data, function(x) clean_penguins[unlist(strsplit(x, ", ")),])

penguin_ball_mapper[[1]]$average_bill_length = sapply(penguin_balls, function(penguin_ball) mean(clean_penguins[penguin_ball$id, "bill_len"]))
penguin_ball_mapper[[1]]$average_bill_depth = sapply(penguin_balls, function(penguin_ball) mean(clean_penguins[penguin_ball$id, "bill_dep"]))
penguin_ball_mapper[[1]]$average_flipper_length = sapply(penguin_balls, function(penguin_ball) mean(clean_penguins[penguin_ball$id, "flipper_len"]))
penguin_ball_mapper[[1]]$average_body_mass = sapply(penguin_balls, function(penguin_ball) mean(clean_penguins[penguin_ball$id, "body_mass"]))

penguin_ball_mapper[[1]]$biscoe_count = sapply(penguin_balls, function(penguin_ball) sum(clean_penguins[penguin_ball$id, "island"] == "Biscoe", na.rm = TRUE))
penguin_ball_mapper[[1]]$dream_count = sapply(penguin_balls, function(penguin_ball) sum(clean_penguins[penguin_ball$id, "island"] == "Dream", na.rm = TRUE))
penguin_ball_mapper[[1]]$torgersen_count = sapply(penguin_balls, function(penguin_ball) sum(clean_penguins[penguin_ball$id, "island"] == "Torgersen", na.rm = TRUE))

penguin_ball_mapper[[1]]$adelie_count = sapply(penguin_balls, function(penguin_ball) sum(clean_penguins[penguin_ball$id, "species"] == "Adelie", na.rm = TRUE))
penguin_ball_mapper[[1]]$chinstrap_count = sapply(penguin_balls, function(penguin_ball) sum(clean_penguins[penguin_ball$id, "species"] == "Chinstrap", na.rm = TRUE))
penguin_ball_mapper[[1]]$gentoo_count = sapply(penguin_balls, function(penguin_ball) sum(clean_penguins[penguin_ball$id, "species"] == "Gentoo", na.rm = TRUE))

createNetworkFromDataFrames(penguin_ball_mapper[[1]], penguin_ball_mapper[[2]])
