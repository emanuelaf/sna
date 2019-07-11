library(readr)
library(tidyverse)
library(igraph)
library(tidygraph)
library(network)
library(sna)

#################################
# DATA EXPLORATION AND CLEANING #
#################################

# import data
film_test2_castcrew <- read_csv("data/film_test2_castcrew.csv")

# see the first 6 rows
head(film_test2_castcrew)

# X1 is just a row number, we do not need it, use select(-) to remove column it
film_test2_castcrew <- film_test2_castcrew %>%
  select(-X1)

# are there equals lines?
film_test2_castcrew %>% group_by_all() %>% summarise(count = n()) %>% arrange(desc(count)) #yes

# keep only distinct rows
film_test2_castcrew <- film_test2_castcrew %>%
  distinct(film, id, nome, tipologia)


# is every entities' name associated with a unique id?
length(unique(film_test2_castcrew$id)) 
length(unique(film_test2_castcrew$nome)) # no, this may be a problem


# some descriptive statistics
film_test2_castcrew %>%
  group_by(nome) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

film_test2_castcrew %>%
  group_by(tipologia) %>%
  summarise(n = n())

#film_test2_castcrew <- film_test2_castcrew %>%
#  filter(tipologia != "company")

ncol(film_test2_castcrew)
nrow(film_test2_castcrew)

length(unique(film_test2_castcrew$id))
length(unique(film_test2_castcrew$film))

castcrew_list <- split(film_test2_castcrew, f = film_test2_castcrew$id)
castcrew_list <- castcrew_list[lapply(castcrew_list, nrow) > 2]

w <- matrix(ncol = length(castcrew_list), nrow = length(castcrew_list))
colnames(w) <- names(castcrew_list)
rownames(w) <- names(castcrew_list)

for (i in 1:(nrow(w))) {
  for (j in i:(nrow(w))) {
    w[i,j] <- length(intersect(castcrew_list[[i]]$film, 
                               castcrew_list[[j]]$film))
  }
}

for (i in 2:nrow(w)) {
  for (j in 1:(i-1)) {
    w[i,j] <- length(intersect(castcrew_list[[i]]$film, 
                               castcrew_list[[j]]$film))
  }
}

diag(w) <- 0

# write.table(w, "w")
network <- graph_from_adjacency_matrix(w, mode = "undirected", weighted = TRUE)

# number of edges and number of vertices
ecount(network)
vcount(network)
V(network)
E(network)

# Representing the data
plot(network) # Not too nice!

layout <- layout.fruchterman.reingold(network) # algorithm form a nicer layout
# optimize a stress function
# prevent from overlap (nodes and ties)
plot(network,
     vertex.label = NA, # clear from the labels
     vertex.size = 4, # how big should be a node
     layout = layout,
     #     edge.arrow.size = 0.15,
     edge.width=E(network)$weight) # much better

# get component
cl <- clusters(network)
cluster.distribution(network)
nodes_cl <- which(cl$membership == which.max(cl$csize))

gi <- induced.subgraph(network, which(cl$membership == which.max(cl$csize)))
# if you want here you can decide if you want values only
# in the upper or lower triangle or both
ad <- get.adjacency(gi)

giant.network.igraph <- graph.adjacency(ad, mode="undirected", weighted = T) # transform the matrix into a "igraph-object"

layout <- layout.fruchterman.reingold(giant.network.igraph) # algorithm form a nicer layout
# optimize a stress function
# prevent from overlap (nodes and ties)
plot(giant.network.igraph,
     vertex.label = NA, # clear from the labels
     vertex.size = 4, # how big should be a node
     layout = layout,
     edge.arrow.size = 0.15, # much better
     edge.width=E(giant.network.igraph)$weight) 

# add attributes
# create the dataset with attributes
film_test2_castcrew <- film_test2_castcrew %>% mutate(company_binaria = ifelse(tipologia == "company", 1, 0))

attributes <- film_test2_castcrew %>%
  filter(id %in% names(V(giant.network.igraph))) %>%
  distinct(id, company_binaria) %>%
  arrange(id)

# add attributes to the network
vertex_attr(giant.network.igraph, "company_bin") <- attributes$company_binaria

# show attributes
vertex_attr(giant.network.igraph)

layout <- layout.fruchterman.reingold(giant.network.igraph) # algorithm form a nicer layout
# optimize a stress function
# prevent from overlap (nodes and ties)
plot(giant.network.igraph,
     vertex.label = NA, # clear from the labels
     vertex.size = 4, # how big should be a node
     layout = layout,
     edge.arrow.size = 0.15, # much better
     edge.width = E(giant.network.igraph)$weight,
     vertex.color = vertex_attr(giant.network.igraph)$company_bin)

# aggiungiamo misure e coloriamo in base ad esse (degree, etc)
gden(as.network(ad))
degDistr <- degree(as.network(ad))

degreeCen <- sna::degree(as.network(ad), gmode="graph") # most relevant measure
closeCen <- sna::closeness(as.network(ad), gmode="graph") # not relevant in this case
betweenCen <- sna::betweenness(as.network(ad), gmode="graph")

cen <- data.frame(name = vertex_attr(giant.network.igraph)$name, degreeCen, closeCen, betweenCen)
colnames(cen) <- c("id", "degree", "closeness", "betweenness")
# cen %>% View
summary(cen)

vertex_attr(giant.network.igraph, "degree") <- cen$degree
vertex_attr(giant.network.igraph, "closeness") <- cen$closeness
vertex_attr(giant.network.igraph, "betweenness") <- cen$betweenness

vertex_attr(giant.network.igraph)

layout <- layout.fruchterman.reingold(giant.network.igraph) # algorithm form a nicer layout
# optimize a stress function
# prevent from overlap (nodes and ties)
plot(giant.network.igraph,
     vertex.label = NA, # clear from the labels
     vertex.size = 4, # how big should be a node
     layout = layout,
     edge.arrow.size = 0.15, # much better
     edge.width = E(giant.network.igraph)$weight,
     vertex.color = vertex_attr(giant.network.igraph)$company_bin,
     vertex.size = vertex_attr(giant.network.igraph)$degree)
