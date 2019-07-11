library(readr)
library(tidyverse)
library(igraph)
library(tidygraph)
library(network)
library(ggrepel)
library(sna)
library(ggraph)

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

#write_csv(as.data.frame(w), "w")

#################################
######## CREATE NETWORK ########
#################################

network <- graph_from_adjacency_matrix(w, mode = "undirected", weighted = TRUE)

# number of edges and number of vertices
ecount(network)
vcount(network)

# edges and vertices ids
V(network)
E(network)

# measures on overall network:
# diameter
diameter(network)
# average distance (small world)
mean_distance(network)
# graph density
graph.density(network)
# transitivity coefficient
transitivity(network, type = "undirected")

# create tbl network object which is easier to manipulate
tbl_network <- as_tbl_graph(network)

# plot
ggraph(tbl_network, layout = "nicely") + 
  geom_node_point() +
  geom_edge_arc(aes(alpha = weight, width = weight), curvature = 0.1) + 
  scale_edge_alpha(range = c(0.2, 1)) +
  scale_edge_width(range = c(0.2, 5)) +
  theme_graph() +
  theme(legend.position = "none")

# there are lots of small components which does not make sense to keep in the analysis
# get components
cl <- clusters(network)
cl$csize # one big components of size 284 and lots of small ones

# create a new dataset only containing the largest connected component
connected_component <- tbl_network %>%
  mutate(component_id = group_components()) %>%
  filter(component_id == 1) %>%
  rename(id = name)

plot1 <- ggraph(connected_component, layout = "nicely") + 
  geom_node_point() +
  geom_edge_arc(aes(alpha = weight, width = weight), curvature = 0.1) + 
  scale_edge_alpha(range = c(0.2, 1)) +
  scale_edge_width(range = c(0.2, 5)) +
  theme_graph() +
  theme(legend.position = "none")
plot1


#################################
##### CENTRALITY MEASURES #######
#################################
connected_component <- connected_component %>%
  mutate(w_degree = centrality_degree(weights = weight),
         unw_degree = centrality_degree(),
         betweenness = centrality_betweenness(),
         closeness = centrality_closeness(),
         pagerank = centrality_pagerank()) 

ggraph(connected_component, layout = "nicely") + 
  geom_node_point(aes(col = w_degree, size = betweenness)) +
  geom_edge_arc(aes(alpha = weight, width = weight), curvature = 0.1) + 
  scale_edge_alpha(range = c(0.2, 1)) +
  scale_edge_width(range = c(0.2, 2)) +
  scale_color_gradient(low="blue", high="red") +
  theme_graph() +
  theme(legend.position = "none")

# create a table for easier calcuations
tbl_connected_component <- as_tibble(connected_component)

# correlation between centrality measures
cor(tbl_connected_component[,4:8])

ggplot(tbl_connected_component) +
  geom_point(aes(x = w_degree, y = betweenness))

# further explore those with high betweennes and fairly low degree
ggplot(tbl_connected_component) +
  geom_point(aes(x = w_degree, y = betweenness)) +
  geom_point(data = tbl_connected_component %>% filter(w_degree <= 20 & betweenness >= 1500), 
             aes(x = w_degree, y = betweenness), col = "red")

# further explore who these individuals are
tbl_connected_component %>% 
  filter(w_degree <= 20 & betweenness >= 1500) %>%
  left_join(film_test2_castcrew, by = "id")

interesting_subj <- tbl_connected_component %>% 
  filter(w_degree <= 20 & betweenness >= 1500) %>%
  left_join(film_test2_castcrew, by = "id")

###############################
######## ADD ATTRIBUTES #######
###############################

attributes <- film_test2_castcrew %>%
  filter(id %in% tbl_connected_component$id) %>%
  distinct(id,nome,tipologia)
head(attributes)

# some people have more than one typology, let's make it one
attributes <- film_test2_castcrew %>%
  filter(id %in%  tbl_connected_component$id) %>%
  mutate(typology_company = ifelse(tipologia == "company", 1, 0)) %>%
  distinct(id, typology_company) 

connected_component <- connected_component %>% left_join(attributes, by = "id")

ggraph(connected_component, layout = "nicely") + 
  geom_node_point(aes(col = typology_company, size = betweenness)) +
  geom_edge_arc(aes(alpha = weight, width = weight), curvature = 0.1) + 
  scale_edge_alpha(range = c(0.2, 1)) +
  scale_edge_width(range = c(0.2, 2)) +
  scale_color_gradient(low="blue", high="red") +
  theme_graph() +
  theme(legend.position = "none")

# Add names, but I find a problem in univocity

#################################
##### COMMUUNITY DETECTION ######
#################################

# core-periphery
cores <- sna::kcores(w, mode = "graph", diag = FALSE)
df_cores <- data.frame(id = names(cores), cores = cores)

connected_component <- connected_component %>% left_join(df_cores, by = "id")

ggraph(connected_component, layout = "nicely") + 
  geom_node_point(aes(col = as.factor(cores), size = betweenness)) +
  geom_edge_arc(aes(alpha = weight, width = weight), curvature = 0.1) + 
  scale_edge_alpha(range = c(0.2, 1)) +
  scale_edge_width(range = c(0.2, 2)) +
#  scale_color_gradient(low="blue", high="red") +
  theme_graph() +
  theme(legend.position = "none")

connected_component <- connected_component %>%
  mutate(community = group_louvain(weights = weight))

ggraph(connected_component, layout = "nicely") + 
  geom_node_point(aes(col = as.factor(community), size = betweenness)) +
  geom_edge_arc(aes(alpha = weight, width = weight), curvature = 0.1) + 
  scale_edge_alpha(range = c(0.2, 1)) +
  scale_edge_width(range = c(0.2, 2)) +
  #  scale_color_gradient(low="blue", high="red") +
  theme_graph() +
  theme(legend.position = "none")

# community detection
as_tibble(connected_component) %>%
  group_by(community) %>%
  summarise(n = n(),
            m_deg = mean(w_degree),
            m_bet = mean(betweenness))

ggplot(connected_component %>% filter(community <= 6), 
       aes(x = typology_company, fill = as.factor(typology_company))) +
  geom_bar() +
  facet_wrap(~community) +
  theme_bw() +
  theme(legend.position = "none")

# to export as edgelist: as_edgelist(connected_component)
