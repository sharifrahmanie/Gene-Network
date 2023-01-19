# Gene-Network
Gene network analysis using iGraph package
```{r }
require(igraph)

load("data.RData")
Data1 <- Data1[,-240] %>%
  as.matrix()
corr <- cor(Data1, method= "pearson")
m <- as.matrix(as.dist(corr))
set.seed(123)
g <- graph_from_adjacency_matrix(
  m,
  mode= "undirected",
  weighted = TRUE,
  diag= FALSE
)
g <- simplify(g,
              remove.multiple = TRUE,
              remove.loops = TRUE)
E(g)[which(E(g)$weight<0)]$color <- "darkblue"
E(g)[which(E(g)$weight>0)]$color <- "darkred"
E(g)$weight <- abs(E(g)$weight)
g <- delete_edges(g, E(g)[which(E(g)$weight < 0.8)])
g <- delete.vertices(g, degree(g) == 0)
V(g)$name <- V(g)$name
V(g)$shape <- "sphere"
V(g)$color <- "skyblue"
V(g)$vertex.frame.color <- "white"
scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
vSizes <- (scale01(apply(Data1, 1, mean)) + 1.0) * 10
edgeweights <- E(g)$weight * 2
mst <- mst(g)
mst.communities <- cluster_edge_betweenness(mst, directed=FALSE)
mst.clustering <- make_clusters(mst, membership=mst.communities$membership)
V(mst)$color <- mst.communities$membership + 1
plot(
  mst.clustering, mst,
  layout= layout.spring,
  edge.curved= F,
  vertex.size=vSizes,
  vertex.label.dist= -0.16,
  vertex.label.color="black",
  asp=FALSE,
  vertex.label.cex= 1,
  edge.width=edgeweights,
  edge.arrow.mode=0,
  main="")

```
