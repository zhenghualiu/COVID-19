library(network)
library(igraph)
library(intergraph)
graph_generations <- function(
  N,  # Number of nodes
  p1=0.3, # Contribution of socio-economic variables to growth rate r
  p2=0.15,  # Contribution of climate variables to growth rate r; p1 + p2 <= 1.
  p3=0.01,
  mlog = -5, sdlog = 1, # parameters for commuting rate
  x_min=0.5, x_max=1, #parameters for x variable
  err_m=0, err_sd=0.1, #parameters for error
  pop = 100000 #population
  )
{
  node_names <- paste("node",seq(1,N),sep="")
  commuting_net <- matrix(rlnorm(N^2,meanlog = mlog,sdlog = slog),N)
  colnames(commuting_net) <- node_names
  rownames(commuting_net) <- node_names
  diag(commuting_net) <- 0
  x1 <- runif(N,x_min,x_max); x2 <- runif(N,x_min,x_max);x3 <- rnorm(N,err_m,err_sd)
  net <- as.network(commuting_net)
  growth_rate <- p1*x1+p2*x2+p3*x3
  network::set.vertex.attribute(net,"Socio-economic",x1)
  network::set.vertex.attribute(net,"Climate",x2)
  network::set.vertex.attribute(net,"pop",pop)
  network::set.vertex.attribute(net,"growth_rate",growth_rate)
  network::set.edge.value(net,"commuting_rate",commuting_net)
  write_graph(asIgraph(net),file= paste("./network/","Commuting-",mlog,"N-",N,"_p1-",p1,"_p2-",
                                        p2,"_pop-",pop,".graphml",sep=""),
              format="graphml")
}

