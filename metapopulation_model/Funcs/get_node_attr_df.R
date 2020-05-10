library(network)
library(igraph)
get_node_attr_df <- function(net)
{
  net2  = asNetwork(net)
  node_attrs <- data.frame(
    Climate = get.vertex.attribute(net2,"Climate"),
    growth_rate = get.vertex.attribute(net2,"growth_rate"),
    pop = get.vertex.attribute(net2,"pop"),
    Socio.economic = get.vertex.attribute(net2,"Socio.economic"),
    id = get.vertex.attribute(net2,"id")
  )
  return(node_attrs)
}