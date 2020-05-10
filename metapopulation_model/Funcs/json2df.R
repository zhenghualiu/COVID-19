library(rlist)
json2df <- function(json,q.level)
{
  N <- length(json)
  names(json) <- paste("rep_",seq(1,N),sep="")
  json1 <- list.rbind(json)
  M <- length(json1[1,1][[1]]$susceptible)
  L <- nrow(json1)
  K <- ncol(json1)
  json2 <- as.data.frame(apply(json1,2,function(x){list.cbind(list.rbind(x)[,1])}))
  json2$rep <- rep(seq(1,L),each=M)
  json2$q.level <- q.level
  json2$n.nodes <- K
  json2$day <- rep(seq(1,M),L)
  res <- melt.data.frame(json2,id.vars = c("rep","q.level","n.nodes","day"))
  return(res)
}