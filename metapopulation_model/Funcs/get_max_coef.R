library(rdaenvpart)
get_max_coef <- function(fit)
{
  res <- NA
  try(res <- fit[[1]]$fit.res %>% filter(p.value < 0.05) %>% summarise(max(Coef))) 
  try(res <- res[1,1])
  return(res)
}

get_tot_explanation <- function(y,x)
{
  res <- NA
  try(temp.res <- rdaenvpart(y,x))
  try(res <- sum(temp.res$IJ.adjR2[,1]))
  return(res)
}