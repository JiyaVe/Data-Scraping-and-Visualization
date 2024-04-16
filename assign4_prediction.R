### ONLY past your predict.y function here
# x = vector of length 100
predict.y <- function(x)
{
  load("fit_params.Rdata")
  
  f.x <- ridge_reg(x,y,chosen.lam)
  return(f.x)
}


