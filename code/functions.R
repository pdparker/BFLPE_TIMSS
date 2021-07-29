
z <- function(x) (x-mean(x,na.rm=TRUE))/sd(x, na.rm=TRUE)

z_se <- function(x) {
  m = mean(x, na.rm=TRUE)
  s = sd(x, na.rm=TRUE)
  weights = c(-m/s)
  new_se = sqrt(weights * x * weights)
  return(new_se)
} 

