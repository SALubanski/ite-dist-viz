# --- FUNCTIONS ----------------------------------------------------------------

date.time.append <- function(str, sep = '_', 
                             date.format ="%Y-%m-%d_%H-%M-%S",
                             suffix = T)
{
  stopifnot(is.character(str))
  if(suffix == T){
    return(paste(str, format(Sys.time(), date.format), sep = sep))
  }
  if(suffix == F){
    return(paste(format(Sys.time(), date.format), str, sep = sep)) 
  }
}

dgm.fun <- function(n, y0.dist = 'norm', ite.sd = 1, ate = 0.5)
{
  # Data Generating Mechanism (DGM) function
  # makes potential outcomes, Y0 & Y1, plus random assignment vector, Z
  
  # n...........sample size
  # y0.dist.....distribution of Y0; 'norm' = Normal; 'chisq' = Chi-Square w/1df
  # ite.sd......SD of individual treatment effect (ITEs) 
  # ate ........Average treatment effect
  
  y0.fun <- function(dist = y0.dist)    # switch for y0 distribution
  {
    switch(dist,
           norm = rnorm(n),             # 'norm' for Standard Normal
           chisq = scale(rchisq(n, 1))) # 'chisq' for Chi-Square w/1 df
  }
  
  y0 <- y0.fun('chisq')                  # generate y0
  y1 <- y0 + ate + rnorm(n, sd = ite.sd) # generate y1
  z <- sample(rep(0:1, each = n/2))      # generate Z
  
  data.frame(z, y0, y1)                  # place into data.frame
}

ite.scatter <- function(y0, y1)
{
  plot(x=y0, y1, type = 'n')
  
  pos.ind <- which(y1 >= y0)
  neg.ind <- which(y1 < y0)
  
  points(x=y0[pos.ind], y1[pos.ind], pch = 16, col = 'black')
  points(x=y0[neg.ind], y1[neg.ind], pch = 16, col = 'red')
  
  abline(a=0, b=1)
}

ite.dens <- function(y0, y1, ...)
{
  ite.dens <- density(y1 - y0)
  plot(ite.dens, ..., main = '')
  below.0 <- mean(y1 - y0 < 0)
  mtext(paste('p(ITE < 0) = ', round(below.0, 2)))
  
  # make table of x & y density value ####
  dens.tab <- cbind(ite.dens$x, ite.dens$y)
  
  # shade part of density below x = 0 ####
  apply(dens.tab[which(ite.dens$x < 0), ], 1, function(x)
  {
    segments(x0 = x[1], y0 = 0,
             x1 = x[1], y1 = x[2],
             col = 'red')
  })
}
