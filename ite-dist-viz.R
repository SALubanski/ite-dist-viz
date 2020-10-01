
# --- GENERATE DATA ------------------------------------------------------------
n <- 1000
y0_dist <- 'norm'
y0.fun <- function(dist = y0_dist)
  {
  switch(dist,
         norm = rnorm(n),
         chisq = scale(rchisq(n, 1)))
}
y0 <- y0.fun('chisq')
ITE.var <- 1
ATE <- 0.5
y1 <- y0 + ATE + rnorm(n, sd = ITE.var)
z <- sample(rep(0:1, each = n/2))



df <- data.frame(z, y0, y1)
head(df)

# treatment assignment & observed PO's ####
df$y <- with(df, ifelse(z == 0, y0, y1))
df$y1_obs <- with(df, ifelse(z == 0, NA, z)*y1)
df$y0_obs <- with(df, ifelse(z == 1, NA, 1-z)*y0)

# data.frame of plotting data (sorted PO's & estimated ITEs) ####
df.plot <- with(df, 
                {
                  y1_sort <- sort(y1_obs, decreasing = T)
                  y0_sort_decr <- sort(y0_obs, decreasing = T)
                  y0_sort_incr <- sort(y0_obs, decreasing = F)
                  ite_min_var <- y1_sort - y0_sort_decr
                  ite_max_var <- y1_sort - y0_sort_incr
                  
                  data.frame(y1_sort, y0_sort_decr, y0_sort_incr,
                             ite_min_var, ite_max_var)
                })

# --- PLOTS --------------------------------------------------------------------

par(mfrow = c(3, 2))

with(df, 
     {
       plot(x=y0, y1, type = 'n', 
            main = 'Scatterplot of True Potential Outcomes')
       
       rho.true <- round(cor(y1, y0), 3)
       
       pos.ind <- which(y1 >= y0)
       neg.ind <- which(y1 < y0)
       
       points(x=y0[pos.ind], y1[pos.ind], pch = 16, col = 'black')
       points(x=y0[neg.ind], y1[neg.ind], pch = 16, col = 'red')
       
       abline(a=0, b=1)
       mtext(paste('rho.true = ', rho.true))
     })

with(df, 
     {
       ite.dens <- density(y1 - y0)
       plot(ite.dens, main = 'True Density of ITEs',
            xlim = range(df.plot$ite_max_var))
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
     })


with(df.plot, 
     {
       rho.min.est <-  round(cor(y1_sort, y0_sort_decr), 3)
       rho.max.est <-  round(cor(y1_sort, y0_sort_incr), 3)
       
       # plot for min ITE var
       plot(x = y0_sort_decr, y = y1_sort, type = 'n',
            main = 'Scatterplot of sorted Potential Outcomes (same direction')
       
       abline(a=0, b=1)
       
       pos.ind <- which(y1_sort >= y0_sort_decr)
       neg.ind <- which(y1_sort < y0_sort_decr)
       
       points(x=y0_sort_decr[pos.ind], y1_sort[pos.ind], pch = 16, col = 'black')
       points(x=y0_sort_decr[neg.ind], y1_sort[neg.ind], pch = 16, col = 'red')
       
       mtext(paste('rho.min.est = ', rho.min.est))
       
       # density for min ITE var
       ite.dens <- density(ite_min_var)
       plot(ite.dens, main = 'Density of ITEs',
            xlim = range(ite_max_var))
       below.0 <- mean(ite_min_var < 0)
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
       
       # plot for max ITE var
       plot(x = y0_sort_incr, y = y1_sort, type = 'n',
            main = 'Scatterplot of Sorted Potential Outcomes (opposite directions)')
       
       abline(a=0, b=1)
       
       pos.ind <- which(y1_sort >= y0_sort_incr)
       neg.ind <- which(y1_sort < y0_sort_incr)
       
       points(x=y0_sort_incr[pos.ind], y1_sort[pos.ind], pch = 16, col = 'black')
       points(x=y0_sort_incr[neg.ind], y1_sort[neg.ind], pch = 16, col = 'red')
       mtext(paste('rho.max.est = ', rho.max.est))
       
       # density for min ITE var
       ite.dens <- density(ite_max_var)
       plot(ite.dens, main = 'Density of ITEs',
            xlim = range(ite_max_var))
       below.0 <- mean(ite_max_var < 0)
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
       
     })
       





# --- OLD CODE ----------------------------------------------------------------
df$y1_obs_rank <- rank(df$y1_obs, na.last = "keep")
df$y0_obs_rank <- rank(df$y0_obs, na.last = "keep")


# get imputed y1s

df$y0_imp <- apply(df, 1, function(x){
  if(x['z'] == 0) x['y0_obs'] else sample(df$y0, 1)
})


# get imptued y0s
df$y0_imp <- apply(df, 1, function(x){
  if(x['z'] == 0) x['y0_obs'] else sample(df$y0, 1)
})


# PO lookup by rank
with(df, y1_obs[which(y1_obs_rank == 1)])

rank.lookup <- function(dat = df, rank, rank.var, target.var)
{
  # browser()
  rank.min <- min(dat[, rank.var], na.rm = T)
  rank.max <- min(dat[, rank.var], na.rm = T)
  stopifnot(rank >= rank.min | rank <= rank.max)
  
  rank_ind <- which(dat[, rank.var] == rank)
  dat[rank_ind, target.var]
  
}

rank.lookup(df, rank = 1, 'y1_obs_rank', 'y1_obs')

sapply(df$y1_obs_rank, function(x){
  if(is.na(x)) NA else{
    rank.lookup(df, rank = x, 'y0_obs_rank', 'y0_obs')
  }
})

# (in progress) get imputed y1s (y0 of same rank)
apply(df, 1, function(x){
  
  # browser()
  
  if(x['z'] == 1)
  {
    x['y1_obs']
  }
  else
  {
    # sample(df$y1, 1)
    rank_ind <- x['y0_obs_rank']
    y1_ind <- which(df$y1_obs_rank == rank_ind)
    df$y1_obs[y1_ind]
  }
  
})

