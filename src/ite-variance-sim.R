library(here)
source(here('src', 'ite-functions.R'))

# --- GENERATE DATA ------------------------------------------------------------
nsim <- 10000

sim.out <- replicate(nsim, {
  df <- dgm.fun(n = 100, y0.dist = 'norm', ite.sd = 4/10, ate = 0.5)
  
  
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
  
  sd(df.plot$ite_min_var)
})

plot(density(sim.out))           # density of ite.sd estimates
abline(v = mean(sim.out))        # expected ite.sd
abline(v = 4/10, lty = 'dashed') # true ite.sd
sim_quantiles <- quantile(sim.out, probs = c(0.025, 0.975))
sapply(sim_quantiles, function(x){
  abline(v = x, lty = 'dotted')
})
