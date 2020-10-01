library(here)
source(here('src', 'ite-functions.R'))

# --- GENERATE DATA ------------------------------------------------------------

df <- dgm.fun(n = 100, y0.dist = 'chisq', ite.sd = 1, ate = 0.5)

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


# --- BEGIN PLOTS --------------------------------------------------------------

png(here('output', date.time.append('ite-plot-matrix.png', suffix = F)),
    width = 2400, height = 3200, res = 300)
par(mfrow = c(3, 2))

# --- TRUE ITE PLOTS -----------------------------------------------------------

# Scatterplot of True Potential Outcomes
with(df, ite.scatter(y0 = y0, y1 = y1))
title(main = 'Scatterplot of True Potential Outcomes')
mtext(paste('rho.true = ', round(cor(df$y1, df$y0), 3)))

# Density of True ITEs w/shading below 0
with(df, ite.dens(y0=y0, y1=y1, xlim = range(df.plot$ite_max_var)))
title(main = 'Density of True ITEs')

# --- ESTIMATED ITE PLOTS: *MINIMUM* VARIANCE ----------------------------------

# Scatterplot of Sorted Potential Outcomes (min ITE variance)
with(df.plot, {
  
  ite.scatter(y0=y0_sort_decr, y1=y1_sort)
  title(main = 'Scatterplot of sorted Potential Outcomes (same direction)')
  
  rho.est <- round(cor(y1_sort, y0_sort_decr), 3)
  mtext(paste('Estimated rho =', rho.est))
})

# Density of Sorted ITEs (min ITE variance)
with(df.plot, ite.dens(y0=y0_sort_decr, y1=y1_sort, xlim = range(df.plot$ite_max_var)))
title(main = 'Density of estimated ITEs (min variance)')

# --- ESTIMATED ITE PLOTS: *MAXIMUM* VARIANCE ----------------------------------

# Scatterplot of Sorted Potential Outcomes (min ITE variance)
with(df.plot, {
  
  ite.scatter(y0=y0_sort_incr, y1=y1_sort)
  title(main = 'Scatterplot of sorted Potential Outcomes (opposite direction)')
  
  rho.est <- round(cor(y0_sort_incr, y1_sort), 3)
  mtext(paste('Estimated rho =', rho.est))
})

# Density of Sorted ITEs (min ITE variance)
with(df.plot, ite.dens(y0=y0_sort_incr, y1=y1_sort, xlim = range(df.plot$ite_max_var)))
title(main = 'Density of estimated ITEs (max variance)')

dev.off()

