n <- 10000
nsim <- 10000
nsamp <- 5

y <- rnorm(n, mean = 0, sd = 10)
y <- rchisq(n, 1)
y <- rbinom(n, 1, 0.1)

nsamp_vec <- seq(2, 50, 1)
out <- sapply(nsamp_vec, function(x){
  replicate(nsim, {
    var(sample(y, x))
  })
})

sd_out_vec <- apply(out, 2, function(x){mean(sqrt(x))})
sd_lb_vec <- apply(out, 2, function(x){quantile(sqrt(x), probs = 0.025)})
sd_ub_vec <- apply(out, 2, function(x){quantile(sqrt(x), probs = 0.975)})

var_out_vec <- apply(out, 2, function(x){mean(x)})
var_lb_vec <- apply(out, 2, quantile, probs = 0.025)
var_ub_vec <- apply(out, 2, quantile, probs = 0.975)

var_adj_vec <- apply(out, 2, function(x){mean(x*((n-1)/(n+1)))})
var_adj_lb_vec <- apply(out, 2, function(x){quantile(x*((n-1)/(n+1)), probs = 0.025)})
var_adj_ub_vec <- apply(out, 2, function(x){quantile(x*((n-1)/(n+1)), probs = 0.975)})

true_var <- var(y)*((n-1)/n)                  # true population SD
true_sd <- sqrt(true_var)

sd_ratio <- sd_out_vec/true_sd
sd_lb_ratio <- sd_lb_vec/true_sd
sd_ub_ratio <- sd_ub_vec/true_sd

var_ratio <- var_out_vec/true_var
var_lb_ratio <- var_lb_vec/true_var
var_ub_ratio <- var_ub_vec/true_var

var_adj_ratio <- var_adj_vec/true_var
var_adj_lb_ratio <- var_adj_lb_vec/true_var
var_adj_ub_ratio <- var_adj_ub_vec/true_var

sd_ylim <- c(floor(min(sd_lb_ratio)*10)/10, 
            ceiling(max(sd_ub_ratio)*10)/10)

plot(sd_ratio, x=nsamp_vec, type = 'b', ylim = sd_ylim)
abline(h = 1, lty = 'dashed')
lines(sd_lb_ratio, x=nsamp_vec)
lines(sd_ub_ratio, x=nsamp_vec)

var_ylim <- c(floor(min(var_lb_ratio)*10)/10, 
             ceiling(max(var_ub_ratio)*10)/10)

plot(var_ratio, x=nsamp_vec, type = 'b', ylim = var_ylim)
abline(h = 1, lty = 'dashed')
lines(var_lb_ratio, x=nsamp_vec)
lines(var_ub_ratio, x=nsamp_vec)


var_adj_ylim <- c(floor(min(var_adj_lb_ratio)*10)/10, 
              ceiling(max(var_adj_ub_ratio)*10)/10)

plot(var_adj_ratio, x=nsamp_vec, type = 'b', ylim = var_adj_ylim)
abline(h = 1, lty = 'dashed')
lines(var_adj_lb_ratio, x=nsamp_vec)
lines(var_adj_ub_ratio, x=nsamp_vec)

mean_sd <- mean(sqrt(out))        # mean(sqrt(var))
mean_sqrt_var <- sqrt(mean(out)) # sqrt(mean(var))

mean_sd/true_sd
mean_sqrt_var/true_sd

plot(density(out), main = 'Density of Variance estimates')
abline(v = true_var)
abline(v = mean(out), lty = 'dashed', col = 'red')

plot(density(sqrt(out)), main = 'Density of SD estimates')
abline(v = true_sd)
abline(v = mean(sqrt(out)), lty = 'dashed', col = 'forestgreen')
abline(v = sqrt(mean(out)), lty = 'dashed', col = 'red')

seq_vec <- seq(0.1, 1, 0.01)
plot(seq_vec, sqrt(seq_vec), ylim =range(seq_vec), type = 'l')
abline(a=0, b=1)

sqrt(mean(seq_vec))
mean(sqrt(seq_vec))

# MSE COMPARISON --------------------- ####
mse.fun <- function(est_vec, param, n){
  sum((est_vec-param)^2)/n + var(est_vec)
}

mse.fun(out[, 10], true_var, nsim)
mse.fun(out[, 10]*((nsamp -1)/(nsamp + 1)), true_var, nsim)

sapply(1:length(nsamp_vec), function(x){
  
  nsamp <- nsamp_vec[x]
  vec <- out[, x]
  vec_adj <- out[, x]*((nsamp -1)/(nsamp + 1))
  
  ratio <- mean(vec)/true_var
  ratio_adj <- mean(vec_adj)/true_var
  mse <- mse.fun(vec, true_var, nsim)
  mse_adj <- mse.fun(vec_adj, true_var, nsim)
  c(var_ratio = ratio, var_ratio_adj = ratio_adj,
    mse = mse, mse_adj = mse_adj)
})

par(mfrow = c(3, 3))
sapply(1:length(nsamp_vec), function(x){
  
  nsamp <- nsamp_vec[x]
  vec <- out[, x]
  vec_adj <- out[, x]*((nsamp -1)/(nsamp + 1))
  
  dens <- density(vec)
  dens_adj <- density(vec_adj)
  
  plot(density(vec), ylim = c(0, max(dens$y, dens_adj$y)))
  lines(density(vec_adj), col = 'red', lty = 'solid')
  abline(v = mean(vec), lty = 'dotted')
  abline(v = mean(vec_adj), col = 'red', lty = 'dotted')
  abline(v = true_var, lwd = 2)
})
