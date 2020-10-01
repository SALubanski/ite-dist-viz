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

