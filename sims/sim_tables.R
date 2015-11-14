setwd("~/binary-regressor/sims/")
load("prelim_sim_results.Rdata")

# Note that the entries for which aDiff is less than aSum
# appear as "zero" but they're really missing 
xtabs(round(a1_a0, 2) ~ aDiff + aSum + n + d, 
      data = subset(full_results$rmse, b == 1))

xtabs(round(bhat, 2) ~ aDiff + aSum + n + d, 
      data = subset(full_results$rmse, b == 2), 
      drop.unused.levels = TRUE)

xtabs(round(bhat, 2) ~ aDiff + aSum + n + d, 
      data = subset(full_results$rmse, b == 1), 
      drop.unused.levels = TRUE)

xtabs(round(bhat, 2) ~ aDiff + aSum + n + d, 
      data = subset(full_results$rmse, b == 0.5), 
      drop.unused.levels = TRUE)

# Various points that we want to make

#----------------------------------------------------------------
# First Point: need a pretty large signal for this to work well
#----------------------------------------------------------------