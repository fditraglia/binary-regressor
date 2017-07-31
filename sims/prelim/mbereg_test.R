library(mbereg)
set.seed(98764321)
system.time(sim1 <- sim_study_a0_zero(n_reps = 1000, b = 0.5))
system.time(sim2 <- sim_study_a0_zero(n_reps = 1000, b = 0.3))
system.time(sim3 <- sim_study_a0_zero(n_reps = 1000, b = 0.1))

sim_results <- list('b=0.5' = sim1, 'b=0.3' = sim2, 'b=0.1' = sim3)
setwd('~/binary-regressor/')
save(sim_results, file = 'test_mbered.Rd')
