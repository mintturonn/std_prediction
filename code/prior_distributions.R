
library(here)
library(rriskDistributions)
library(readxl)

betavars <- function(mu1, var1){
  alpha1 = ((1 - mu1) / var1 - 1 / mu1) * mu1 ^ 2;
  beta1 = alpha1 * (1 / mu1 - 1);
  
  return(list(alpha=alpha1, beta=beta1))
}


###############################################################################

### Natural history

# Symptomatic proportion
sympt_pr_ct     <- 1-c(0.775, 0.745, 0.716)
sympt_pr_gc     <- 1-c(0.742, 0.684, 0.618) 

# sympt rate
sympt_rate_ct <- 1/(c(58.9, 49.4, 39.7)/365)
sympt_rate_gc <- 1/(c(36.2, 31.8, 27.5)/365)

# clearance rate
clear_ct <- 1/(c(416.1, 397.9, 376.0)/365)
clear_gc <- 1/(c(108.6, 91.3, 77.3)/365)

# RR for screening infected
rr_inf <- c(0, 1.2, 3.4) # (1, 2.2, 4.4)


 
p <- c(0.025, 0.50, 0.975) 

fit.sympt_pr_ct <- round(get.beta.par(p, as.vector(sympt_pr_ct), show.output = FALSE, tol = 0.0001),3)
fit.sympt_pr_gc   <- round(get.beta.par(p, as.vector(sympt_pr_gc), show.output = FALSE, tol = 0.0001),3)

fit.sympt_rate_ct <- round(get.gamma.par(p, as.vector(sympt_rate_ct), show.output = FALSE, tol = 0.0001),3)
fit.sympt_rate_gc <- round(get.gamma.par(p, as.vector(sympt_rate_gc), show.output = FALSE, tol = 0.0001),3)

fit.clear_ct <- round(get.gamma.par(p, as.vector(clear_ct), show.output = FALSE, tol = 0.0001),3)
fit.clear_gc <- round(get.gamma.par(p, as.vector(clear_gc), show.output = FALSE, tol = 0.0001),3)

fit.rr_inf <- round(get.gamma.par(p, as.vector(rr_inf), show.output = FALSE, tol = 0.0001),3)

# screening rate
fit.screen_uninf <- round(get.beta.par(c(0.25, 0.975), as.vector(c(0.335, 0.67)), show.output = FALSE, tol = 0.0001),3)


