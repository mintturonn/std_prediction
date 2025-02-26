
library(rstan)
library(bayesplot)
library(reshape2)
library(tidyverse)

#        example(stan_model, package = "rstan", run.dontrun = TRUE)
#        rm(list = ls())
#        .rs.restartR()
        
# these need to be run each time rstan library is loaded
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

################################
ct_data <- list(CT = 301, CTN = 10293, D = 115, DN = 5668, CTnD = 6, CTnDN = 7443, TnD =  106, TnDN = 5638, T = 1626, TN = 5641) 

fit_ct <- stan(file = "model_code/national_model_base.stan", data = ct_data)
print(fit_ct)

#   df_fit <- as.data.frame(fit_ct)

# ADD prior info into the posterior arrays
fit_post <- as.array(fit_ct)
dimnames(fit_post)

#################################
post <- array(0, dim=c(1000,5,16), 
              dimnames = list(NULL,
                              c("chain:1", "chain:2", "chain:3", "chain:4", "chain:X"),
                              c("t_ct","ct_t","d_t","ct_d","d_ct", "incid", "dur","ct", "d", "t","ct_and_d", "t_and_d", "ct_and_t", "ctN", "ct_tN", "lp_" )))
post[,1:4,] <- fit_post


post[,5,1] <- rbeta(1000, 1.5, 3) # t_ct
post[,5,2] <- rbeta(1000, 1.5, 3) # ct_t
post[,5,3] <- rbeta(1000, 1.5, 3) # d_t
post[,5,4] <- rbeta(1000, 1.5, 3) # ct_d
post[,5,5] <- rbeta(1000, 1.5, 3) # d_ct
post[,5,6] <- rbeta(1000, 1, 30) # incid
post[,5,7] <- rbeta(1000, 4, 4) # dur


mcmc_dens_overlay(post, pars = c("t_ct", "ct_t", "d_t", "ct_d","d_ct", "incid", "dur")) +
   scale_color_discrete(name = "Chain", labels = c("1", "2", "3", "4", "Prior"))

######

color_scheme_set("pink")
mcmc_pairs(fit_post, pars = c("d", "t_ct", "ct_t", "d_t", "ct_d","d_ct"),
           off_diag_args = list(size = 1.5))

color_scheme_set("blue")

mcmc_areas(fit_post, pars = c("d_t"), 
               prob = 0.95, # 80% intervals
               prob_outer = 1, # 99%
               point_est = "mean") +
  ggtitle("CT diagnosis positivity") -> f1

mcmc_areas(fit_post, pars = c("ct_t"), 
               prob = 0.95, # 80% intervals
               prob_outer = 1, # 99%
               point_est = "median") +
  ggtitle("Current CT given tested in 12m") -> f2

mcmc_areas(fit_post, pars = c("t_ct"), 
               prob = 0.95, # 80% intervals
               prob_outer = 1, # 99%
               point_est = "median") +
  ggtitle("Tested in 12m given current CT ") -> f3

mcmc_areas(fit_post, pars = c("incid"), 
           prob = 0.95, # 80% intervals
           prob_outer = 1, # 99%
           point_est = "median") +
  ggtitle("Annual incidence") -> f4

mcmc_areas(fit_post, pars = c("dur"), 
           prob = 0.95, # 80% intervals
           prob_outer = 1, # 99%
           point_est = "median") +
  ggtitle("Average duration") -> f5

plot_grid(f1, f2, f3, f4, f5, labels = c('A)', 'B)', "C)", 'D)', "E)"), label_size = 10, nrow =5)


######### Calbriation fit figure

ct_data2 <- as.data.frame(ct_data)
ct_data2$CTp <- ct_data2$CT / ct_data2$CTN
ct_data2$CTnDp <- ct_data2$CTnD / ct_data2$CTnDN
ct_data2$TnDp <- ct_data2$TnD / ct_data2$TnDN
ct_data2$Tp <- ct_data2$T / ct_data2$TN
ct_data2$Dp <- ct_data2$D / ct_data2$DN


ct_data2$CTse <- sqrt(ct_data2$CTp*(1-ct_data2$CTp)/ct_data2$CTN)
ct_data2$CTnDse <- sqrt(ct_data2$CTnDp*(1-ct_data2$CTnDp)/ct_data2$CTnDN)
ct_data2$TnDse <- sqrt(ct_data2$TnDp*(1-ct_data2$TnDp)/ct_data2$TnDN)
ct_data2$Tse <- sqrt(ct_data2$Tp*(1-ct_data2$Tp)/ct_data2$TN)
ct_data2$Dse <- sqrt(ct_data2$Dp*(1-ct_data2$Dp)/ct_data2$DN)


out <- as.data.frame(rbind(fit_post[,1,], fit_post[,2,], fit_post[,3,], fit_post[,4,])) 

# c("ct", "t", "ct_and_d", "t_and_d")

p1 <- figs("ct", "CTp", "CTse", 0.05, "Current CT")
p2 <- figs("t", "Tp", "Tse", 0.4, "CT Testing (12m)")
p3 <- figs("ct_and_d", "CTnDp", "CTnDse", 0.005, "Current CT & past CT diag(12m)")
p4 <- figs("t_and_d", "TnDp", "TnDse", 0.05, "Past CT test & CT diagn(12m)")
p5 <- figs("d", "Dp", "Dse", 0.05, "CT diagnosis (12m)")

plot_grid(p1, p2,  p5, p3, p4, labels = c('A)', 'B)', "C)", "D)" , "E)"), label_size = 12, nrow =2)

figs <- function(n1, n2, n3, ymax, name2){
  
 d <- data.frame(matrix(NA, ncol = 4, nrow = 2))
 x <- c("mean", "ll", "ul", "Estimate")
 colnames(d) <- x
 d$mean <- c(mean(out[, n1]), ct_data2[,n2])
 d$ll <- c(quantile(out[, n1], probs = 0.025)[[1]],  ct_data2[,n2] - 1.96*ct_data2[,n3] )
 d$ul <- c(quantile(out[, n1], probs = 0.975)[[1]],  ct_data2[,n2] + 1.96*ct_data2[,n3] )
 d$Estimate <- c("Model", "Data")
 
 print(d)
 
 d %>% 
 ggplot(aes(x=Estimate, y = mean, ymin = ll, ymax = ul, color=Estimate)) +
   geom_pointrange(fatten = 1) +
   ylim(c(0, ymax)) + ylab("") + xlab("") +
   theme_minimal_hgrid() +
   ggtitle(name2) +
   theme(legend.position = "none", 
         plot.title = element_text(size = 8))  -> p
}
 




# national model
fit_cloze <- stan(
  file = "model_code/national_model.stan",
  data = lst_cloze_data
)

print(fit_cloze)


df_fit_cloze <- as.data.frame(fit_cloze)
mcmc_dens(df_fit_cloze, pars = "theta") +
  geom_vline(xintercept = mean(df_fit_cloze$theta))



