
library(here)
library(rstan)
library(bayesplot)
library(geofacet)
library(ggplot2)
library(readxl)
library(reshape2)
library(tidyverse)

#        example(stan_model, package = "rstan", run.dontrun = TRUE)
#        rm(list = ls())
#        .rs.restartR()
        
# these need to be run each time rstan library is loaded
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

################################
source(here("data/ct_data.R"))

# source(here("code/read_claims_data_restricted"))

read_excel(here("data/nhanes_nsfg.xlsx"), sheet = "nhanes") %>%
  filter(age != "all"  ) -> nhnsd

nhnsd %>%
  filter(ab!="NA") -> nhnsd_ab

read_excel(here("data/nhanes_nsfg.xlsx"), sheet = "nsfg") %>%
  filter(age != "all" ) %>%
  arrange(age) -> nsfgd


################################

# this needs to be more worked through! 
# d_pool <- (matrix(nhnsd$d[nhnsd$gender=="female"], 2, byrow = T) + matrix(nsfgd$ctd[nsfgd$gender=="female"], 2, byrow = T))
# dN_pool <- (matrix(nhnsd$d_N[nhnsd$gender=="female"], 2, byrow = T) + matrix(nsfgd$ctd_N[nsfgd$gender=="female"], 2, byrow = T))

# d_pool <- matrix(nhnsd$d[nhnsd$gender=="female"], 2, byrow = T)
# dN_pool <- matrix(nhnsd$d_N[nhnsd$gender=="female"], 2, byrow = T)

d_pool <- matrix(c(ctdat$cases_15_24[ctdat$Sex=="Female"], ctdat$cases_25_39[ctdat$Sex=="Female"]), 2, byrow = T)
dN_pool <- matrix(c(ctdat$pop_15_24[ctdat$Sex=="Female"], ctdat$pop_25_39[ctdat$Sex=="Female"]), 2, byrow = T)

# dm_pool <- (matrix(nhnsd$d[nhnsd$gender=="male"], 2, byrow = T) + matrix(nsfgd$ctd[nsfgd$gender=="male"], 2, byrow = T))
# dNm_pool <- (matrix(nhnsd$d_N[nhnsd$gender=="male"], 2, byrow = T) + matrix(nsfgd$ctd_N[nsfgd$gender=="male"], 2, byrow = T))

dm_pool  <- matrix(c(ctdat$cases_15_24[ctdat$Sex=="Male"], ctdat$cases_25_39[ctdat$Sex=="Male"]), 2, byrow = T)
dNm_pool <- matrix(c(ctdat$pop_15_24[ctdat$Sex=="Male"], ctdat$pop_25_39[ctdat$Sex=="Male"]), 2, byrow = T)

ct_data <- list(N1 = length(nhnsd$cycle[nhnsd$gender=="female" & nhnsd$age=="u25"]), N2 = length(nsfgd$cycle[nsfgd$gender=="female" & nsfgd$age=="<25"]),
                N3 = length(d_pool[1,]),
                N = length(nhnsd$cycle[nhnsd$gender=="female" & nhnsd$age=="u25"])+4,# ,
                year_nh = nhnsd$year[nhnsd$gender=="female" & nhnsd$age=="u25"], year_ns = nsfgd$year[nsfgd$gender=="female" & nsfgd$age=="<25"],
                CT = matrix(nhnsd$ct[nhnsd$gender=="female"], 2, byrow = T), CTN =  matrix(nhnsd$ct_N[nhnsd$gender=="female"], 2, byrow = T), 
                CTm = matrix(nhnsd$ct[nhnsd$gender=="male"], 2, byrow = T), CTNm = matrix(nhnsd$ct_N[nhnsd$gender=="male"], 2, byrow = T), 
                Ab = matrix(nhnsd_ab$ab[nhnsd_ab$gender=="female"], 2, byrow = T), AbN =  matrix(nhnsd_ab$ab_N[nhnsd_ab$gender=="female"], 2, byrow = T), 
                D = round(d_pool/10000,0), DN = round(dN_pool/10000,0), 
                Dm = round(dm_pool/10000,0), DNm = round(dNm_pool/10000,0), 
                # CTnD = matrix(nhnsd$j_ctnd[nhnsd$gender=="female"], 2, byrow = T), CTnDN = matrix(nhnsd$j_N[nhnsd$gender=="female"], 2, byrow = T), 
                # CTnDm = matrix(nhnsd$j_ctnd[nhnsd$gender=="male"], 2, byrow = T), CTnDNm = matrix(nhnsd$j_N[nhnsd$gender=="male"], 2, byrow = T), 
                # TnD =  matrix(nsfgd$j_tnd[nsfgd$gender=="female"], 2, byrow = T), TnDN =  matrix(nsfgd$j_N[nsfgd$gender=="female"], 2, byrow = T), 
                # TnDm =  matrix(nsfgd$j_tnd[nsfgd$gender=="male"], 2, byrow = T), TnDNm =  matrix(nsfgd$j_N[nsfgd$gender=="male"], 2, byrow = T), 
                Te = matrix(nsfgd$t[nsfgd$gender=="female"], 2, byrow = T), TN = matrix(nsfgd$t_N[nsfgd$gender=="female"], 2, byrow = T),
                Tem = matrix(nsfgd$t[nsfgd$gender=="male"], 2, byrow = T), TNm = matrix(nsfgd$t_N[nsfgd$gender=="male"], 2, byrow = T),
                year = seq(1999, 2020, by=1)- mean(seq(1999, 2020, by=1)), age = c(0,1), sex= c(0,1) )
# 

source(here("model_code/init_funs.R"))

fit_ct <- stan(file = "model_code/national_model_age.stan", data = ct_data, warmup = 4950, iter=5000, chains = 2, init = init_fun); #, init = "0"

# print(fit_ct)

df_fit <- as.data.frame(fit_ct)

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

mcmc_areas(df_fit, pars = c("dur["), 
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
 





