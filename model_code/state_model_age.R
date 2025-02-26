
library(here)
library(rstan)
library(bayesplot)
library(geofacet)
library(ggplot2)
library(readxl)
library(reshape2)
library(tigris)
library(tidyverse)

#        example(stan_model, package = "rstan", run.dontrun = TRUE)
#        rm(list = ls())
#        .rs.restartR()
        
# these need to be run each time rstan library is loaded
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

################################
source(here("data/ct_data.R"))

source(here("code/read_claims_data_restricted.R"))

# read_excel(here("data/nhanes_nsfg.xlsx"), sheet = "nhanes") %>%
#   filter(age != "all"  ) -> nhnsd
# 
# nhnsd %>%
#   filter(ab!="NA") -> nhnsd_ab
# 
# read_excel(here("data/nhanes_nsfg.xlsx"), sheet = "nsfg") %>%
#   filter(age != "all" ) %>%
#   arrange(age) -> nsfgd


################################
# figures
# 
# gc_ct_age_sex %>%
#   filter(Gender == "F") %>%
#   ggplot(aes(x=index_year, y=positivity_ct, color=age_c)) +
#   geom_line() +
#   theme_classic() +
#   facet_geo(~ state_2)
# 
# gc_ct_age_sex %>%
#   filter(Gender == "F") %>%
#   ggplot(aes(x=index_year, y=positivity_ct, color=state_2)) +
#   geom_line() +
#   facet_wrap(~age_c) +
#   theme_bw() + theme(legend.position = "none")
# 
# gc_ct_age_sex %>%
#   # filter(Gender == "M") %>%
#   ggplot(aes(x=index_year, y=pop_prop, color=state_2)) +
#   geom_line() +
#   facet_wrap(~age_c+Gender) +
#   xlim(c(2019, 2021)) +
#   theme_bw() + theme(legend.position = "none")
# 
# gc_ct_age_sex %>%
#   filter(index_year== 2021) %>%
#   ggplot(aes(x=age_c, y=pop_prop, group=state_2, color=state_2)) +
#   geom_line() +
#   facet_wrap(~Gender) +
#   theme_bw() + theme(legend.position = "none")
# 
# gc_ct_age_sex %>%
#   filter(Gender == "M") %>%
#   filter(index_year<2022) %>%
#   ggplot(aes(y=rate/1000, x=positivity_ct*100, color=as_factor(index_year))) +
#   geom_point() +
#   geom_smooth(method='lm') +
#   facet_wrap(~age_c, scales = "free") +
#   theme_bw() + ylab("Diagnoses (per 100)") + xlab("Positivity in claims data (per 100)") + theme(legend.position = "none")
# 
# 
# gc_ct_age_sex %>%
#   filter(Gender == "M") %>%
#   filter(index_year < 2022) %>%
#   # filter(!(is.na(positivity_ct))) %>%
#   # filter(state_2 %in% c("AZ", "CA", "CO", "DC", "FL", "GA", "IL", "LA", "MS", "NC", "NJ", "NY", "OH", "PA", "SC", "TN", "TX", "VA", "WA")) %>%
#   arrange(state_2, age_c, index_year) -> dat_m
# 

# f_ctd_id  <- as.data.frame(which(!is.na(dat_f$cases_ct), arr.ind = TRUE))
# f_gc_id   <- as.data.frame(which(!is.na(dat_f$num_test_gc), arr.ind = TRUE))
# f_gcd_id  <- as.data.frame(which(!is.na(dat_f$cases_gc), arr.ind = TRUE))

state_num <- length(unique(dat_f$state_2))
yrs <- 4
ages <- 5

ctgc_data <- list(Pop =  array(dat_f$population_ct, dim = c(yrs, ages, state_num)),
                yr = yrs,
                age = ages,
                state = state_num,
                f_ct_lnt = length(f_ct_id[,1]),
                f_gc_lnt = length(f_gc_id[,1]),
                f_ctd_lnt = length(f_ctd_id[,1]),
                f_gcd_lnt = length(f_gcd_id[,1]),
                f_ct = f_ct_id,
                f_ctd = f_ctd_id,
                f_gc = f_gc_id,
                f_gcd = f_gcd_id,
                m_ct_lnt = length(m_ct_id[,1]),
                m_gc_lnt = length(m_gc_id[,1]),
                m_ctd_lnt = length(m_ctd_id[,1]),
                m_gcd_lnt = length(m_gcd_id[,1]),
                m_ct = m_ct_id,
                m_ctd = m_ctd_id,
                m_gc = m_gc_id,
                m_gcd = m_gcd_id,
                Cnum_f = ctnum_f_array,
                Cden_f = ctden_f_array,
                Gnum_f = gcnum_f_array,
                Gden_f = gcden_f_array,
                CDnum_f = ctdnum_f_array,
                CDden_f = ctdden_f_array,
                GDnum_f = gcdnum_f_array,
                GDden_f = gcdden_f_array, #,
                Cnum_m = ctnum_m_array,
                Cden_m = ctden_m_array,
                Gnum_m = gcnum_m_array,
                Gden_m = gcden_m_array,
                CDnum_m = ctdnum_m_array,
                CDden_m = ctdden_m_array,
                GDnum_m = gcdnum_m_array,
                GDden_m = gcdden_m_array
                )
  
            
init_fun <- function() { list(
  RR_f0     =  array(rgamma(5*state_num, 60, 30), dim = c(5, state_num)),
# RR_m00    =  array(rgamma(5*state_num, 60, 30), dim = c(5, state_num)),
  tr_f0     =  array(rbeta(5*state_num, 5,8), dim = c(5, state_num)),
  tr_m0     =  array(rbeta(5*state_num, 5,12), dim = c(5, state_num)),
  RR_f0_rel =  array(rgamma(5*state_num, 90, 50), dim = c(5, state_num)),
# RR_m0_rel =  array(rgamma(5*state_num, 90, 50), dim = c(5, state_num)),
  tr_f0_rel =  array(rbeta(5*state_num, 90,10), dim = c(5, state_num)),
# tr_m0_rel =  array(rbeta(5*state_num, 90,10), dim = c(5, state_num)),
  p_ct_f =  array(rbeta(years_num*age_num*state_num, 1,100), dim = c(years_num, age_num, state_num)),
# p_ct_m =  array(rbeta(years_num*age_num*state_num, 1,100), dim = c(years_num, age_num, state_num)),
  p_gc_f =  array(rbeta(years_num*age_num*state_num, 1,100), dim = c(years_num, age_num, state_num)) #,
#  p_gc_m =  array(rbeta(years_num*age_num*state_num, 1,100), dim = c(years_num, age_num, state_num))
  )
}

niter <- 5000
fit_ct <- stan(file = "model_code/state_model_1sex.stan", 
               data = ctgc_data, 
               init = init_fun,
               iter =  niter,
               warmup = niter - 100, 
               chains = 4); #, init = "0"

df_fit <- as.data.frame(fit_ct)
# print(fit_ct)

stan_dens(fit_ct, pars = "tr", separate_chains = TRUE)




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
 





