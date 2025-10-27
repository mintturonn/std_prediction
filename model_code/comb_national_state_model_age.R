
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

# source(here("code/read_claims_data_restricted.R"))
source(here("code/read_claims_data_restricted_averaged.R"))

yrs <- 1 #4
ages <- 5
##############################

read_excel(here("data/nhanes_nsfg.xlsx"), sheet = "nhanes") %>%
  filter(age != "all"  ) %>%
  # filter(year > 2009) %>%
  filter(year == 2016) -> nhnsd

nhnsd %>%
  filter(ab!="NA") -> nhnsd_ab

read_excel(here("data/nhanes_nsfg.xlsx"), sheet = "nsfg") %>%
  filter(age != "all" ) %>%
  filter(year != 1999) %>%
  arrange(age) %>%
  # filter(year > 2009) %>%
  filter(year == 2019) -> nsfgd

##############################




ctgc_data <- list(
       ## national
       N1 = length(nhnsd$cycle[nhnsd$gender=="female" & nhnsd$age=="u25"]), N2 = length(nsfgd$cycle[nsfgd$gender=="female" & nsfgd$age=="<25"]),
       years = c(2023, 2023), #2010:2023,
       N = 1, #length(2010:2023),# ,
       year_nh = nhnsd$year[nhnsd$gender=="female" & nhnsd$age=="u25"], year_ns = nsfgd$year[nsfgd$gender=="female" & nsfgd$age=="<25"],
       CT = 100*matrix(nhnsd$ct[nhnsd$gender=="female"], 2, byrow = T), CTN =  100*matrix(nhnsd$ct_N[nhnsd$gender=="female"], 2, byrow = T), 
       CTm = 1000*matrix(nhnsd$ct[nhnsd$gender=="male"], 2, byrow = T), CTNm = 1000*matrix(nhnsd$ct_N[nhnsd$gender=="male"], 2, byrow = T), 
       # Ab = matrix(nhnsd_ab$ab[nhnsd_ab$gender=="female"], 2, byrow = T), AbN =  matrix(nhnsd_ab$ab_N[nhnsd_ab$gender=="female"], 2, byrow = T), 
       # D = round(d_pool/10000,0), DN = round(dN_pool/10000,0), 
       # Dm = round(dm_pool/10000,0), DNm = round(dNm_pool/10000,0), 
       Te = matrix(nsfgd$t[nsfgd$gender=="female"], 2, byrow = T), TN = matrix(nsfgd$t_N[nsfgd$gender=="female"], 2, byrow = T),
       Tem = 1000*matrix(nsfgd$t[nsfgd$gender=="male"], 2, byrow = T), TNm = 1000*matrix(nsfgd$t_N[nsfgd$gender=="male"], 2, byrow = T),
       age = ages,
       ## state
       pop_m =  pop_m_array,
       pop_f =  pop_f_array,
       yr = 1, #length(2010:2023),
       age_st = ages,
       state = state_num,
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
       CDnum_f = round(ctdnum_f_array/1),
       CDden_f = round(ctdden_f_array/1),
       GDnum_f = round(gcdnum_f_array/1),
       GDden_f = round(gcdden_f_array/1), 
       Cnum_m = ctnum_m_array,
       Cden_m = ctden_m_array,
       Gnum_m = gcnum_m_array,
       Gden_m = gcden_m_array,
       CDnum_m = ctdnum_m_array,
       CDden_m = ctdden_m_array,
       GDnum_m = gcdnum_m_array,
       GDden_m = gcdden_m_array)
  
            
init_fun <- function() { list(

  # national_tr = rnorm(length(2010:2023), 0.2, 0.05),
  # national_RR = rnorm(length(2010:2023), 2, 0.05),
  RR_f0     =  array(rgamma(state_num,30, 30), dim = c(state_num)),
  # RR_m0    =  array(rgamma(state_num,30, 30), dim = c(state_num)),
  tr_f0     =  array(rbeta(state_num,30, 30), dim = c(state_num)),
  # tr_m0     =  array(rbeta(state_num,30, 30), dim = c(state_num)),
  RR_age = rnorm(ages,1,0.1),
  tr_age = c(rgamma(1,5,2), rbeta(4,2,2)),
  p_ct_f0 = rbeta(state_num, 1,10),
  p_gc_f0 = rbeta(state_num, 1,10)
  # p_ct_f =  array(rbeta(length(2010:2023)*age_num*state_num, 1,100), dim = c(length(2010:2023), age_num, state_num)),
  # p_ct_m =  array(rbeta(length(2010:2023)*age_num*state_num, 1,100), dim = c(length(2010:2023), age_num, state_num)),
  # p_gc_f =  array(rbeta(length(2010:2023)*age_num*state_num, 1,100), dim = c(length(2010:2023), age_num, state_num))
  # p_gc_m =  array(rbeta(length(2010:2023)*age_num*state_num, 1,100), dim = c(length(2010:2023), age_num, state_num))
  )
}

niter <- 15000
fit_ct <- stan(file = "model_code/comb_national_state_model_age.stan", 
               data = ctgc_data, 
               init = init_fun,
               iter =  niter,
               warmup = niter - 50, 
               control=list(adapt_delta=0.95), # Increase adapt_delta to reduce divergent transitions:
               chains = 5); #, init = "0"

df_fit <- as.data.frame(fit_ct)
# print(fit_ct)

stan_dens(fit_ct, pars = "tr", separate_chains = TRUE)

#
print(fit_ct, probs=c(0.05, 0.5, 0.95))

# Diagnostic plots
traceplot(fit_ct)
stan_trace(fit_ct)

# Posterior checks
library(bayesplot)
mcmc_pairs(fit_ct, np = nuts_params(fit_ct))

# Check for divergences
divergent <- get_divergent_iterations(fit_ct)
print(divergent)

pairs("p_ct_f")

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







