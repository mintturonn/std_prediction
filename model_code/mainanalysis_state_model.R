
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

ctgc_data_main <- list(
       ## national
       N1 = length(nhnsd$cycle[nhnsd$gender=="female" & nhnsd$age=="u25"]), N2 = length(nsfgd$cycle[nsfgd$gender=="female" & nsfgd$age=="<25"]),
       years = c(2023, 2023), #2010:2023,
       N = 1, #length(2010:2023),# ,
       year_nh = nhnsd$year[nhnsd$gender=="female" & nhnsd$age=="u25"], year_ns = nsfgd$year[nsfgd$gender=="female" & nsfgd$age=="<25"],
       CT = matrix(nhnsd$ct[nhnsd$gender=="female"], 2, byrow = T), CTN =  matrix(nhnsd$ct_N[nhnsd$gender=="female"], 2, byrow = T), 
       CTm = matrix(nhnsd$ct[nhnsd$gender=="male"], 2, byrow = T), CTNm = matrix(nhnsd$ct_N[nhnsd$gender=="male"], 2, byrow = T), 
       # Ab = matrix(nhnsd_ab$ab[nhnsd_ab$gender=="female"], 2, byrow = T), AbN =  matrix(nhnsd_ab$ab_N[nhnsd_ab$gender=="female"], 2, byrow = T), 
       # D = round(d_pool/10000,0), DN = round(dN_pool/10000,0), 
       # Dm = round(dm_pool/10000,0), DNm = round(dNm_pool/10000,0), 
       Te = matrix(nsfgd$t[nsfgd$gender=="female"], 2, byrow = T), TN = matrix(nsfgd$t_N[nsfgd$gender=="female"], 2, byrow = T),
       Tem = matrix(nsfgd$t[nsfgd$gender=="male"], 2, byrow = T), TNm = matrix(nsfgd$t_N[nsfgd$gender=="male"], 2, byrow = T),
       Te_hedis = 0.45*100, TN_hedis = 100,
       age = ages,
       ## state
       pop_m =  pop_m_array,
       pop_f =  pop_f_array,
       #yr = 1, #length(2010:2023),
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

  clear_ct = rgamma(1, 2598.217, 2831.49), 
  clear_gc  =rgamma(1, 266.468,  66.484),
  prsympt_ct = rbeta(1, 430, 1260), 
  prsympt_gc  = rbeta(1, 136, 293), 
  test_sympt_ct = rgamma(1, 234,32), 
  test_sympt_gc = rgamma(1, 425,  37),
  i_ct_f0 = rbeta(state_num, 1,10),
  i_gc_f0 = rbeta(state_num, 1,10),
  test_inf_RR = rgamma(5, 9.954, 3.855)
  )
}

niter <- 30000
fit_main <- stan(file = "model_code/mainanalysis_state_model.stan", 
               data = ctgc_data_main, 
               init = init_fun,
               iter =  niter,
               warmup = niter - 100, 
               control=list(adapt_delta=0.85), # Increase adapt_delta to reduce divergent transitions:
               chains = 5); #, init = "0"

df_fit <- as.data.frame(fit_main)


pars <- c("clear_ct", "clear_gc", "prsympt_ct", "prsympt_gc", "test_sympt_ct", "test_sympt_gc")
pars2 <- c("test_asympt_ctgc")
pars3 <- c("pr_det_ct_f")
pars4 <- c("test_asympt_RR")
pars5 <- c("dur_ct")
pars6 <- c("dur_gc")

pars7 <- c("test_asympt_RR")

pars8 <- c("test_inf_RR")


as.data.frame(fit_main, pars = pars) -> test
as.data.frame(fit_main, pars = pars2) -> test2
as.data.frame(fit_main, pars = pars3) -> test3

as.data.frame(fit_main, pars = pars4) -> test4
as.data.frame(fit_main, pars = pars5) -> test5
as.data.frame(fit_main, pars = pars6) -> test6

as.data.frame(fit_main, pars = pars7) -> test7

as.data.frame(fit_main, pars = pars8) -> test8

pr_detection <-(test$prsympt_ct*test$test_sympt_ct+(1-test$prsympt_ct)*test2$`test_asympt_ctgc[1,1]`)/(test$clear_ct+test$prsympt_ct*test$test_sympt_ct+(1-test$prsympt_ct)*test2$`test_asympt_ctgc[1,1]`)


mcmc_trace(as.matrix(fit_main), pars = c("gc[1]", "ct[1]")) #



