# 

library(here)
library(reshape2)
library(readxl)

dat  <- read_excel(here::here('data', 'params.xlsx'), 
                   col_types = c("guess", "guess", "guess", "guess", "numeric", "numeric", "numeric", "guess", "guess", "guess"))

# rm(list = ls())
# .rs.restartR()
###################################
# network funs, need the parttype
source(here('stat_network/network_funs.R'))
# transmission
source(here('stat_network/gc_transm.R'))
# parameters
source(here('stat_network/init_SIS_model.R'))

#################

load(here('old-calibs-all_2023-07-24.RData'))


pn_index1 <- pn_index2 <-  scr_pr <- scr_inc <- scr_diag <- scr_s <- scr_dur_per_inf <- scr_reinf <- inst_type.all <- numeric(0)
  ind_inc1 <- ind_inc2 <- ind_ties <-   ind_dur1 <- ind_dur2 <- run_id <- numeric(0)

runs1 <- length(to_save$params.all)

 for (r in 1:runs1){
 
#
  init_inf <- to_save$init_inf.all[[r]][[1]]
  parts    <-  to_save$parts.all[[r]]
  params <- to_save$params.all[[r]][[1]]
  inst_type <- to_save$inst.type.all[r,]
  
  # 8 years in total
  params$simlength <- 416
#
   for (j in 1:5){
      # mod <- sims_pn_instonly(init_inf, parts, inst_type, params)
       mod <- sims_pn(init_inf, parts, inst_type, params)
# # 
    run_id <- rbind(run_id, r)   
# # Individual
    ind_inc1 <-   cbind(ind_inc1, rowSums(mod$inc[,1:209])) 
    ind_inc2 <-   cbind(ind_inc2, rowSums(mod$inc[,210:416])) 
    ind_ties <-   cbind(ind_ties, rowSums(parts>0))
    ind_dur1 <-   cbind( ind_dur1, rowSums(mod$inf[,1:209]>0)) 
    ind_dur2 <-   cbind( ind_dur2, rowSums(mod$inf[,210:416]>0)) 
  # population level  
    scr_pr   <-  rbind(scr_pr, colSums(mod$inf>0))
    scr_inc  <-  rbind(scr_inc, cumsum(colSums(mod$inc>0)))
    scr_diag <-  rbind(scr_diag, cumsum(colSums(mod$diagn>0)))
    scr_s    <-  rbind(scr_s, cumsum(colSums(mod$diagn==2)))
    inst_type.all <- rbind(inst_type.all, inst_type)
    pn_index1 <-  rbind(pn_index1, cumsum(colSums(mod$pn_index[,1:209])))
    pn_index2 <-  rbind(pn_index2, cumsum(colSums(mod$pn_index[,210:416])))

# print(r)
   }
 }


base <- list(params.all = to_save$params.all,
             init_inf.all = to_save$init_inf.all,
             parts.all = to_save$parts.all,
             inst.type.all = to_save$inst.type.all,
              ind_inc1 = ind_inc1,
              ind_inc2 = ind_inc2,
              ind_ties = ind_ties,
              ind_dur1 = ind_dur1,
              ind_dur2 = ind_dur2,
              scr_pr = scr_pr, 
              scr_inc = scr_inc,
              scr_diag = scr_diag,
              scr_s = scr_s,
              pn_index1= pn_index1,
              pn_index2 = pn_index2)

save(base, file = paste0("intervention_", "_", Sys.time(), ".RData"))

quit(save = "no", status = 0)

