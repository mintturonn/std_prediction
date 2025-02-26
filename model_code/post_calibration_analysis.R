# 

library(here)
library(tidyverse)
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

load("/Users/minttu/gc_pn_impact/calibration/old-calibs-all_2023-07-24.RData")
# 
# cnames <- list.files("~/gc_pn_impact/calibration", "runs_nw",  full.names=TRUE)
# cruns <- length(cnames)
# 
# priors <- matrix(NA,  10^5, length(dat$params))
# prmsdat <- matrix(NA,  cruns, length(dat$params))
# colnames(priors) <- colnames(prmsdat) <- dat$params
# 
# prmsdat <- cbind(prmsdat, trnsm_pr_inst=rep(NA, 15))
# 
# params.all <- init_inf.all  <- parts.all <- nw_type.all <- inst.type.all <- list() 
# 
# for (i in 1:cruns) {
#   
#   load(cnames[i])
#   
#   ifelse( inst_transmission_add[i]==1,  runs$params.all[[1]]$trnsm_pr_inst <- 0.01, runs$params.all[[1]]$trnsm_pr_inst <- 0.02)
#  
#   
#   params.all[[i]]   <- runs$params.all
#   init_inf.all[[i]] <- runs$init_inf
#   parts.all[[i]]    <-  runs$parts
#   nw_type.all[[i]]  <- runs$nw_type
#   inst.type.all[[i]] <-  inst_type <- rbinom(params[["popsize"]], 1, rbeta(1, dat$shape1[dat$params=="pr_inst_type"], dat$shape2[dat$params=="pr_inst_type"]))
#   
#   prmsdat[i, "sympt_pr"]      <-  runs$params.all[[1]]$sympt_pr
#   prmsdat[i, "sympt_test_pr"] <-  runs$params.all[[1]]$sympt_test_pr
#   prmsdat[i, "trnsm_pr_cas"]  <-  runs$params.all[[1]]$trnsm_pr_cas
#   prmsdat[i, "trnsm_pr_main"] <-  runs$params.all[[1]]$trnsm_pr_rr
#   prmsdat[i, "clear_pr"]      <-  runs$params.all[[1]]$clear_pr
#   prmsdat[i, "screen_pr"]     <-  runs$params.all[[1]]$screen_pr
#   prmsdat[i, "index_interview"]<-  runs$params.all[[1]]$pn_pr_interv
#   prmsdat[i, "partner_reached"]<-  runs$params.all[[1]]$pn_pr_reach
#   prmsdat[i, "trnsm_pr_inst"]<-   runs$params.all[[1]]$trnsm_pr_inst
#   prmsdat[i, "pn_pr_main"]    <-  runs$params.all[[1]]$pn_pr_interv * runs$params.all[[1]]$pn_pr_reach
# }
# 
# to_save <- list(params.all = params.all, 
#              init_inf.all = init_inf.all,
#              parts.all = parts.all,
#              nw_type.all = nw_type.all,
#              prmsdat = prmsdat,
#              inst.type.all = inst.type.all) 
# save(to_save, file = paste0("old-calibs-all_", "_", Sys.time(), ".RData"))
# 
#    pn_index <-  scr_pr <- scr_inc <- scr_diag <- scr_s <- scr_dur_per_inf <- scr_reinf <- inst_type.all <- numeric(0)
# 
#    crn_pn_partn <- crn_pn_pr_main <- crn_sypmt_pr <- crn_sympt_test <- crn_screen_pr <- crn_clear_pr <- list(0)
# #  crn_inf_cas <- crn_inf_main <-

   s <- 1
   
 for (r in 1:cruns){
#
  init_inf <- init_inf.all[[r]][[1]]
  parts    <-  parts.all[[r]]
  params <- params.all[[r]][[1]]
  
  params$simlength <- 416
  
#
   for (j in 1:5){
      # mod <- sims_pn_instonly(init_inf, parts, inst_type, params)
       mod <- sims_pn(init_inf, parts, inst_type, params)
# #
# #
    ind_inc <-   cbind(rowSums(mod$inc[,1:209]))   
    scr_pr   <-  rbind(scr_pr, colSums(mod$inf>0))
    scr_inc  <-  rbind(scr_inc, cumsum(colSums(mod$inc>0)))
    scr_diag <-  rbind(scr_diag, cumsum(colSums(mod$diagn>0)))
    scr_s    <-  rbind(scr_s, cumsum(colSums(mod$diagn==2)))
    inst_type.all <- rbind(inst_type.all, inst_type)
    pn_index <-  rbind(pn_index, cumsum(colSums(mod$pn_pr_main)))

    # crn_pn_partn[[s]] <-  mod$rn_pn_partn
    # crn_pn_pr_main[[s]] <- mod$crn_pn_pr_main
    # crn_sypmt_pr[[s]] <-  mod$crn_sypmt_pr
    # crn_sympt_test[[s]] <-  mod$crn_sympt_test
    # crn_screen_pr[[s]] <-  mod$crn_screen_pr
    # crn_clear_pr[[s]] <-  mod$crn_clear_pr
    # crn_inf_cas[[s]]  <- mod$crn_inf_cas
    # crn_inf_main[[s]]  <- mod$crn_inf_main
    
print(r)
   }
 }

# 
# Sys.time()-p
# 
# 
# 
# 
# # inst_tie_fun <- function(){
# # 
# # 
# # 
# # 
# # }
# 
# inst_type <- rbinom(params[["popsize"]], 1, 0.3)

# 
# sims_pn_instonly <- function(init_inf, types, inst_type, params) {
#   
#   inf_state <- init_inf
#   incidence <- clearance <- diagn <- pn_index <- crn_pn_partn <- which_partners <- matrix(0, dim(init_inf)[1], params[["simlength"]] +1)
#   crn_pn_pr_main <- matrix(rbinom(params[["popsize"]] *(params[["simlength"]]+1) , 1, params[["pn_pr_main"]]), nrow = params[["popsize"]], byrow=TRUE) 
#   
#   # partner level PN using CRN (1 partner ony received PN)
#   for (i in 1:params[["popsize"]]){
#     
#   ifelse(sum(which(types[i,]==2))>0, crn_pn_partn[i,] <- sample(which(types[i,]==2), params[["simlength"]] +1, replace = TRUE), NA )
#   
#     }
#   
#   # individual-level events
#   crn_sypmt_pr <- matrix(rbinom(params[["popsize"]] *(params[["simlength"]]+1) , 1, params[["sympt_pr"]]), nrow = params[["popsize"]], byrow=TRUE) 
#   crn_clear_pr <- matrix(rbinom(params[["popsize"]] *(params[["simlength"]]+1) , 1, params[["clear_pr"]]), nrow = params[["popsize"]], byrow=TRUE) 
#   crn_screen_pr <- matrix(rbinom(params[["popsize"]] *(params[["simlength"]]+1) , 1, params[["screen_pr"]]), nrow = params[["popsize"]], byrow=TRUE) 
#   crn_sympt_test <- matrix(rbinom(params[["popsize"]] *(params[["simlength"]]+1) , 1, params[["sympt_test_pr"]]), nrow = params[["popsize"]], byrow=TRUE) 
#   
#   crn_inf_cas <- array(0, dim = c(100, params[["simlength"]]+1, params[["popsize"]])) 
#   crn_inf_main <- array(0, dim = c(100, params[["simlength"]]+1, params[["popsize"]])) 
#   
#   for (i  in 1:100){
#     for (s in 1:(params[["simlength"]]+1)){
#       for (p in 1:params[["popsize"]]){
#     crn_inf_cas[i,s,p] <-   rbinom(1,1, 1-(1-params[["trnsm_pr_cas"]])^(i) )
#     crn_inf_main[i,s,p] <-   rbinom(1,1, 1-(1-params[["trnsm_pr_main"]])^(i) )
#       }
#     }
#   }
#  
#   for (i in 2:( params[["simlength"]] +1)){
#     
#     for (m in 1:params[["popsize"]]){
#       
#       # infection acquisition
#       if ( inf_state[m,i-1]==0 ) {
#       
#         # check who the partners are
#         caspart <- ifelse( sum(types[m,]==1)>0, which(types[m,]==1), 0) #  1-(1-params[["trnsm_pr_cas"]])^(1:length(which(types[m,]==1)))
#         mainpart <-ifelse( sum(types[m,]==2)>0, which(types[m,]==2), 0)  
#     
#         # at tie level
#         # cas_inf_pr <- 1-(1-params[["trnsm_pr_cas"]])^sum(inf_state[caspart,i-1]>0)
#         # main_inf_pr <- 1-(1-params[["trnsm_pr_main"]])^sum(inf_state[mainpart,i-1]>0)
#         cas_inf <- ifelse(sum(inf_state[caspart,i-1]>0)>0, crn_inf_cas[sum(inf_state[caspart,i-1]>0),i,m] ,0)
#         main_inf <- ifelse(sum(inf_state[mainpart,i-1]>0)>0, crn_inf_main[sum(inf_state[mainpart,i-1]>0),i,m] ,0)
#         
#         # here prevalence among those with instantaneous partners
#         ifelse(inst_type[m] == 1, 
#                inst_inf_pr <- 1-(1-params[["trnsm_pr_inst"]]*(sum(inf_state[inst_type==1,i-1])/sum(inst_type==1)) )^1, 
#                inst_inf_pr <- 0)
#         
#        # trnsm_event <- ifelse(rbinom(1,1, cas_inf_pr)+rbinom(1,1, main_inf_pr)+rbinom(1,1, inst_inf_pr) >0, 1, 0)
#         trnsm_event <- ifelse(cas_inf + main_inf + rbinom(1,1, inst_inf_pr) >0, 1, 0)
#         
#         new_inf <- ifelse(trnsm_event==1, ifelse( crn_sypmt_pr[m,i] ==1, 2, 1), 0)
#         inf_state[m,i] <- new_inf
#         incidence[m,i] <- new_inf
#         
#         # infection clearance  
#       }else{
#         # symptomatic
#         if ( inf_state[m,i-1]==2 ) {
#            
#           clearance[m,i] <-  crn_clear_pr[m,i]
#           diagn[m,i] <- ifelse(crn_sympt_test[m,i]==1, 2,0)
#           
#           inf_state[m,i] <- ifelse(clearance[m,i]>0 | diagn[m,i]>0 , 0, inf_state[m,i-1])
#         }
#         
#         # asymptomatic
#         if ( inf_state[m,i-1]==1 ) {
# 
#           clearance[m,i] <- crn_clear_pr[m,i]
#           diagn[m,i] <- crn_screen_pr[m,i]
#           
#           inf_state[m,i] <- ifelse(clearance[m,i]>0 | diagn[m,i]>0 , 0, inf_state[m,i-1])
#         }
#         
#         # partner notification
#         if (diagn[m,i] >0 &  params[["pn_pr_main"]]>0 & sum(which(types[m,]==2))>0 ) {
#           
#           if (crn_pn_pr_main[m,i] > 0){
#             # partv <- which(types[m,]==2)
#             
#             # how many partners  (currently n=1)
#               which_partners[m,i] <- crn_pn_partn[m,i]
#             
#             # if the notified partner is infected, they are treated
#             pn_succ <- ifelse(inf_state[crn_pn_partn[m,i],i-1]>0, 3, -3)
#             # update infection state of the notified partner
#             inf_state[which_partners[m,i],i] <- ifelse(pn_succ == 3, 0, inf_state[which_partners[m,i],i-1])
#             # record the notified partner was cured via PN
#             diagn[which_partners[m,i],i] <- ifelse(pn_succ == 3 & diagn[which_partners[m,i],i]==0, 3, diagn[which_partners[m,i],i])
#             
#           }
#         }
#       }
#     }
#   }
#   
#   return(list(inf = inf_state, inc = incidence, clear = clearance, diagn = diagn,  pn_partners = which_partners, 
#               crn_pn_pr_main=crn_pn_pr_main, crn_pn_partn=crn_pn_partn, crn_sypmt_pr= crn_sypmt_pr, 
#               crn_clear_pr=crn_clear_pr, crn_screen_pr=crn_screen_pr, crn_sympt_test= crn_sympt_test,
#               crn_inf_cas=crn_inf_cas, crn_inf_main=crn_inf_main)) 
# }
# 
# 
# ############################################################
# 
# sims_pn_instonly_inter <- function(init_inf, types, inst_type, params, crn_pn_pr_main, crn_pn_partn, crn_sypmt_pr, crn_clear_pr, crn_screen_pr, crn_sympt_test, crn_inf_cas, crn_inf_main) {
#   
#   inf_state <- init_inf
#   incidence <- clearance <- diagn <- pn_index <- crn_pn_partn <- which_partners <- matrix(0, dim(init_inf)[1], params[["simlength"]] +1)
#   
#   
#   for (i in 2:( params[["simlength"]] +1)){
#     
#     for (m in 1:params[["popsize"]]){
#       
#       # infection acquisition
#       if ( inf_state[m,i-1]==0 ) {
#   
#         
#         # check who the partners are
#         caspart <- ifelse( sum(types[m,]==1)>0, which(types[m,]==1), 0) #  1-(1-params[["trnsm_pr_cas"]])^(1:length(which(types[m,]==1)))
#         mainpart <-ifelse( sum(types[m,]==2)>0, which(types[m,]==2), 0) 
#         
#         # at tie level
#         # cas_inf_pr <- 1-(1-params[["trnsm_pr_cas"]])^sum(inf_state[caspart,i-1]>0)
#         # main_inf_pr <- 1-(1-params[["trnsm_pr_main"]])^sum(inf_state[mainpart,i-1]>0)
#         cas_inf <- ifelse(sum(inf_state[caspart,i-1]>0)>0, crn_inf_cas[sum(inf_state[caspart,i-1]>0),i,m] ,0)
#         main_inf <- ifelse(sum(inf_state[mainpart,i-1]>0)>0, crn_inf_main[sum(inf_state[mainpart,i-1]>0),i,m] ,0)
#         
#         # here prevalence among those with instantaneous partners
#         ifelse(inst_type[m] == 1, 
#                inst_inf_pr <- 1-(1-params[["trnsm_pr_inst"]]*(sum(inf_state[inst_type==1,i-1])/sum(inst_type==1)))^1, 
#                inst_inf_pr <- 0)
#         
#         # trnsm_event <- ifelse(rbinom(1,1, cas_inf_pr)+rbinom(1,1, main_inf_pr)+rbinom(1,1, inst_inf_pr) >0, 1, 0)
#         trnsm_event <- ifelse(cas_inf + main_inf + rbinom(1,1, inst_inf_pr) >0, 1, 0)
#         
#         new_inf <- ifelse(trnsm_event==1, ifelse( crn_sypmt_pr[m,i] ==1, 2, 1), 0)
#         inf_state[m,i] <- new_inf
#         incidence[m,i] <- new_inf
#         
#         # infection clearance  
#       }else{
#         # symptomatic
#         if ( inf_state[m,i-1]==2 ) {
#           
#           clearance[m,i] <-  crn_clear_pr[m,i]
#           diagn[m,i] <- ifelse(crn_sympt_test[m,i]==1, 2,0)
#           
#           inf_state[m,i] <- ifelse(clearance[m,i]>0 | diagn[m,i]>0 , 0, inf_state[m,i-1])
#         }
#         
#         # asymptomatic
#         if ( inf_state[m,i-1]==1 ) {
#           
#           clearance[m,i] <- crn_clear_pr[m,i]
#           diagn[m,i] <- crn_screen_pr[m,i]
#           
#           inf_state[m,i] <- ifelse(clearance[m,i]>0 | diagn[m,i]>0 , 0, inf_state[m,i-1])
#         }
#         
#         # partner notification
#         if (diagn[m,i] >0 &  params[["pn_pr_main"]]>0 & sum(which(types[m,]==2))>0 ) {
#           
#           if (crn_pn_pr_main[m,i] > 0){
#             # partv <- which(types[m,]==2)
#             
#             # how many partners  (currently n=1)
#             which_partners[m,i] <- crn_pn_partn[m,i]
#             
#             # if the notified partner is infected, they are treated
#             pn_succ <- ifelse(inf_state[crn_pn_partn[m,i],i-1]>0, 3, -3)
#             # update infection state of the notified partner
#             inf_state[which_partners[m,i],i] <- ifelse(pn_succ == 3, 0, inf_state[which_partners[m,i],i-1])
#             # record the notified partner was cured via PN
#             diagn[which_partners[m,i],i] <- ifelse(pn_succ == 3 & diagn[which_partners[m,i],i]==0, 3, diagn[which_partners[m,i],i])
#             
#           }
#         }
#       }
#     }
#   }
#   
#   return(list(inf = inf_state, inc = incidence, clear = clearance, diagn = diagn,  pn_partners = which_partners, 
#                crn_pn_pr_main=crn_pn_pr_main, crn_pn_partn=crn_pn_partn, crn_sypmt_pr= crn_sypmt_pr, 
#                crn_clear_pr=crn_clear_pr, crn_screen_pr=crn_screen_pr, crn_sympt_test= crn_sympt_test)) 
# }
# 
# 
# 
