# newest code -------------------------------------------------------------

# factors -----------------------------------------------------------------
## factors to get maxes
fctr = matrix(rep(NA,6), byrow=T, ncol=2)
# factor by which to find the max (uniform(1,2))
for(gg in 1){
  fctr[2, gg] = runif(1, data$factorscr[1], data$factorscr[2])
  fctr[1, gg] = runif(1, 1, fctr[2,gg]) 
  fctr[3, gg] = runif(1, 1, fctr[1,gg])}

  #fctr[3, 1] = runif(1, 1, fctr[1,1]) 
for(gg in 2){ for(k in 1:data$nages){
  fctr[k, gg] = runif(1,1,1.5) 
}}

  cat('factor 3: ', fctr[3,], '\n')
# initial points ----------------------------------------------------------
  ## indexes [,1,2,1] = timepoint 1, age group 2, gender 1
  ## start with initial timepoints for females (i.e. [, 1, , 1]), age group 2, FEMALES & MALES
for(dd in 1){  ## will be (dd in 1:data$ndis) for independent
sscr.prior[,1,2,1,dd] = scr.prior[,1,2,1,dd] =  rbeta(dim.post, data$pbetascr[1], data$pbetascr[2]);
##sscr.prior[,1,2,2,dd] = scr.prior[,1,2,2,dd] =  dmultB*sscr.prior[,1,2,1,dd] ## commented Dec11 2018

  ## age group 1
for(gg in 1){ ## was 1:data$ngenders
sscr.prior[,1,1,gg,dd] = scr.prior[,1,1,gg,dd] =  runif(dim.post, 0, sscr.prior[,1,2,gg,dd])
sscr.prior[,1,3,gg,dd] = scr.prior[,1,3,gg,dd] =  runif(dim.post, 0, sscr.prior[,1,1,gg,dd])  ## oldest females
#sscr.prior[,1,3,gg,dd] = scr.prior[,1,3,gg,dd] =  runif(dim.post, 0, sscr.prior[,1,1,gg,dd]) ## commented to accommodate new old males
}

# added Dec 11 2018 --------------------------------------------------------#
for(k in 1:data$nages){sscr.prior[,1,k,2,dd] = scr.prior[,1,k,2,dd] =  runif(dim.post, 0.1, 0.3) } ## first point, any ages, males, dd=1 CT

## age group 3 (dd=1)
## sscr.prior[,1,3,2,dd] = scr.prior[,1,3,2,dd] =  runif(dim.post, 0.1, 0.3)  ## oldest males commented Dec11 2018

# maxes & wiggling --------------------------------------------------------
## always (d==1)
##for(gg in 1:data$ngenders){for(k in 1:data$nages){ 
for(k in 1:data$nages){for(gg in 1){
  sscr.prior[,data$brkpoint.scr,k,gg,dd] = scr.prior[,data$brkpoint.scr,k,gg,dd] = fctr[k,gg]*scr.prior[,1,k,gg,dd] # column 1, i.e. the start.
  } ## closes gg in 1 (females); now males:
for(gg in 2){sscr.prior[,data$brkpoint.scr,k,gg,dd] = scr.prior[,data$brkpoint.scr,k,gg,dd] = fctr[3,2]*scr.prior[,1,k,gg,dd]} 
} ## closes k in 1:data$nages
for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ for(k in 1:data$nages){for(gg in 1:data$ngenders){
  wig.scr[r,w,k,gg,dd] <- data$wig.parms}}}} #= runif(1, 0.01, 0.05) }}}}
  #wig.scr[k,g,d] ~ dunif(0.01, 0.05)

} ## closes (for dd in 1)


# added Dec7  -------------------------------------------------------------#
for(dd in 2){  for(k in 1:data$nages){  ## GC, any ages, F & M             #
  sscr.prior[,1,k,1,dd] = scr.prior[,1,k,1,dd] =  sscr.prior[,1,k,1,1]     #
  sscr.prior[,1,k,2,dd] = scr.prior[,1,k,2,dd] =  runif(dim.post, 0.1, 0.3)#
}}                                                                         #
  ##maxima gono                                                            #
for(dd in 2){for(k in 1:data$nages){                                       #
  sscr.prior[,data$brkpoint.scr,k,1,dd] = scr.prior[,data$brkpoint.scr,k,1,dd] = scr.prior[,data$brkpoint.scr,k,1,1] ## females are shared  
  sscr.prior[,data$brkpoint.scr,k,2,dd] = scr.prior[,data$brkpoint.scr,k,2,dd] = fctr[3,2]*scr.prior[,1,k,2,dd]  ## males gono all by the factor 1x3x multiplied
  for(gg in 1:data$ngenders){                                              #
    wig.scr[,,k,gg,dd] <- data$wig.parms #= wig.scr[,,k,gg,1]       ## shared wiggling as before
  }}}                                                                      #
# end addition ------------------------------------------------------------#
 
  
# # now sharing patterns ----------------------------------------------------
# if(RWs=='6'){
# # 6 RWs -------------------------------------------------------------------
# # I define specifically what happens for d==2
# for(gg in 1:data$ngenders){ for(k in 1:data$nages){  ##(gg in 1:data$ngenders) for non independent case
#   sscr.prior[,,k,gg,2] = scr.prior[,,k,gg,2]  = sscr.prior[,,k,gg,1]
#   sscr.prior[,data$brkpoint.scr,k,gg,2]  = scr.prior[,data$brkpoint.scr,k,gg,2] = sscr.prior[,data$brkpoint.scr,k,gg,1]
#   wig.scr[,,k,gg,2] = wig.scr[,,k,gg,1] ## same wiggling
# }}
# 
#     ## "independent CG wrt CT" for older males
#     ## put gg in 1 above then: (line 45)
#     # for(gg in 2){ for(k in 1:2){  ## share males only for ages 1-2 (age 3 is what it is)
#     #   sscr.prior[,,k,g,2] = scr.prior[,,k,gg,2] = sscr.prior[,,k,g,1]
#     #   sscr.prior[,data$brkpoint.scr, k,gg,2] = scr.prior[,data$brkpoint.scr, k,gg,2] = sscr.prior[,data$brkpoint.scr,k,gg,1]
#     #   wig.scr[,,k,gg,2] = wig.scr[,,k,gg,1] ## same wiggling
#     # } ## closes k=1:2
#     # ## gg=2, k=3
#     # sscr.prior[,1,3,gg,2] = scr.prior[,1,3,gg,2] = runif(dim.post, 0, 0.3)
#     # sscr.prior[,data$brkpoint.scr,3,gg,2] = scr.prior[,data$brkpoint.scr,3,gg,2] = fctr[3,gg]*scr.prior[,1,3,gg,2]
#     # for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){
#     # wig.scr[r,w,3,gg,2]= runif(1, 0.01, 0.05) }}
#     # } ## closes gg = 2
# } ## closes if "6"

# else if(RWs=='7'){
# # 7 RWs -------------------------------------------------------------------
# 
# for(dd in 2){for(gg in 1:data$ngenders){ for(k in 1:2){
#   sscr.prior[,,k,gg,dd] = scr.prior[,,k,gg,dd]   = sscr.prior[,,k,gg,1]
#   sscr.prior[,data$brkpoint.scr,k,gg,dd]  = scr.prior[,data$brkpoint.scr,k,gg,dd] = sscr.prior[,data$brkpoint.scr,k,gg,1]
#   wig.scr[,,k,gg,dd] = wig.scr[,,k,gg,1] ## same wiggling
# } } }
# 
#   sscr.prior[,,3,1,2] = scr.prior[,,3,1,2]   = sscr.prior[,,3,1,1]
#   sscr.prior[,data$brkpoint.scr,3,1,2]  = scr.prior[,data$brkpoint.scr,3,1,2] = sscr.prior[,data$brkpoint.scr,3,1,1]
#   wig.scr[,,3,1,2] = wig.scr[,,3,1,1] ## same wiggling
# 
#   sscr.prior[,1,3,2,2] = scr.prior[,1,3,2,2]   = runif(dim.post, 0, sscr.prior[,1,1,2,2])
#   sscr.prior[,data$brkpoint.scr,3,2,2]  = scr.prior[,data$brkpoint.scr,3,2,2] = sscr.prior[,1,3,2,2]*fctr[3,2]
#   wig.scr[,,3,2,2] = runif(dim.post, 0.01, 0.05) ## same wiggling
#   
#   for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ wig.scr[r,w,3,2,2]= runif(1, 0.01, 0.05) }}
# 
# }  else if(RWs=='8'){
# # 8 RWs -------------------------------------------------------------------
#   for(dd in 2){for(gg in 1:data$ngenders){ for(k in 1:2){
#     sscr.prior[,,k,gg,dd] = scr.prior[,,k,gg,dd]   = sscr.prior[,,k,gg,1]
#     sscr.prior[,data$brkpoint.scr,k,gg,dd]  = scr.prior[,data$brkpoint.scr,k,gg,dd] = sscr.prior[,data$brkpoint.scr,k,gg,1]
#     wig.scr[,,k,gg,dd] = wig.scr[,,k,gg,1] ## same wiggling
#   } } }
#   
#   sscr.prior[,1,3,1,2] = scr.prior[,1,3,1,2]   = runif(dim.post, 0, sscr.prior[,1,1,1,2])
#   sscr.prior[,data$brkpoint.scr,3,1,2]  = scr.prior[,data$brkpoint.scr,3,1,2] = sscr.prior[,1,3,2,2]*fctr[3,1]
#   for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ wig.scr[r,w,3,1,2]= runif(1, 0.01, 0.05) }}
#   
#   sscr.prior[,1,3,2,2] = scr.prior[,1,3,2,2]   = runif(dim.post, 0, sscr.prior[,1,1,2,2])
#   sscr.prior[,data$brkpoint.scr,3,2,2]  = scr.prior[,data$brkpoint.scr,3,2,2] = sscr.prior[,1,3,2,2]*fctr[3,2]
#   for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ wig.scr[r,w,3,2,2]= runif(1, 0.01, 0.05) }}
# 
# } else if(RWs=='9'){    
# # 9 RWs -------------------------------------------------------------------
#   for(k in 1:3){
#     sscr.prior[,,k,1,2] = scr.prior[,,k,1,2]   = sscr.prior[,,k,1,1]
#     sscr.prior[,data$brkpoint.scr,k,1,2]  = scr.prior[,data$brkpoint.scr,k,1,2] = sscr.prior[,data$brkpoint.scr,k,1,1]
#     wig.scr[,,k,1,2] = wig.scr[,,k,1,1] ## same wiggling
#   }
#   
#   sscr.prior[,1,2,2,2] = scr.prior[,1,2,2,2]   = dmultB*sscr.prior[,1,2,1,2]
#   sscr.prior[,data$brkpoint.scr,2,2,2]  = scr.prior[,data$brkpoint.scr,2,2,2] = sscr.prior[,1,2,2,2]*fctr[2,2]
#   
#   sscr.prior[,1,1,2,2] = scr.prior[,1,1,2,2]   = runif(dim.post, 0, sscr.prior[,1,2,2,2])
#   sscr.prior[,data$brkpoint.scr,1,2,2]  = scr.prior[,data$brkpoint.scr,1,2,2] = sscr.prior[,1,1,2,2]*fctr[1,2]
#   
#   sscr.prior[,1,3,2,2] = scr.prior[,1,3,2,2]   = runif(dim.post, 0, sscr.prior[,1,1,2,2])
#   sscr.prior[,data$brkpoint.scr,3,2,2]  = scr.prior[,data$brkpoint.scr,3,2,2] = sscr.prior[,1,3,2,2]*fctr[3,2]
#   
#   for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ for(k in 1:data$nages){ wig.scr[r,w,k,2,2]= runif(1, 0.01, 0.05) }}}
# } else if(RWs=='12'){  
# # 12 RWs -------------------------------------------------------------------
#   for(dd in 2){
#     
#     sscr.prior[,1,2,1,dd] = scr.prior[,1,2,1,dd] =  rbeta(dim.post, data$pbetascr[1], data$pbetascr[2]);
#     sscr.prior[,1,2,2,dd] = scr.prior[,1,2,2,dd] =  dmultB*sscr.prior[,1,2,1,dd]
#     
#     for(gg in 1:data$ngenders){
#     sscr.prior[,1,1,gg,dd] = scr.prior[,1,1,gg,dd] =  runif(dim.post, 0, sscr.prior[,1,2,gg,dd])
#     sscr.prior[,1,3,gg,dd] = scr.prior[,1,3,gg,dd] =  runif(dim.post, 0, sscr.prior[,1,1,gg,dd])
#     
#     for(k in 1:data$nages){
#     sscr.prior[,data$brkpoint.scr,k,gg,dd]  = scr.prior[,data$brkpoint.scr,k,gg,dd] = sscr.prior[,data$brkpoint.scr,k,gg,dd]*fctr[k,gg]
#  
#     for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ wig.scr[r,w,k,gg,dd]= runif(1, 0.01, 0.05)  }}}}}
# }   
# 

# sample ------------------------------------------------------------------
scr.prior = scr.prior[index.chose,,,,]

# slopes & RWs ------------------------------------------------------------
slM=NULL
for(dd in 1:data$ndis){for(gg in 1:data$ngenders){
slM=(scr.prior[, data$brkpoint.scr,,gg,dd]-scr.prior[,1,,gg,dd])/(data$brkpoint.scr-1) }

for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ for(k in 1:data$nages){for(gg in 1:data$ngenders){
  scr.prior[r,w,k,gg,dd] = ifelse(w%in%c(2:data$brkpoint.scr),
  max(0, min(scr.prior[r,data$brkpoint.scr,k,gg,dd], runif(1, scr.prior[r,w-1,k,gg,dd]+ slM[r,k]-wig.scr[r,w,k,gg,dd], scr.prior[r,w-1,k,gg,dd]+slM[r,k]+wig.scr[r,w,k,gg,dd]))),
  max(0, min(scr.prior[r,data$brkpoint.scr,k,gg,dd], runif(1, scr.prior[r,w-1,k,gg,dd]-wig.scr[r,w,k,gg,dd], scr.prior[r,w-1,k,gg,dd]+wig.scr[r,w,k,gg,dd]))))
}}}} }

# end ---------------------------------------------------------------------


# new code ----------------------------------------------------------------
# scr.prior = sscr.prior= NULL
# dmultB = runif(1,data$dmultB.parms[1],data$dmultB.parms[2])
# 
# # empty arrays ------------------------------------------------------------
# scr.prior=sscr.prior= wig.scr= array(
#   c(matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#     matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#     matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
#   dim=c(howmany, (data$n.single.years+data$pred),3, 2))
#   
# if(d==1 & g==1){
# # priors for CT -----------------------------------------------------------
#   ## indexes [,1,2,1] = timepoint 1, age group 2, gender 1
#   ## start with initial timepoints for females (i.e. [, 1, , 1])
#   sscr.prior[,1,2,1] = scr.prior[,1,2,1] =  rbeta(howmany, data$pbetascr[1], data$pbetascr[2]);
#   sscr.prior[,1,1,1] = scr.prior[,1,1,1] =  runif(howmany, 0, sscr.prior[,1,2,1])
#   sscr.prior[,1,3,1] = scr.prior[,1,3,1] =  runif(howmany, 0, sscr.prior[,1,1,1])
#   ## get initial timepoints for males rescaling (or whatever), i.e. [,1, , 2]
#   sscr.prior[,1,2,2] = scr.prior[,1,2,2] =  dmultB*sscr.prior[,1,2,1]
#   sscr.prior[,1,1,2] = scr.prior[,1,1,2] =  runif(howmany, 0, sscr.prior[,1,2,2])
#   
#   ##sscr.prior[,1,3,2] = scr.prior[,1,3,2] =  runif(howmany, 0, sscr.prior[,1,1,2])
#   sscr.prior[,1,3,2] = scr.prior[,1,3,2] =  runif(howmany, sscr.prior[,1,3,1]*0.5, sscr.prior[,1,3,1]*1.5) ## changed
#   
#   ## factors to get maxes
#   factor1=factor2=factor3=NULL
#   # factor by which to find the max (uniform(1,2))
# for(gg in 1:data$ngenders){
#   factor2[gg] = runif(1, data$factorscr[1], data$factorscr[2])
#   factor1[gg] = runif(1, 1, factor2[gg])
#   factor3[gg] = runif(1, 1, factor1[gg]) }
# 
#   ## maxes at breakpoint, for all age groups
#   sscr.prior[,data$brkpoint.scr,2,] = scr.prior[,data$brkpoint.scr,2,] = factor2*scr.prior[,1,2,]
#   sscr.prior[,data$brkpoint.scr,1,] = scr.prior[,data$brkpoint.scr,1,] = factor1*scr.prior[,1,1,]
#   sscr.prior[,data$brkpoint.scr,3,] = scr.prior[,data$brkpoint.scr,3,] = factor3*scr.prior[,1,3,]
# 
#   ## slopes
#   sl=NULL
#   sl=(sscr.prior[, data$brkpoint.scr,,]-sscr.prior[,1,,])/(data$brkpoint.scr-1)
# 
#   ## RWs
#   for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ for(k in 1:data$nages){for(gg in 1:data$ngenders){
#   wig.scr[r,w,k,gg]= runif(1, 0.01, 0.05)
#   scr.prior[r,w,k,gg] = ifelse(w%in%c(2:data$brkpoint.scr),
#       max(0, min(scr.prior[r,data$brkpoint.scr,k,gg], runif(1, scr.prior[r,w-1,k,gg]+ sl[r,k,gg]-wig.scr[r,w,k,gg], scr.prior[r,w-1,k,gg]+sl[r,k,gg]+wig.scr[r,w,k,gg]))),
#       max(0, min(scr.prior[r,data$brkpoint.scr,k,gg], runif(1, scr.prior[r,w-1,k,gg]-wig.scr[r,w,k,gg], scr.prior[r,w-1,k,gg]+wig.scr[r,w,k,gg]))))
#   }}}}
# 
# # save prior CT -----------------------------------------------------------
# save(scr.prior, file=paste(save.dir,screening.reporting,'scr_prior_d1.rda', sep=''))
# } else if(d==1 & g==2){  
#   load(file=paste(save.dir,screening.reporting,'scr_prior_d1.rda', sep='')) 
# } 
# 
# else if(d==2){
# # priors for GC decided by sharing scheme ---------------------------------
# 
# if(RWs=='6'){
# # save the priors for CT - done -------------------------------------------
#   load(file=paste(save.dir,screening.reporting,'scr_prior_d1.rda', sep=''))
# } else if(RWs=='7'){
# 
# # load priors for CT and then modify  -------------------------------------
# # need to modify [, ,3,2] (age group 3, for males GC, creating from scratch-
#   load(file=paste(save.dir,screening.reporting,'scr_prior_d1.rda', sep=''))
#   #scr.prior[,,3,2]
#   ## first point, overwrite
#   ## scr.prior[,1,3,2] =  runif(howmany, 0, scr.prior[,1,1,2])
#   
#   scr.prior[,1,3,2] =  runif(howmany, scr.prior[,1,3,1]*0.5, scr.prior[,1,3,1]*1.5) ## changed
#   
#   ## maximum
#   scr.prior[,data$brkpoint.scr,3,2] = factor3[2]*scr.prior[,1,3,2]
#   ## slope
#   sl32=NULL
#   sl32=(scr.prior[, data$brkpoint.scr,3,2]-scr.prior[,1,3,2])/(data$brkpoint.scr-1)
#   ## RW
#   for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ for(k in 3){for(gg in 2){
#     wig.scr[r,w,k,gg]= runif(1, 0.01, 0.05)
#     scr.prior[r,w,k,gg] = ifelse(w%in%c(2:data$brkpoint.scr),
#       max(0, min(scr.prior[r,data$brkpoint.scr,k,gg], runif(1, scr.prior[r,w-1,k,gg]+ sl32[r]-wig.scr[r,w,k,gg], scr.prior[r,w-1,k,gg]+sl32[r]+wig.scr[r,w,k,gg]))),
#       max(0, min(scr.prior[r,data$brkpoint.scr,k,gg], runif(1, scr.prior[r,w-1,k,gg]-wig.scr[r,w,k,gg], scr.prior[r,w-1,k,gg]+wig.scr[r,w,k,gg]))))
#   }}}}
# # that’s the new scr.prior for males, age group 3 -------------------------
# 
# } else if(RWs=='8'){
# 
# # load priors for CT and then modify  -------------------------------------
# # need to modify [, ,3,1:2] (age group 3, for females AND males GC, creating from scratch-
#   load(file=paste(save.dir,screening.reporting,'scr_prior_d1.rda', sep=''))
#   ## first point, overwrite
#   
#   ##scr.prior[,1,3,] =  runif(howmany, 0, scr.prior[,1,1,])
#   scr.prior[,1,3,1] =  runif(howmany, 0, scr.prior[,1,1,1])  ## changed
#   scr.prior[,1,3,2] =  runif(howmany, scr.prior[,1,3,1]*0.5, scr.prior[,1,3,1]*1.5) ## changed
#   
#   ## maximum
#   scr.prior[,data$brkpoint.scr,3,] = factor3*scr.prior[,1,3,]
#   ## slope
#   sl3=NULL
#   sl3=(scr.prior[, data$brkpoint.scr,3,]-scr.prior[,1,3,])/(data$brkpoint.scr-1)
#   ## RW
#   for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ for(k in 3){for(gg in 1:2){
#     wig.scr[r,w,k,gg]= runif(1, 0.01, 0.05)
#     scr.prior[r,w,k,gg] = ifelse(w%in%c(2:data$brkpoint.scr),
#       max(0, min(scr.prior[r,data$brkpoint.scr,k,gg], runif(1, scr.prior[r,w-1,k,gg]+ sl3[r,gg]-wig.scr[r,w,k,gg], scr.prior[r,w-1,k,gg]+sl3[r,gg]+wig.scr[r,w,k,gg]))),
#       max(0, min(scr.prior[r,data$brkpoint.scr,k,gg], runif(1, scr.prior[r,w-1,k,gg]-wig.scr[r,w,k,gg], scr.prior[r,w-1,k,gg]+wig.scr[r,w,k,gg]))))
#   }}}}
# 
# } else if(RWs=='9'){
# 
# # load priors for CT and then modify  -------------------------------------
# # need to modify [, ,1:3,2] (age group 1:3, for males GC, creating from scratch-
#   load(file=paste(save.dir,screening.reporting,'scr_prior_d1.rda', sep=''))
#   ## first point, overwrite
#   scr.prior[,1,2,2] =  dmultB*scr.prior[,1,2,1]
#   scr.prior[,1,1,2] =  runif(howmany, 0, scr.prior[,1,2,2])
#   #scr.prior[,1,3,2] =  runif(howmany, 0, scr.prior[,1,1,2])
#   scr.prior[,1,3,2] =  runif(howmany, scr.prior[,1,3,1]*0.5, scr.prior[,1,3,1]*1.5) ## changed
#   
#   ## maxima
#   scr.prior[,data$brkpoint.scr,2,2] = factor2[2]*scr.prior[,1,2,2]
#   scr.prior[,data$brkpoint.scr,1,2] = factor1[2]*scr.prior[,1,1,2]
#   scr.prior[,data$brkpoint.scr,3,2] = factor3[2]*scr.prior[,1,3,2]
# 
#   ## slope
#   slM=NULL
#   slM=(scr.prior[, data$brkpoint.scr,,2]-scr.prior[,1,,2])/(data$brkpoint.scr-1)
#   ## RW
#   for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ for(k in 1:3){for(gg in 2){
#     wig.scr[r,w,k,gg]= runif(1, 0.01, 0.05)
#     scr.prior[r,w,k,gg] = ifelse(w%in%c(2:data$brkpoint.scr),
#        max(0, min(scr.prior[r,data$brkpoint.scr,k,gg], runif(1, scr.prior[r,w-1,k,gg]+ slM[r,k]-wig.scr[r,w,k,gg], scr.prior[r,w-1,k,gg]+slM[r,k]+wig.scr[r,w,k,gg]))),
#        max(0, min(scr.prior[r,data$brkpoint.scr,k,gg], runif(1, scr.prior[r,w-1,k,gg]-wig.scr[r,w,k,gg], scr.prior[r,w-1,k,gg]+wig.scr[r,w,k,gg]))))
#   }}}}
# }
# } ## closes if(d==2)
# end new code ------------------------------------------------------------




# # old code starts here ----------------------------------------------------
# scr.prior = sscr.prior= NULL
# dmultB = runif(1,data$dmultB.parms[1],data$dmultB.parms[2])
# 
# if(length(grep('S1',case))==1){
#   
#   scr.prior=sscr.prior= wig.scr= array(
#       c(matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#         matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#         matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
#         dim=c(howmany, (data$n.single.years+data$pred),3, 2))
# 
#     sscr.prior[,1,2,1] = scr.prior[,1,2,1] =  rbeta(howmany, data$pbetascr[1], data$pbetascr[2]);
#     sscr.prior[,1,1,1] = scr.prior[,1,1,1] =  runif(howmany, 0, sscr.prior[,1,2,1])
#     sscr.prior[,1,3,1] = scr.prior[,1,3,1] =  runif(howmany, 0, sscr.prior[,1,1,1])
# 
#     sscr.prior[,1,2,2] = scr.prior[,1,2,2] =  dmultB*sscr.prior[,1,2,1]
#     sscr.prior[,1,1,2] = scr.prior[,1,1,2] =  runif(howmany, 0, sscr.prior[,1,2,2])
#     sscr.prior[,1,3,2] = scr.prior[,1,3,2] =  runif(howmany, 0, sscr.prior[,1,1,2])
# 
#     factor1=factor2=factor3=NULL
#     # factor by which to find the max (uniform(1,2))
#     for(gg in 1:data$ngenders){
#     factor2[gg] = runif(1, data$factorscr[1], data$factorscr[2])
#     factor1[gg] = runif(1, 1, factor2[gg])
#     factor3[gg] = runif(1, 1, factor1[gg]) }
# 
#   sscr.prior[,data$brkpoint.scr,2,] = scr.prior[,data$brkpoint.scr,2,] = factor2*scr.prior[,1,2,]
#   sscr.prior[,data$brkpoint.scr,1,] = scr.prior[,data$brkpoint.scr,1,] = factor1*scr.prior[,1,1,]
#   sscr.prior[,data$brkpoint.scr,3,] = scr.prior[,data$brkpoint.scr,3,] = factor3*scr.prior[,1,3,]
# 
#   sl=NULL
#   sl=(sscr.prior[, data$brkpoint.scr,,]-sscr.prior[,1,,])/(data$brkpoint.scr-1)
#   
#   for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ for(k in 1:data$nages){for(gg in 1:data$ngenders){
#   wig.scr[r,w,k,gg]= runif(1, 0.01, 0.05)
#   scr.prior[r,w,k,gg] = ifelse(w%in%c(2:data$brkpoint.scr),
#     max(0, min(scr.prior[r,data$brkpoint.scr,k,gg], runif(1, scr.prior[r,w-1,k,gg]+ sl[r,k,gg]-wig.scr[r,w,k,gg], scr.prior[r,w-1,k,gg]+sl[r,k,gg]+wig.scr[r,w,k,gg]))),
#     max(0, min(scr.prior[r,data$brkpoint.scr,k,gg], runif(1, scr.prior[r,w-1,k,gg]-wig.scr[r,w,k,gg], scr.prior[r,w-1,k,gg]+wig.scr[r,w,k,gg]))))
#   }}}}
#   #####################################
# } else if(length(grep('S2',case))==1){
# 
# if(d==1){
#   
#   scr.prior=sscr.prior= wig.scr= array(
#     c(matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#       matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#       matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
#     dim=c(howmany, (data$n.single.years+data$pred),3, 2))
#   
#   sscr.prior[,1,2,1] = scr.prior[,1,2,1] =  rbeta(howmany, data$pbetascr[1], data$pbetascr[2]);
#   sscr.prior[,1,1,1] = scr.prior[,1,1,1] =  runif(howmany, 0, sscr.prior[,1,2,1])
#   sscr.prior[,1,3,1] = scr.prior[,1,3,1] =  runif(howmany, 0, sscr.prior[,1,1,1])
#   
#   sscr.prior[,1,2,2] = scr.prior[,1,2,2] =  dmultB*sscr.prior[,1,2,1]
#   sscr.prior[,1,1,2] = scr.prior[,1,1,2] =  runif(howmany, 0, sscr.prior[,1,2,2])
#   sscr.prior[,1,3,2] = scr.prior[,1,3,2] =  runif(howmany, 0, sscr.prior[,1,1,2])
#   
#   factor1=factor2=factor3=NULL
#   # factor by which to find the max (uniform(1,2))
#   for(gg in 1:data$ngenders){
#     factor2[gg] = runif(1, data$factorscr[1], data$factorscr[2])
#     factor1[gg] = runif(1, 1, factor2[gg])
#     factor3[gg] = runif(1, 1, factor1[gg]) }
#   
#   sscr.prior[,data$brkpoint.scr,2,] = scr.prior[,data$brkpoint.scr,2,] = factor2*scr.prior[,1,2,]
#   sscr.prior[,data$brkpoint.scr,1,] = scr.prior[,data$brkpoint.scr,1,] = factor1*scr.prior[,1,1,]
#   sscr.prior[,data$brkpoint.scr,3,] = scr.prior[,data$brkpoint.scr,3,] = factor3*scr.prior[,1,3,]
#   
#   sl=NULL
#   sl=(sscr.prior[, data$brkpoint.scr,,]-sscr.prior[,1,,])/(data$brkpoint.scr-1)
#   
#   for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ for(k in 1:data$nages){for(gg in 1:data$ngenders){
#     wig.scr[r,w,k,gg]= runif(1, 0.01, 0.05)
#     scr.prior[r,w,k,gg] = ifelse(w%in%c(2:data$brkpoint.scr),
#                                  max(0, min(scr.prior[r,data$brkpoint.scr,k,gg], runif(1, scr.prior[r,w-1,k,gg]+ sl[r,k,gg]-wig.scr[r,w,k,gg], scr.prior[r,w-1,k,gg]+sl[r,k,gg]+wig.scr[r,w,k,gg]))),
#                                  max(0, min(scr.prior[r,data$brkpoint.scr,k,gg], runif(1, scr.prior[r,w-1,k,gg]-wig.scr[r,w,k,gg], scr.prior[r,w-1,k,gg]+wig.scr[r,w,k,gg]))))
#   }}}}  
#   
# save(scr.prior, file=paste(results.date.dir,'scr_prior_d1.rda', sep=''))  
# } else if(d==2){
# load(file=paste(results.date.dir,'scr_prior_d1.rda', sep=''))  } 
#   
# }
# # ends here ---------------------------------------------------------------





# # 1. shared slopes, different walks
# ## blank priors: array 25(samples)x21(timepoints)x3(agegroups)x2(genders)
# scr.prior=sscr.prior= array(
#     c(matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)), 
#       matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)), 
#       matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
#       dim=c(howmany, (data$n.single.years+data$pred),3, 2))
# 
#   sscr.prior[,1,2,1] = scr.prior[,1,2,1] =  rbeta(howmany, data$pbetascr[1], data$pbetascr[2]); 
#   sscr.prior[,1,1,1] = scr.prior[,1,1,1] =  runif(howmany, 0, sscr.prior[,1,2,1])
#   sscr.prior[,1,3,1] = scr.prior[,1,3,1] =  runif(howmany, 0, sscr.prior[,1,1,1])
# 
#   sscr.prior[,1,2,2] = scr.prior[,1,2,2] =  dmultB*sscr.prior[,1,2,1]
#   sscr.prior[,1,1,2] = scr.prior[,1,1,2] =  runif(howmany, 0, sscr.prior[,1,2,2])
#   sscr.prior[,1,3,2] = scr.prior[,1,3,2] =  runif(howmany, 0, sscr.prior[,1,1,2])
# 
#   factor1=factor2=factor3=NULL
#   # factor by which to find the max (uniform(1,2))
#   for(s in 1:data$ngenders){ 
#   factor2[s] = runif(1, data$factorscr[1], data$factorscr[2])
#   factor1[s] = runif(1, 1, factor2[s])
#   factor3[s] = runif(1, 1, factor1[s]) }
# 
# sscr.prior[,data$brkpoint.scr,2,] = scr.prior[,data$brkpoint.scr,2,] = factor2*scr.prior[,1,2,]
# sscr.prior[,data$brkpoint.scr,1,] = scr.prior[,data$brkpoint.scr,1,] = factor1*scr.prior[,1,1,]
# sscr.prior[,data$brkpoint.scr,3,] = scr.prior[,data$brkpoint.scr,3,] = factor3*scr.prior[,1,3,]
# 
# #########################################
# 
# if(d==1){
# sl=NULL
# sl=(sscr.prior[, data$brkpoint.scr,,]-sscr.prior[,1,,])/(data$brkpoint.scr-1)
# slope = apply(sl,c(2,3),mean)
# save(sl, file=paste(getwd(),'/slopes.rda', sep='')) }else if(d==2){load(file=paste(getwd(),'/slopes.rda', sep=''))}
# 
# for(r in 1:nrow(scr.prior)){
#   for(w in 2:(data$n.single.years+pred)){
#     for(k in 1:3){
# scr.prior[r,w,k,g] = ifelse(w%in%c(2:data$brkpoint.scr),
#     max(0, min(scr.prior[r,data$brkpoint.scr,k,g], runif(1, scr.prior[r,w-1,k,g]+ sl[r,k,g]-wig.scr, scr.prior[r,w-1,k,g]+sl[r,k,g]+wig.scr))),
#     max(0, min(scr.prior[r,data$brkpoint.scr,k,g], runif(1, scr.prior[r,w-1,k,g]-wig.scr, scr.prior[r,w-1,k,g]+wig.scr))))
# }}}
#####################################
# ## check
#  k=2; g=1; r=1;
# ## line (to verify slopes)
# plot(c(1,13), c(scr.prior[r,1,k,g], scr.prior[r,data$brkpoint.scr,k,g]),
#  pch=19, col='black', xlim=c(1,21), ylim=c(0, 0.5))
# ##
# for(w in 2:(data$n.single.years+pred)){scr.prior[r,w,k,g] = ifelse(w%in%c(2:data$brkpoint.scr),
# scr.prior[r,w-1,k,g]+ sl[r,k,g], scr.prior[r,w-1,k,g])}
# lines(2:(data$n.single.years+pred),scr.prior[r,2:(data$n.single.years+pred),k,g], col='red' )
#
# for(w in 2:(data$n.single.years+pred)){
#   scr.prior[r,w,k,g] = ifelse(w%in%c(2:data$brkpoint.scr),
#   runif(1, scr.prior[r,w-1,k,g]+ sl[r,k,g]-wig.scr, scr.prior[r,w-1,k,g]+sl[r,k,g]+wig.scr),
#   runif(1, scr.prior[r,w-1,k,g]-wig.scr, scr.prior[r,w-1,k,g]+wig.scr))
# }
# lines(1:(data$n.single.years+pred),scr.prior[r,1:(data$n.single.years+pred),k,g], col='green' )
#
#
# for(w in 2:(data$n.single.years+pred)){
#   scr.prior[r,w,k,g] = ifelse(w%in%c(2:data$brkpoint.scr),
#   max(0, min(scr.prior[r,data$brkpoint.scr,k,g], runif(1, scr.prior[r,w-1,k,g]+ slope[k,g]-wig.scr, scr.prior[r,w-1,k,g]+slope[k,g]+wig.scr))),
#   max(0, min(scr.prior[r,data$brkpoint.scr,k,g], runif(1, scr.prior[r,w-1,k,g]-wig.scr, scr.prior[r,w-1,k,g]+wig.scr))))
# }
# lines(2:(data$n.single.years+pred),scr.prior[r,2:(data$n.single.years+pred),k,g], col='blue' )
#################################################
# if(d==1){
# sl=NULL
# sl=(sscr.prior[, data$brkpoint.scr,,]-sscr.prior[,1,,])/(data$brkpoint.scr-1)
# 
#   #slope = apply(sl,c(2,3),mean)
#   #save(sl, file=paste(getwd(),'/slopes.rda', sep='')) #}else if(d==2){load(file=paste(getwd(),'/slopes.rda', sep=''))}
# 
# for(r in 1:nrow(scr.prior)){ for(w in 2:(data$n.single.years+pred)){ for(k in 1:3){
#   scr.prior[r,w,k,g] = ifelse(w%in%c(2:data$brkpoint.scr),
#   max(0, min(scr.prior[r,data$brkpoint.scr,k,g], runif(1, scr.prior[r,w-1,k,g]+ sl[r,k,g]-wig.scr, scr.prior[r,w-1,k,g]+sl[r,k,g]+wig.scr))),
#   max(0, min(scr.prior[r,data$brkpoint.scr,k,g], runif(1, scr.prior[r,w-1,k,g]-wig.scr, scr.prior[r,w-1,k,g]+wig.scr))))
# }}}
# 
# save(scr.prior, file=paste(getwd(),'/screenpriors.rda', sep='')) } else if(d==2){
#   load(file=paste(getwd(),'/screenpriors.rda', sep='')); scr.prior=scr.prior
#   }








