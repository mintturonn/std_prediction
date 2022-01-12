comprep.priors = function(g,d){

# beginning new code ------------------------------------------------------
# rep0 = array(c(matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#       matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#       matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
#       dim=c(howmany, (data$n.single.years+data$pred),3, 2))
# 
# # generate the 6 for CT ---------------------------------------------------
# for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#  rep0[,1,kk,gg]=runif(howmany, 0.6, 0.8)
#  rep0[,data$brkpoint.rep,kk,gg]=runif(howmany,0.9, 0.95) 

# slopes ------------------------------------------------------------------
# slrep = NULL
# slrep=(rep0[, data$brkpoint.scr,,]-rep0[,1,,])/(data$brkpoint.scr-1)
# 
# save(rep0, file=paste(save.dir,screening.reporting,'rep0_d1.rda', sep=''))
# save(slrep, file=paste(save.dir,screening.reporting,'slrep_d1.rda', sep=''))  

# if(RWs_CR=='6'){
# # 6 random walks ----------------------------------------------------------
# ## means that CT is shared with GC
#   if(d==1){
#     
#     wig.rep=matrix(rep(NA, data$nages*data$ngenders), byrow=T, ncol=data$ngenders)
#     
#     for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#       wig.rep[kk,gg] = runif(1, 0.01, 0.05)
#       save(wig.rep, file=paste0(save.dir,'wig.rep1.rda'))
#       
#     for(rr in 1:nrow(rep0)){for(ww in 2:(data$n.single.years+pred)){
#       rep0[rr,ww,kk,gg] = ifelse(ww%in%c(2:data$brkpoint.rep),
#           min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]+ slrep[rr,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+slrep[rr,kk,gg]+wig.rep[kk,gg])),
#           min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+wig.rep[kk,gg])))  
#     }}}}
#     
#     save(rep0, file=paste0(save.dir,screening.reporting,'rep01.rda'))
# 
#   } else if(d==2){ load(file=paste(save.dir,screening.reporting,'rep01.rda', sep=''))  } 
# } else if(RWs_CR=='12'){
# 12 random walks ---------------------------------------------------------
## means all different (as before)
if(d==1){
    
# null array --------------------------------------------------------------
rep0 = array(c(matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
       matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
       matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
       dim=c(howmany, (data$n.single.years+data$pred),3, 2))
    
# first and max -----------------------------------------------------------
  rep0[,1,,] = runif(howmany, 0.6, 0.8)
  rep0[,data$brkpoint.rep,,]=runif(howmany,0.9, 0.95)
  
  #for(gg in 1:data$ngenders){ #for(kk in 1:data$nages){
   #    rep0[,1,kk,gg] = runif(howmany, 0.6, 0.8)
   #   rep0[,data$brkpoint.rep,kk,gg]=runif(howmany,0.9, 0.95) #}
   #}

# slope for reporting -----------------------------------------------------
   slrep = NULL
   slrep=(rep0[, data$brkpoint.scr,,]-rep0[,1,,])/(data$brkpoint.scr-1)
    
   save(rep0, file=paste0(save.dir,screening.reporting,'rep0_d1.rda'));
   save(slrep, file=paste0(save.dir,screening.reporting,'slrep_d1.rda'))  
    
   wig.rep <- matrix(rep(data$wig.parms, data$nages*data$ngenders), byrow=T, ncol=data$ngenders)
    
  for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
      #wig.rep[kk,gg] <- data$wig.parms #= runif(1, 0.01, 0.05)
  for(rr in 1:nrow(rep0)){for(ww in 2:(data$n.single.years+pred)){
      rep0[rr,ww,kk,gg] = ifelse(ww%in%c(2:data$brkpoint.rep),
      max(rep0[rr,1,kk,gg] , min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]+ slrep[rr,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+slrep[rr,kk,gg]+wig.rep[kk,gg]))),
      max(rep0[rr,1,kk,gg] , min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+wig.rep[kk,gg]))))  
      }}}}  
    
  } else if(d==2){
    
#null array --------------------------------------------------------------
rep0 = array(c(matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
       matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
       matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
       dim=c(howmany, (data$n.single.years+data$pred),3, 2))
      
# first and max -----------------------------------------------------------
    rep0[,1,,] = runif(howmany, 0.7, 0.95)
    rep0[,data$brkpoint.rep,,]=runif(howmany,0.9, 0.95)

# for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#    rep0[,1,kk,gg]=runif(howmany, 0.7, 0.95)
#    rep0[,data$brkpoint.rep,kk,gg]=runif(howmany,0.9, 0.95) } }
      
   # slope for reporting -----------------------------------------------------
   slrep = NULL
   slrep=(rep0[, data$brkpoint.scr,,]-rep0[,1,,])/(data$brkpoint.scr-1)
      
   #save(rep0, file=paste0(save.dir,screening.reporting,'rep0_d1.rda'));
   #save(slrep, file=paste0(save.dir,screening.reporting,'slrep_d1.rda'))  
      
   wig.rep <- matrix(rep(data$wig.parms, data$nages*data$ngenders), byrow=T, ncol=data$ngenders)
      
   for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
   for(rr in 1:nrow(rep0)){for(ww in 2:(data$n.single.years+pred)){
   rep0[rr,ww,kk,gg] = ifelse(ww%in%c(2:data$brkpoint.rep),
        max(rep0[rr,1,kk,gg] , min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]+ slrep[rr,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+slrep[rr,kk,gg]+wig.rep[kk,gg]))),
        max(rep0[rr,1,kk,gg] , min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+wig.rep[kk,gg]))))  
   }}}}  
      
  #load(file=paste0(save.dir,screening.reporting,'rep0_d1.rda'))  
  #load(file=paste0(save.dir,screening.reporting,'slrep_d1.rda'))  
  #wig.rep <- matrix(rep(data$wig.parms, data$nages*data$ngenders), byrow=T, ncol=data$ngenders) ##=matrix(rep(NA, data$nages*data$ngenders), byrow=T, ncol=data$ngenders)
  #for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
  #for(rr in 1:nrow(rep0)){for(ww in 2:(data$n.single.years+pred)){
  #    rep0[rr,ww,kk,gg] = ifelse(ww%in%c(2:data$brkpoint.rep),
  #    max(rep0[rr,1,kk,gg] , min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]+ slrep[rr,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+slrep[rr,kk,gg]+wig.rep[kk,gg]))),
  #    max(rep0[rr,1,kk,gg] , min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+wig.rep[kk,gg]))) )
  #}}}}  
} # closes if(d==2) 

#}
# End new code ------------------------------------------------------------

return(rep0)
}  
  
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# if(length(grep('R1',case))==1){
# 
#     rep0 = array(c(matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#     matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#     matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
#     dim=c(howmany, (data$n.single.years+data$pred),3, 2))
#     for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#     rep0[,1,kk,gg]=runif(howmany, 0.6, 0.8)
#     rep0[,data$brkpoint.rep,kk,gg]=runif(howmany,0.9, 0.95) }}
#   
#     slrep = NULL
#     slrep=(rep0[, data$brkpoint.scr,,]-rep0[,1,,])/(data$brkpoint.scr-1)
# 
#     wig.rep=matrix(rep(NA, data$nages*data$ngenders), byrow=T, ncol=data$ngenders)
# 
#     for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#     wig.rep[kk,gg] = runif(1, 0.01, 0.05)
#     for(rr in 1:nrow(rep0)){for(ww in 2:(data$n.single.years+pred)){
#     rep0[rr,ww,kk,gg] = ifelse(ww%in%c(2:data$brkpoint.rep),
#       min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]+ slrep[rr,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+slrep[rr,kk,gg]+wig.rep[kk,gg])),
#       min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+wig.rep[kk,gg])))  
#     }}}}  
#     
# } else if(length(grep('R2',case))==1){
# ## 2.Shared Start Value & Slope
#   
# if(d==1){
# 
#     rep0 = array(c(matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#     matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#     matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
#     dim=c(howmany, (data$n.single.years+data$pred),3, 2))
#     for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#     rep0[,1,kk,gg]=runif(howmany, 0.6, 0.8)
#     rep0[,data$brkpoint.rep,kk,gg]=runif(howmany,0.9, 0.95) }}
#     slrep = NULL
#     slrep=(rep0[, data$brkpoint.scr,,]-rep0[,1,,])/(data$brkpoint.scr-1)
# 
#     save(rep0, file=paste(results.date.dir,'rep0_d1.rda', sep=''))  
#     save(slrep, file=paste(results.date.dir,'slrep_d1.rda', sep=''))  
#     
#     wig.rep=matrix(rep(NA, data$nages*data$ngenders), byrow=T, ncol=data$ngenders)
#     
#     for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#       wig.rep[kk,gg] = runif(1, 0.01, 0.05)
#     for(rr in 1:nrow(rep0)){for(ww in 2:(data$n.single.years+pred)){
#       rep0[rr,ww,kk,gg] = ifelse(ww%in%c(2:data$brkpoint.rep),
#         min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]+ slrep[rr,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+slrep[rr,kk,gg]+wig.rep[kk,gg])),
#         min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+wig.rep[kk,gg])))  
#     }}}}  
# 
# } else if(d==2){
# load(file=paste(results.date.dir,'rep0_d1.rda', sep=''))  
# load(file=paste(results.date.dir,'slrep_d1.rda', sep=''))  
# 
# wig.rep=matrix(rep(NA, data$nages*data$ngenders), byrow=T, ncol=data$ngenders)
# 
#     for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#       wig.rep[kk,gg] = runif(1, 0.01, 0.05)
#     for(rr in 1:nrow(rep0)){for(ww in 2:(data$n.single.years+pred)){
#       rep0[rr,ww,kk,gg] = ifelse(ww%in%c(2:data$brkpoint.rep),
#         min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]+ slrep[rr,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+slrep[rr,kk,gg]+wig.rep[kk,gg])),
#         min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+wig.rep[kk,gg])))  
#     }}}}  
# 
# } 
#   
# } else if(length(grep('R3',case))==1){
# ## 3.Fully Shared Start Value & Slope  
# if(d==1){
#     rep0 = array(c(matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#       matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#       matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
#       dim=c(howmany, (data$n.single.years+data$pred),3, 2))
#       rep01 = runif(howmany, 0.6, 0.8); rep0max= runif(howmany,0.9, 0.95)
# 
#     for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#       rep0[,1,kk,gg]= rep01
#       rep0[,data$brkpoint.rep,kk,gg]=rep0max }}
# 
#     slrep = NULL
#     slrep=(rep0[, data$brkpoint.scr,,]-rep0[,1,,])/(data$brkpoint.scr-1)
#   
#     save(rep0, file=paste(results.date.dir,'rep0_d1.rda', sep=''))  
#     save(slrep, file=paste(results.date.dir,'slrep_d1.rda', sep=''))  
# 
#     wig.rep=matrix(rep(NA, data$nages*data$ngenders), byrow=T, ncol=data$ngenders)
# 
#     for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#       wig.rep[kk,gg] = runif(1, 0.01, 0.05)
#     for(rr in 1:nrow(rep0)){for(ww in 2:(data$n.single.years+pred)){
#       rep0[rr,ww,kk,gg] = ifelse(ww%in%c(2:data$brkpoint.rep),
#         min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]+ slrep[rr,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+slrep[rr,kk,gg]+wig.rep[kk,gg])),
#         min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+wig.rep[kk,gg])))  
#     }}}}  
# 
# } else if(d==2){
#   load(file=paste(results.date.dir,'rep0_d1.rda', sep=''))  
#   load(file=paste(results.date.dir,'slrep_d1.rda', sep=''))  
#   wig.rep=matrix(rep(NA, data$nages*data$ngenders), byrow=T, ncol=data$ngenders)
#   
#   for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#     wig.rep[kk,gg] = runif(1, 0.01, 0.05)
#   for(rr in 1:nrow(rep0)){for(ww in 2:(data$n.single.years+pred)){
#     rep0[rr,ww,kk,gg] = ifelse(ww%in%c(2:data$brkpoint.rep),
#       min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]+ slrep[rr,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+slrep[rr,kk,gg]+wig.rep[kk,gg])),
#       min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+wig.rep[kk,gg])))  
#   }}}}  
# }
# 
# } else if(length(grep('R4',case))==1){
# ## 4.Shared walk  
# if(d==1){
#   rep0 = array(c(matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#     matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#     matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
#     dim=c(howmany, (data$n.single.years+data$pred),3, 2))
#   for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#    rep0[,1,kk,gg]=runif(howmany, 0.6, 0.8)
#    rep0[,data$brkpoint.rep,kk,gg]=runif(howmany,0.9, 0.95) }}
#   slrep = NULL
#   slrep=(rep0[, data$brkpoint.scr,,]-rep0[,1,,])/(data$brkpoint.scr-1)
# 
#   wig.rep=matrix(rep(NA, data$nages*data$ngenders), byrow=T, ncol=data$ngenders)
#     
#   for(gg in 1:data$ngenders){ for(kk in 1:data$nages){
#     wig.rep[kk,gg] = runif(1, 0.01, 0.05)
#   for(rr in 1:nrow(rep0)){for(ww in 2:(data$n.single.years+pred)){
#     rep0[rr,ww,kk,gg] = ifelse(ww%in%c(2:data$brkpoint.rep),
#       min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]+ slrep[rr,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+slrep[rr,kk,gg]+wig.rep[kk,gg])),
#       min(rep0[rr,data$brkpoint.rep,kk,gg],runif(1, rep0[rr,ww-1,kk,gg]-wig.rep[kk,gg], rep0[rr,ww-1,kk,gg]+wig.rep[kk,gg])))  
#   }}}}  
# save(rep0, file=paste(results.date.dir,'rep0_d1.rda', sep=''))  
# 
# } else if(d==2){ load(file=paste(results.date.dir,'rep0_d1.rda', sep='')) }   
# 
# } else if(length(grep('R5',case))==1){
# ## 5.Fully shared walk
# if(d==1){
#   rep0 =  array(c(matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#     matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
#     matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
#     dim=c(howmany, (data$n.single.years+data$pred),3, 2))
#   rrep0 = matrix(rep(NA, howmany*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))
#   rrep01 = runif(howmany, 0.6, 0.8); rrep0max= runif(howmany,0.9, 0.95)
#   
#   rrep0[,1]= rrep01
#   rrep0[,data$brkpoint.rep]=rrep0max
#   
#   slrep = NULL
#   slrep=(rrep0[, data$brkpoint.scr]-rrep0[,1])/(data$brkpoint.scr-1)
#   
#   wig.rep=NULL
#   wig.rep = runif(1, 0.01, 0.05)
# 
#   for(rr in 1:nrow(rep0)){for(ww in 2:(data$n.single.years+pred)){
#     rrep0[rr,ww] = ifelse(ww%in%c(2:data$brkpoint.rep),
#       min(rrep0[rr,data$brkpoint.rep],runif(1, rrep0[rr,ww-1]+ slrep-wig.rep, rrep0[rr,ww-1]+slrep+wig.rep)),
#       min(rrep0[rr,data$brkpoint.rep],runif(1, rrep0[rr,ww-1]-wig.rep, rrep0[rr,ww-1]+wig.rep)))  
#   }}  
#   
# for(gg in 1:data$ngenders){for(kk in 1:data$nages){rep0[,,kk,gg]=rrep0[,] }}  
# save(rep0, file=paste(results.date.dir,'rep0_d1.rda', sep=''))
# } else if(d==2){ load(file=paste(results.date.dir,'rep0_d1.rda', sep=''))}
# } 
