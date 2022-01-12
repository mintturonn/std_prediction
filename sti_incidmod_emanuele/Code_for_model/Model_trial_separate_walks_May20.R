rm(list=ls())
lapply(c('rjags','coda','foreign','splines'), require, character.only = TRUE)

bcb=''

# Directories -------------------------------------------------------------
### please change these according to where the code ('code.dir') and the data ('data.dir')
### are stored
overall.dir = paste0(bcb,"/homes/mazzola/")
code.dir    = paste0(bcb,"/homes/mazzola/RCode/CDC_code_May20/")
data.dir    = paste0(bcb,"/homes/mazzola/Datafiles/2020-05-11/")

# Load transformed data ---------------------------------------------------
setwd(code.dir)
source(file=paste0(getwd(),'/Code_for_data_prep/','data_prep_for_mcmc.R'))

# Go back to main directory -----------------------------------------------
setwd(overall.dir)

# Main parameters for MCMC ------------------------------------------------
niter = 200000                    # number iteration for the RJags model
niterK = paste0(niter/1000,'K')
NC    = 1                         # number of chains
pred  = 2                         # years predicted out after last datapoint
popul.factor = 10                 # fraction of populations for sensitivity analysis on case reports
                                  # 1= full population, 2= half the population, ... 10= 1/10 population
screening.reporting = '6_12_5pct_all' #name string identifier to add at the end of each saved file 

NRows = nrow(CR) + pred           # overall number of years in play (observed+predicted)

# Fixed parameters for prior distributions --------------------------------
pbetascr  = c(10,40)  

## pdt.C	 = c(4,4)		
## pdu.C   = c(4,4.696)
## pdt.G   = c(4,4)  ## same as CT
## pdu.G	= c(4,7.4)  	 


# Base parameter for 'rescaled beta' distributions -------------------------
### dt = duration treated infection
### du = duration untreated infection
### .C = Chlamydia
### .G = Gonorrhea
pdt.C	    = c(1,1)		
pdu.C     = c(1,1)
pdt.G     = c(1,1)
pdu.G		  = c(1,1)  	 


# Breakpoint for random walks for completeness of reporting/screening ------
comp.rep.brk = scr.brk = which(c(1999:2019)==2011)

# Interval for max probability of reporting --------------------------------
maxrep = c(0.9, 0.95)

# Splines parameters and preparation ---------------------------------------
# spl.chl.length <- length(which(!is.na(apply(Prev[,,1,1],1,sum))))*2
# spl.gon.length <- length(which(!is.na(apply(Prev[,,1,2],1,sum))))*2
# spl.chl = bs(1:spl.chl.length,  knots=c(9.5), Boundary.knots=c(1,18), deg=3, intercept=T)[,] # basis chlamydia  
# spl.gon = bs(1:spl.gon.length,  knots=c(9.5), Boundary.knots=c(1,10), deg=3, intercept=T)[,]  # basis gonorrhea  
# spl.chl.add = matrix( rep(spl.chl[spl.chl.length,], NRows-spl.chl.length), byrow=T, nrow=NRows-spl.chl.length)
# spl.gon.add = matrix( rep(spl.gon[spl.gon.length,], NRows-spl.gon.length), byrow=T, nrow=NRows-spl.gon.length)

spl.chl = bs(1:nrow(caserepF),  knots=c(9.5), deg=3, intercept=T)[,] # basis chlamydia  Boundary.knots=c(1,18),
spl.gon = bs(1:nrow(caserepF),  knots=c(9.5), deg=3, intercept=T)[,]  # basis gonorrhea  Boundary.knots=c(1,10),
spl.chl.add = matrix( rep(spl.chl[nrow(caserepF),], NRows-nrow(caserepF)), byrow=T, nrow=NRows-nrow(caserepF))
spl.gon.add = matrix( rep(spl.gon[nrow(caserepF),], NRows-nrow(caserepF)), byrow=T, nrow=NRows-nrow(caserepF))


# Complete basis ----------------------------------------------------------
spl.chl = rbind(spl.chl, spl.chl.add)
spl.gon = rbind(spl.gon, spl.gon.add)

# set up population dataset with projected.years, for CR prediction -------
pred.years = which(proj.pop3[,2,]%in%c(2019:2020))[1:2]
pp = proj.pop3[pred.years,3:5,]
pp = array(c(pp[,,2], pp[,,1], pp[,,2], pp[,,1]), dim=c(2,3,2,2))

DN1= array(c(rbind(DN[,,1,1],pp[,,1,1]), rbind(DN[,,2,1],pp[,,1,2]), 
             rbind(DN[,,1,2],pp[,,1,2]), rbind(DN[,,2,2],pp[,,2,2])), dim=c(NRows, 3, 2, 2)) 

# Data list for rjags model -----------------------------------------------
data= list( 
  'n.chains'		  = NC,
  'n.iterations'	= niter,	
  
  # data (prevalence, case reports, denominators, std.errors) ---------------
  'P.hat'			   = Prev,    
  'R.by.year'	   = round(CR/popul.factor, 0),   
  'N.by.year'		 = round(DN/popul.factor, 0),  # population is divided, in case needed, and rounded at the closest integer
  's.over.rootn' = SORn,
  
  # data features (dimensions, lengths) -------------------------------------
  'nyrs'			    = nrow(Prev), 
  'n.single.years'= nrow(CR),    
  'nages'			    = ncol(CR),
  'ngenders'		  = 2,
  'ndis'          = 2, 
  
  # parameters for beta distribution for screening --------------------------
  'pbetascr'	   = pbetascr, #(for generaating 1st point of the series for age group 2)
  
  # parameters for duration -------------------------------------------------
  ### 2 x 2 x 2 array. ON THE ROWS: (pdt,pdu); ON THE COLS:(comp1,comp2); [,,1]=Chlamydia, [,,2]=Gono
  'dur.parms'    = array(c(matrix(c(pdt.C,pdu.C), byrow=T, ncol=2, dimnames=list(c('pdu','pdt'),c('',''))), 
                   matrix(c(pdt.G,pdu.G), byrow=T, ncol=2, dimnames=list(c('pdu','pdt'),c('','')))), 
                   dim=c(2,2,2)),
  
  # parameters for probability being symptomatic ----------------------------
  ### 2 x 2 x 2 array. ON THE ROWS: (Females,Males); ON THE COLS:(comp1,comp2); [,,1]=Chlamydia, [,,2]=Gono
  'm.parms'   =array(c(matrix(c(1,1,1,1), byrow=T, ncol=2), matrix(c(1,1,1,1), byrow=T, ncol=2)), dim=c(2,2,2)),
  
  # parameters for probability treatment given symptomatic ------------------
  'pvs'			  = c(1,1),
  
  # parameters for rescaled beta on durations -------------------------------
  ### 2 x 2 x 2 array. ON THE ROWS: (du,dt); ON THE COLS: (int,slope); [,,1]=Chlamydia, [,,2]=Gono
  # Explanation, once and for all:
  # $Chlamydia [, , 1]
  #                                                 [,1]  [,2]
  # Females, untreated = Males, untreated du [1,] 1.110 1.620
  # Females, treated   = Males, treated   dt [2,] 0.079 0.151
  #
  # $Gonorrhea [, , 2], same as above
  #        [,1]  [,2]
  # [1,] 0.246 1.000
  # [2,] 0.079 0.151
  'duration.resc'= array(c(matrix(c(1.11,1.62,0.079,0.151), byrow=T, ncol=2), 
                   ## CT, untreated females = untreated males (row1), treated F = treated M (row2)
                   matrix(c(0.246,1, 0.079,0.151), byrow=T, ncol=2)), 
                   ## GC, untreated females = untreated males (row1), treated F = treated M (row2) (Ashleigh)
                   dim=c(2,2,2)),  
  
  ### (int,slope) for rescaling prob(sympt) for Chlamydia ONLY.
  # Explanation
  # , , 1 Chlamydia
  #        [,1]  [,2]  (hard bounds, min, max)
  #  [1,] 0.159 0.311  Females
  #  [2,] 0.159 0.311  Males
  #
  #, , 2 Gonhorrea
  #        [,1] [,2] (hard bounds, min, max)
  #  [1,] 0.22 0.61  Females
  #  [2,] 0.40 0.60  Males
  # 
  'm.resc'         = array(c(matrix(c(0.159,0.311,0.159,0.311), byrow=T, ncol=2), 
                     matrix(c(0.22,0.61,0.4,0.6), byrow=T, ncol=2)), dim=c(2,2,2)), ## overwritten Josh
  # multipliers -------------------------------------------------------------
  'dmultN.parms' =c(0,1),       
  'dmultB.parms' =c(0.1,0.8),

  # years predicted out -----------------------------------------------------
  'pred'			   = pred,
  
  # further prior coefficients ----------------------------------------------
  'factorscr'	   = c(1,2),        ##factor by which to increase screening rate
  'brkpoint.rep' = comp.rep.brk,  ## breakpoint for case reports random walk
  'brkpoint.scr' = scr.brk,       ## breakpoint for screening random walk
  'max.rep'      = maxrep,        ## max probability of reporting
  'wig.parms'    = 0.05,          ## wiggling allowance for random walks
  
  # population from which draw case reports prediction ----------------------
  'N.pred'       = DN1, #array(rep(100000, NRows*3*2*2), dim=c(NRows,3,2,2)), #, #
  
  # spline array for prevalence ---------------------------------------------
  'prev.spline.mat' = array(c(spl.chl, spl.gon), dim=c(NRows,5,2) )
)

# set names for data list -------------------------------------------------
namesdata=names(data)
# check print -------------------------------------------------------------
print(data)

s1=Sys.time()

# rjags model code --------------------------------------------------------
cat('model{
    
# Likelihoods ---------------------------------------------------------
    for(d in 1:ndis){for(g in 1:ngenders){for(k in 1:nages){ for(j in 1:nyrs){ 
    P.hat[j,k,g,d] ~ dnorm( pi.j[j,k,g,d], 1/pow(s.over.rootn[j,k,g,d],2) ) ## for prevalence
    pi.j[j,k,g,d] = (pi.w[(2*j-1),k,g,d]+pi.w[(2*j),k,g,d])/2  }            ## combined prevalence = midpoint of adjacent ones
    for(w in 1:n.single.years){ 
    R.by.year[w,k,g,d] ~ dbin( theta[w,k,g,d], N.by.year[w,k,g,d] )         ## for number of case reports by year
    } } } }
    
# multipliears to get males’screening, disease specific ---------------
    d.multB ~ dunif(dmultB.parms[1],dmultB.parms[2])

# Durations (untreated, treated symptomatic, treated asymptomatic -----
    for(d in 1:ndis){for(g in 1:ngenders){
    
    ## multipliers to get duration treatment, asymptomatic, gender-spec, disease-spec
    d.multN[g,d]~ dunif(dmultN.parms[1],dmultN.parms[2])
    
    ## gender-spec, disease-spec, base distribution
    d.unt[g,d] ~ dbeta(dur.parms[1,1,d],dur.parms[1,2,d]) ## original
    ## rescaled beta 
    ##du[g,d] <- duration.resc[1,1,d]+duration.resc[1,2,d]*d.unt[g,d] ## original
    du[g,d] <- duration.resc[1,1,d]+(duration.resc[1,2,d]-duration.resc[1,1,d])*d.unt[g,d]

    ## as above, for treated symptomatic
    d.ts[g,d] ~ dbeta(dur.parms[2,1,d],dur.parms[2,2,d]) ## original
    ##dt[g,d] <- duration.resc[2,1,d]+duration.resc[2,2,d]*d.ts[g,d] ## original
    dt[g,d] <- duration.resc[2,1,d]+(duration.resc[2,2,d]-duration.resc[2,1,d])*d.ts[g,d] 

    ### dur.treated asymptomatic
    dnew[g,d]<- dt[g,d]+(d.multN[g,d])*(du[g,d]-dt[g,d])
    } }
    
# SCREENING  ----------------------------------------------------------

# multipliers across age groups ---------------------------------------
      for(g in 1){
      fctr[2,g] ~ dunif(factorscr[1], factorscr[2])
      fctr[1,g] ~ dunif(1, fctr[2,g]) 
      fctr[3,g] ~ dunif(1,fctr[1,g]) } ## increase for females, age group 3 (as before)

      for(g in 2){ for(k in 1:3){ fctr[k,g] ~ dunif(1, 1.5) } }
# end multipliers -----------------------------------------------------

# starting points -----------------------------------------------------
    # starting points: CT ---------------------------------------------
    for(d in 1){ 
    for(g in 1){ # Females
      sscr[2,g,d] ~ dbeta(pbetascr[1], pbetascr[2]) 
      sscr[1,g,d] ~ dunif(0, sscr[2,g,d])
      sscr[3,g,d] ~ dunif(0, sscr[1,g,d]) }

    for(g in 2){ for(k in 1:nages){ ## males, any ages,
      sscr[k,g,d] ~ dunif(0.1, 0.3) }}
    } # done CT -------------------------------------------------------

    # starting points: GC ---------------------------------------------
    for(d in 2){ for(k in 1:nages){                                           
        sscr[k,1,d] <- sscr[k,1,1] # females gono get the same as females chlamydia
        sscr[k,2,d] ~ dunif(0.1, 0.3) # males gono get all from 10% to 30%  
    }} # done GC ------------------------------------------------------
# end starting points -------------------------------------------------

# maxima  -------------------------------------------------------------
    # take the mins and multiply by corresponding factors
    # CT --------------------------------------------------------------
    for(d in 1){ for(k in 1:nages){ for(g in 1:ngenders){ ## Females, Males -----------------------
      maxscr[k,g,d] <- sscr[k,g,d]*fctr[k,g]
      wig.scr[k,g,d] <- wig.parms }}} # done CT -----------------------
    # GC --------------------------------------------------------------
    for(d in 2){ for(k in 1:nages){                                             
      maxscr[k,1,d] <- maxscr[k,1,1]          ## females are shared with CT    
      maxscr[k,2,d] <- sscr[k,2,d]*fctr[k,2]  ## males gono all by the factor 1x1.5x multiplied
    for(g in 1:ngenders){ wig.scr[k,g,d] <- wig.parms }}} # done GC ---
# end maxima  ---------------------------------------------------------
    
# slopes --------------------------------------------------------------
    for(d in 1:ndis){ for(g in 1:ngenders){ for(k in 1:nages){
    slscr[k,g,d] <- (maxscr[k,g,d]-sscr[k,g,d])/(brkpoint.scr-1) ## (k*g=6 slopes)
    unif.scr[1,k,g,d] <- 0
    scr[1,k,g,d] <- sscr[k,g,d]
    
    for(w in 2:(n.single.years+pred)){
    unif.scr[w,k,g,d] ~ dunif(0,1)
    
# random walk ---------------------------------------------------------
    scr[w,k,g,d]  <- ifelse(w<= brkpoint.scr,
    max(0, min(maxscr[k,g,d], 2*wig.scr[k,g,d]*unif.scr[w,k,g,d]+(scr[w-1,k,g,d]+slscr[k,g,d]-wig.scr[k,g,d]))),
    max(0, min(maxscr[k,g,d], 2*wig.scr[k,g,d]*unif.scr[w,k,g,d]+(scr[w-1,k,g,d]-wig.scr[k,g,d]))) ) 
    }}}} 
# end -----------------------------------------------------------------
    
# further quantities --------------------------------------------------
    
  for(d in 1:ndis){for(g in 1:ngenders){for(k in 1:nages){ for(w in 1:(n.single.years+pred) ){
    
  # fraction treated (“gamma”) ----------------------------------------
    gam[w,k,g,d]<- m[d,g]*vs[d] + (1-m[d,g])*scr[w,k,g,d]
    
  # overall duration --------------------------------------------------
    dur[w,k,g,d]<- m[d,g]*vs[d]*dt[g,d] + m[d,g]*(1-vs[d])*du[g,d] + (1-m[d,g])*scr[w,k,g,d]*dnew[g,d] + 
                  (1-m[d,g])*(1-scr[w,k,g,d])*du[g,d]
    
  # prevalence — logit(prevalence)=spline -----------------------------
    logit(pi.w[w,k,g,d])<- a[1,k,g,d]*prev.spline.mat[w,1,d]+a[2,k,g,d]*prev.spline.mat[w,2,d]+a[3,k,g,d]*prev.spline.mat[w,3,d]+
    a[4,k,g,d]*prev.spline.mat[w,4,d]+a[5,k,g,d]*prev.spline.mat[w,5,d]
    
  # incidence=prevalence/duration -------------------------------------
    eta[w,k,g,d]<- pi.w[w,k,g,d]/dur[w,k,g,d]
    
  # case reports ------------------------------------------------------
    theta[w,k,g,d]<- eta[w,k,g,d]*rep[w,k,g,d]*gam[w,k,g,d]   ## rate
    R.pred[w,k,g,d] ~ dbin( theta[w,k,g,d], N.pred[w,k,g,d] ) ## predicted number over proj.popul
    }}}}
    
# other prior distributions -------------------------------------------
    
  # spline coefficients -----------------------------------------------
    for(d in 1:ndis){for(g in 1:ngenders){for(k in 1:nages){
    for(s in 1:5){ap[s,k,g,d] ~ dnorm(0, 1/10)}
    }}}
    
  # algebraic constraint on coefficients to get null first derivative -
    for(d in 1:ndis){for(s in 1:5){
    dif1[s,d]  <- prev.spline.mat[1,s,d]-prev.spline.mat[2,s,d]
    
    # dif.last[s,d] <- ifelse(d==1, 
    #   prev.spline.mat[17,s,d]-prev.spline.mat[18,s,d],
    #   prev.spline.mat[9,s,d]-prev.spline.mat[10,s,d] )

    dif18[s,d] <- prev.spline.mat[17,s,d]-prev.spline.mat[18,s,d]
    }}
    
    for(d in 1:ndis){ for(g in 1:ngenders){ for(k in 1:nages){ 
    sum1[1,k,g,d] <- 0; sum5[1,k,g,d] <- 0
    for(s in 2:4){
    sum1[s,k,g,d] <- ap[s,k,g,d]*dif1[s,d]
    sum5[s,k,g,d] <- ap[s,k,g,d]*dif18[s,d]
    # sum5[s,k,g,d] <- ap[s,k,g,d]*dif.last[s,d] 
    }
    
    sum11[k,g,d] <- -sum(sum1[,k,g,d]) 
    sum55[k,g,d] <- -sum(sum5[,k,g,d]) 
    
    a[1,k,g,d]<- sum11[k,g,d]/dif1[1,d]
    # a[5,k,g,d]<- sum55[k,g,d]/dif.last[5,d]
    a[5,k,g,d]<- sum55[k,g,d]/dif18[5,d]
    for(s in 2:4){a[s,k,g,d]<-ap[s,k,g,d]} 
    }}}
    
  # completeness of reporting system ----------------------------------
  for(g in 1:ngenders){
    rep0[1,g,1]            ~ dunif(0.6, 0.8)             # start CT (only 1), M & F
    rep0[1,g,2]            ~ dunif(0.7, 0.95)            # start GC M&F
  for(d in 1:ndis){                                      # different slopes by disease
    rep0[brkpoint.rep,g,d] ~ dunif(max.rep[1],max.rep[2])  # max (only 1)
    slrep[g,d] <- (rep0[brkpoint.rep,g,d]-rep0[1,g,d])/(brkpoint.rep-1) #slope
    }}

for(d in 1:ndis){for(g in 1:ngenders){for(k in 1:nages){
    wig.rep[k,g,d] <- wig.parms         ## yearly perturb (still different, so 12 different r.walks)
    rep[1,k,g,d] <-  rep0[1,g,d]        ## all the same starting point
    for(w in 2:(n.single.years+pred)){
    rep.u[w,k,g,d] ~ dunif(0,1)
    rep[w,k,g,d] <- ifelse(w <= brkpoint.rep,
    max(rep[1,k,g,d] , min(0.95, 2*wig.rep[k,g,d]*rep.u[w,k,g,d]+(rep[w-1,k,g,d]+slrep[g,d]-wig.rep[k,g,d]))),
    max(rep[1,k,g,d] , min(0.95, 2*wig.rep[k,g,d]*rep.u[w,k,g,d]+(rep[w-1,k,g,d]-wig.rep[k,g,d]))))  }
}}}

# probability being symptomatic ---------------------------------------
    m.base.chlam ~ dbeta(m.parms[1,1,1], m.parms[1,2,1]) ## base distrib for chlamydia
    ## always [g,d]
    m[1,1] <- m.resc[1,1,1]+(m.resc[1,2,1]-m.resc[1,1,1])*m.base.chlam  ## rescaled on hard bounds
    m[2,1] <- m[1,1]                                                    ## as original (males=females)
    
    m.base.gono ~ dbeta(m.parms[1,1,2],m.parms[1,2,2])
    m[1,2] <- m.resc[1,1,2]+(m.resc[1,2,2]-m.resc[1,1,2])*m.base.gono  ## gono, different: females from beta directly
    m[2,2] <- m.resc[2,1,2]+(m.resc[2,2,2]-m.resc[2,1,2])*m.base.gono  ## males, rescaled hard bound betw.Ashleighs parameters

# probability treatment given symptomatic -----------------------------
    for(d in 1:ndis){ 
    vs.beta[d] ~ dbeta(pvs[1], pvs[2])	
    vs[d] <- 0.8+(0.9-0.8)*vs.beta[d]  }
    
}', file='model1.bug')
# end rjags model code ----------------------------------------------------

# data & initial values for chain -----------------------------------------
inits1 <- list(
  'd.multB'=0.45,
  'd.multN'=matrix(rep(0.5,4), byrow=T, ncol=2),
  'd.unt'=matrix(rep(0.5,4), byrow=T, ncol=2),
  'd.ts'=matrix(rep(0.5,4), byrow=T, ncol=2),
  'm[2,]'=c(0.5, 0.8),
  ##'vs'=c(0.5,0.5),
  'ap' = array( c(
    matrix(c(rep(log(0.05),3), rep(NA,12)), byrow=T, ncol=3), 
    matrix(c(rep(log(0.05),3), rep(NA,12)), byrow=T, ncol=3),
    matrix(c(rep(log(0.05),3), rep(NA,12)), byrow=T, ncol=3), 
    matrix(c(rep(log(0.05),3), rep(NA,12)), byrow=T, ncol=3)), dim=c(5,3,2,2))
)


# posterior evaluation ----------------------------------------------------
jags_mod <- jags.model('model1.bug', data=data, inits=inits1, n.chains=NC, n.adapt=5000)
  ## n.chains = # parallel chains for the model
  ## n.adapt  = # iterations for adaptation (an initial sampling phase during which the samplers 
  ##            adapt their behavior to maximize their efficiency. The sequence of samples generated during 
  ##            this adaptive phase is not a Markov chani, and not used for posterior inference)
update(jags_mod, n.iter=10000) ##20000
  ## this constitutes a burn-in period (added to the n.adapt adaptive period) ##'beta',
posts=coda.samples(model=jags_mod,variable.names=c('pi.w','pi.j','eta', 'a','dur','gam', 'theta', 'R.pred', 'm','vs',
                                                   'scr','rep','dt','du','dnew','slscr','slrep'), n.iter=niter, thin=1) 

# execution time check ----------------------------------------------------
s2=Sys.time()
print(s2-s1)
s3=Sys.time()

# save results ------------------------------------------------------------
posts.stat = summary(posts)$statistics  
posts.quantiles = summary(posts)$quantiles

## posterior for 2-year and 1-year prevalence 
posts.median.prev.j 	= posts.quantiles[grep('\\bpi.j\\b',rownames(posts.quantiles)),3]
posts.mean.prev.j 		= posts.stat[grep('\\bpi.j\\b',rownames(posts.stat)),]
posts.ci.prev.j 		  = posts.quantiles[grep('\\bpi.j\\b',rownames(posts.quantiles)),]
posts.median.prev.w 	= posts.quantiles[grep('\\bpi.w\\b',rownames(posts.quantiles)),3]
posts.mean.prev.w 		= posts.stat[grep('\\bpi.w\\b',rownames(posts.stat)),]
posts.ci.prev.w 	    = posts.quantiles[grep('\\bpi.w\\b',rownames(posts.quantiles)),]
posts.slscr           = posts.stat[grep("slscr", rownames(posts.stat)),]
posts.slrep           = posts.stat[grep("slrep", rownames(posts.stat)),]

## posterior for spline coefficients
a.chain.prev      = posts[[1]][,grep('\\ba\\b',colnames(posts[[1]]))]

## posterior for incidence, rate and number of case reports
posts.mean.incid  = posts.stat[grep('\\beta\\b',rownames(posts.stat)),]
posts.ci.incid    = posts.quantiles[grep('\\beta\\b',rownames(posts.quantiles)),]
posts.mean.theta  = posts.stat[grep('\\btheta\\b',rownames(posts.stat)),]
posts.ci.theta    = posts.quantiles[grep('\\btheta\\b',rownames(posts.quantiles)),]
posts.mean.R      = posts.stat[grep('\\bR.pred\\b',rownames(posts.stat)),]
posts.ci.R        = posts.quantiles[grep('\\bR.pred\\b',rownames(posts.quantiles)),]

rep.post          = posts[[1]][,grep('\\brep\\b', colnames(posts[[1]]))]

## posterior for P(symptomatic) & P(treated symptomatic)
m.post = posts[[1]][,grep('\\bm\\b', colnames(posts[[1]]))]
vs.post = posts[[1]][,grep('\\bvs\\b', colnames(posts[[1]]))]
  
## duration posteriors
dt.post = posts[[1]][,grep('\\bdt\\b', colnames(posts[[1]]))]
du.post = posts[[1]][,grep('\\bdu\\b', colnames(posts[[1]]))]
dnew.post = posts[[1]][,grep('\\bdnew\\b', colnames(posts[[1]]))]
dur.post = posts[[1]][,grep('\\bdur\\b', colnames(posts[[1]]))]

## assemble everything into a list
list.posts = list(
  'posts.median.prev.j'=posts.median.prev.j,
  'posts.mean.prev.j'=posts.mean.prev.j,
  'posts.ci.prev.j'=posts.ci.prev.j,
  'posts.median.prev.w'=posts.median.prev.w,
  'posts.mean.prev.w'=posts.mean.prev.w, 
  'posts.ci.prev.w'=posts.ci.prev.w,
  'a.chain.prev'=a.chain.prev,
  'posts.mean.incid'=posts.mean.incid,
  'posts.ci.incid'=posts.ci.incid,
  'posts.mean.theta'=posts.mean.theta,
  'posts.ci.theta'=posts.ci.theta,
  'posts.mean.R'=posts.mean.R,
  'posts.ci.R'=posts.ci.R,
  'rep.post'=rep.post,
  'm.post'=m.post,
  'vs.post'=vs.post,
  'dt.post'=dt.post,
  'du.post'=du.post,
  'dnew.post'=dnew.post,
  'dur.post'=dur.post,
  'slscr.post'=posts.slscr,
  'slrep.post'=posts.slrep)

# current results directory -----------------------------------------------
save.dir = paste0(bcb,'/gplab/mazzola/msm/BayesianModeling/Results/',Sys.Date(),'/')
dir.create(file.path(save.dir))
setwd(save.dir)

#results.date.dir = paste(overall.dir,'/Results/',Sys.Date(), '/',sep='')
#dir.create(file.path(results.date.dir))
#setwd(results.date.dir)

# addition to file name ---------------------------------------------------
casereports = paste('full_cases_over_', popul.factor, sep='')

# save function -----------------------------------------------------------
savefile = function(fl, name){ save(fl, file=paste(bcb, save.dir, name,'_',screening.reporting,'_',casereports,'_',niterK,'.Rda', sep='')) }

# save results files ------------------------------------------------------
if(NC>1){ savefile(DIC, 'DIC') }
savefile(list.posts, 'list.posts')
savefile(data, 'data_allchain')
savefile(posts, 'posts_complete')

s4=Sys.time()
print(s4-s3)

# end code ----------------------------------------------------------------


