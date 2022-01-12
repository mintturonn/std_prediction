# run it
# setwd('/homes/mazzola/RCode/CDC_code_May20/Code_for_plots/')
# source('newplots_Nov19.R')

rm(list=ls())
lapply(c('psych','gplots','coda','foreign','splines'), require, character.only = TRUE)

bcb=''
# Directories -------------------------------------------------------------
overall.dir       = paste0(bcb,"/homes/mazzola/")
code.dir          = paste0(bcb,"/homes/mazzola/RCode/CDC_code_May20")
data.dir          = paste0(bcb,"/homes/mazzola/Datafiles/2020-05-11/")

load(paste0(data.dir, '/project.popul.array.rda'))

source(paste0(code.dir,'/Code_for_data_prep/data_prep_for_mcmc.R') )
SDt = Sys.Date()

results.date.dir  = paste0(bcb,"/gplab/mazzola/msm/BayesianModeling/Results/",SDt,'/')
save.dir =paste0(bcb,'/gplab/mazzola/msm/BayesianModeling/Results/',SDt,'/')

niter = 200000
popul.factor = 10
niterK = paste0(niter/1000,'K')
screening.reporting = '6_12_5pct_all'
RWs='6' 
RWs_CR ='12' 
casereports = paste('full_cases_over_', popul.factor, sep='')
#case = 'S2R3'

setwd(save.dir)

load(file = paste0('list.posts','_',screening.reporting,'_',casereports,'_',niterK,'.Rda')); list.posts=fl; ##list.posts. less.beta,'_',niter
load(file = paste0('data_allchain','_',screening.reporting,'_',casereports,'_',niterK,'.Rda')); data=fl;  ## data
load(file = paste0('posts_complete','_',screening.reporting,'_',casereports,'_',niterK,'.Rda')); posts=fl; ## posts

# input parameters --------------------------------------------------------
colnames(DN)  = rep(c('15-19 years','20-24 years','25-39 years'),1)
pred          = 2 
comp.rep.brk  = which(c(1999:2019)==2011)
ag = age.grp  = c('15-19','20-24','25-39')
howmany       = 25
index.chose   = sample(1:niter, howmany, replace=FALSE)

# input empty arrays ------------------------------------------------------
prevalent.cases = incident.cases = mean.pct.prev = mean.pct.incid = case.reports=array(c(
  matrix(rep(NA,(data$n.single.years+data$pred)*3), byrow=T, ncol=3), 
  matrix(rep(NA,(data$n.single.years+data$pred)*3), byrow=T, ncol=3),
  matrix(rep(NA,(data$n.single.years+data$pred)*3), byrow=T, ncol=3), 
  matrix(rep(NA,(data$n.single.years+data$pred)*3), byrow=T, ncol=3)), 
  dim=c((data$n.single.years+data$pred),3,2,2))

# function for transparent colors -----------------------------------------
t_col <- function(color, percent = 50, name = NULL) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
           max = 255,
           alpha = (100-percent)*255/100,
           names = name)
  invisible(t.col) }

# loop for infections & genders -------------------------------------------
sapply(1:2, function(d){ 
  
  sapply(1:2, function(g){

# overall parameters ------------------------------------------------------
    d<<-d; g<<-g
    seq.age           = 1:3
    pred.single.years = data$pred
    cut               = 0
    nyears.p 		      = seq(from=1, to=(data$n.single.years+pred.single.years-cut), by=1)
    nyears   		      = seq(from=1, to=data$n.single.years, by=1)
    nyears.fillin     = seq(from=0.75, to=(data$n.single.years+pred.single.years+0.25), by=0.5)
    ag = age.grp      = c('15-19','20-24','25-39')

# parameters for Chlamydia ------------------------------------------------
    if(d==1){
    source(paste0(code.dir,'comprep_priors_Apr19.R'))
    # print( rep0[,1,,d] )
    #rep0.init = rep0[,1,,d]
# figure names ------------------------------------------------------------
    if(g==1){
    sex='F'; filename=paste0('/Females_C_',casereports,'_',screening.reporting,'.pdf'); dir.create(file.path(save.dir))
    } else if(g==2){
    sex='M'; filename=paste0('/Males_C_',casereports,'_',screening.reporting,'.pdf'); dir.create(file.path(save.dir)) }
    setwd(file.path(save.dir)) 
    
# pdf canvas --------------------------------------------------------------
    pdf(file=paste0(save.dir,filename), width=18, height=35)
    MM = matrix(c(1:3,18:20,24:26,21:23,4,6,8,5,7,9,10:17,27), byrow=T, ncol=3)
    layout.show(layout(MM))
    par(mar=c(2,4.5,4,0.5)+0.1)

  } else if(d==2) {

# parameters for Gonorrhea ------------------------------------------------
    source(paste0(code.dir,'/comprep_priors_Apr19.R'))
    source(paste0(code.dir,'/datasource_Gono.R') )

    # these need to stay here; datasource_Gono.R erases one class -------------
    age.grp = c('15-19','20-24','25-39')
    ag 	<<- age.grp
    
    # figure names ------------------------------------------------------------
    if(g==1){
    sex='F'; filename=paste0('/Females_G_',casereports,'_',screening.reporting,'.pdf'); dir.create(file.path(save.dir))
    } else if(g==2){
    sex='M'; filename=paste0('/Males_G_',casereports,'_',screening.reporting,'.pdf'); dir.create(file.path(save.dir)) }
    setwd(file.path(save.dir))   
  
    # pdf canvas --------------------------------------------------------------
    pdf(file=paste0(save.dir,filename), width=18, height=35)
    MM = matrix(c(1:3,18:20,24:26,21:23,4,6,8,5,7,9,10:17,27), byrow=T, ncol=3)
    layout.show(layout(MM))
    par(mar=c(2,4.5,4,0.5)+0.1)
  }

# common code -------------------------------------------------------------

# prevalence plot ---------------------------------------------------------
  source(paste0(code.dir,'/plot_prevalence.R'));
  plot.prevalence(g,d,seq.age, pred.single.years, cut, nyears.p, nyears, nyears.fillin, save.dir)
# end ---------------------------------------------------------------------

# screening plots ---------------------------------------------------------
  ## time variables ----
  dsy = data$n.single.years+data$pred  ## this is how it's modeled in Rjags
  dsy1 = dsy-1

  ## Parameters for posterior choice ----
  ## "howmany" is how many draws I ultimately want from the distributions (either prior or posteriors)
  dim.post = nrow(posts[[1]])*data$n.chains 

  ### empty prior matrix for aux quantities (m, vs, dt, du, dnew)
  prior.matrix = matrix(rep(NA, dim.post*5), byrow=T, ncol=5)
  colnames(prior.matrix)=c('m','vs','dt','du','dnew')

  ## notice that I save different "index.chose" for males and females, so ideally I might have two different samples for the genders
  ## index.chose = if(g==1){sample(1:dim.post, howmany, replace=FALSE)}else if(g==2){sample(1:dim.post, howmany, replace=FALSE)}  ## which row of the posterior I'm sampling
  if(d==1){
  #prior.matrix[,'m'] = m = data$m.resc[1]+data$m.resc[2]*rbeta(dim.post, data$m.parms[g,1,d], data$m.parms[g,2,d])
  prior.matrix[,'m'] = m = data$m.resc[1,1,d]+(data$m.resc[1,2,d]-data$m.resc[1,1,d])*rbeta(dim.post, data$m.parms[g,1,d], data$m.parms[g,2,d])
  }else if (d==2){
  #if(g==1){                                         
  # prior.matrix[,'m'] = m = rbeta(dim.post, data$m.parms[g,1,d], data$m.parms[g,2,d])
  #} else if(g==2){                                  
  prior.matrix[,'m'] = m = data$m.resc[g,1,d]+(data$m.resc[g,2,d]-data$m.resc[g,1,d])*rbeta(dim.post, data$m.parms[g,1,d], data$m.parms[g,2,d])
  #} 
  }

  prior.matrix[,'vs']= vs =  0.8 + (0.9-0.8)*rbeta(dim.post,data$pvs[1],data$pvs[2]) #rbeta(dim.post, data$pvs[1],data$pvs[2]) 
  save(prior.matrix, file=paste(save.dir,screening.reporting,'prior.matrix.rda', sep=''))

  g<<-g; dim.post <<- dim.post
# initialize screening prior ----------------------------------------------
if(d==1){   
  scr.prior = sscr.prior= NULL
  dmultB = runif(1,data$dmultB.parms[1],data$dmultB.parms[2])
  scr.prior=sscr.prior= wig.scr =   array(
  c(matrix(rep(NA, dim.post*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
  matrix(rep(NA, dim.post*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred)),
  matrix(rep(NA, dim.post*(data$n.single.years+data$pred)), byrow=T, ncol=(data$n.single.years+data$pred))),
  dim=c(dim.post, (data$n.single.years+data$pred), data$nages, data$ngenders, data$ndis))
  
  scr.prior<<- scr.prior; sscr.prior<<-sscr.prior; wig.scr<<-wig.scr; dmultB<<-dmultB;
  source(file=paste0(code.dir,'/screening_priors_Apr19.R'))
  save(scr.prior, file=paste(save.dir,screening.reporting,'scr_prior_d1.rda', sep='')) } else if (d==2){
  load(file=paste0(save.dir,screening.reporting,'scr_prior_d1.rda')) }

  # plotting functions ------------------------------------------------------
  source(file=paste0(code.dir,'/plot_screening.R')); 
  plot.screening.fractiontrt(g,d,dsy, dsy1, dim.post, prior.matrix, index.chose, seq.age, nyears.p, nyears)
# end ---------------------------------------------------------------------

# overall & individual durations ------------------------------------------
  ## Empty dur.treated and untreated ----
  dt=du=dnew=matrix(rep(NA,2* dim.post), ncol=2)

  ## Multiplier for treated symptomatic ----
  d.multN = matrix( cbind(rep( runif(dim.post, data$dmultN.parms[1], data$dmultN.parms[2]),2)), byrow=F, ncol=2)

  ## Priors directly from parameters in data ----
  ##dt[,g]=data$duration.resc[2,1,d]+data$duration.resc[2,2,d]*rbeta(dim.post, data$dur.parms[1,1,d],data$dur.parms[1,2,d])
  ##du[,g]=data$duration.resc[1,1,d]+data$duration.resc[1,2,d]*rbeta(dim.post, data$dur.parms[2,1,d],data$dur.parms[2,2,d])
  
  dt[,g]=data$duration.resc[2,1,d]+(data$duration.resc[2,2,d]-data$duration.resc[2,1,d])*rbeta(dim.post, data$dur.parms[1,1,d],data$dur.parms[1,2,d])
  du[,g]=data$duration.resc[1,1,d]+(data$duration.resc[1,2,d]-data$duration.resc[1,1,d])*rbeta(dim.post, data$dur.parms[2,1,d],data$dur.parms[2,2,d])
  
  dnew[,g]= dt[,g]+(d.multN[,g])*(du[,g]-dt[,g])

  prior.matrix[,'dt']=dt[,g]
  prior.matrix[,'du']=du[,g]
  prior.matrix[,'dnew']=dnew[,g]

  save(prior.matrix, file=paste(save.dir,screening.reporting,'prior.matrix.rda', sep=''))
  gd.index = grep(paste(g,',',d,']', sep=''), colnames(list.posts$dt.post))

  ## Posteriors from saved data ----
  dt.post     = as.matrix(list.posts$dt.post[,gd.index])
  du.post     = as.matrix(list.posts$du.post[,gd.index])
  dnew.post   = as.matrix(list.posts$dnew.post[,gd.index])
  if(data$n.chains>1){
  m.post=as.vector(list.posts$m.post)
  vs.post=as.vector(list.posts$vs.post) } else {
  m.post = as.vector(list.posts$m.post[,gd.index])
  vs.post = as.vector(list.posts$vs.post[,d]) }

  gd.index = grep(paste(',',g,',',d,']', sep=''), colnames(list.posts$dur.post))
  dur.post    = as.matrix(list.posts$dur.post[,gd.index])

  source(file=paste(code.dir,'/plot_durations.R', sep='')); 
  plot.durations(g,d, dt, du, dt.post, du.post,  m, m.post, dnew, dnew.post, vs, vs.post, dur.post, 
               prior.matrix, gd.index, seq.age, nyears.p, nyears, index.chose, save.dir)
# end ---------------------------------------------------------------------

# Incidence ---------------------------------------------------------------
  source(file=paste(code.dir,'/plot_incidence.R', sep='')); 
  plot.incidence(g,d, seq.age, nyears.p, nyears, dur.post,pred.single.years, save.dir)
# end ---------------------------------------------------------------------

# completeness of reporting system ----------------------------------------
  ## Retrieve posteriors ----
  posts.rep = list.posts$rep.post
  
  rep0 = comprep.priors(g,d)
  print(rep0[,1,,d])
  #print(rep0.init)
  
  # pdf(file=paste0(save.dir,filename), width=18, height=6)
  # MM = matrix(c(1:3), byrow=T, ncol=3)
  # layout.show(layout(MM))
  # par(mar=c(2,4.5,4,0.5)+0.1)
  
  ## Reporting loop ----
  lapply(seq.age, function(ia){
    index = grep(paste(',',ia,',',g,',',d,']', sep=''), colnames(posts.rep))  
    posts.rep.ia = posts.rep[index.chose,index]  
    
    ## empty plot
    plot(nyears.p, nyears.p, type='n', xaxt='n',  ylab='P(treated case is reported in reporting system)', xlab='', ylim=c(0,1),  cex.lab=1.25, cex.axis=1.25);
    axis(side=1, at=c(nyears.p), seq(from=1999, length.out=length(nyears.p)), cex.axis=1.25)  
    
    ## posterior polot
    sapply(1:length(index.chose), function(i) lines(nyears, posts.rep.ia[i,nyears], col='red'))  
    sapply(1:length(index.chose), function(i) lines(length(nyears):length(nyears.p), posts.rep.ia[i,length(nyears):length(nyears.p)], col=t_col('red',65,'transred')))  
    
    ## prior plot
    sapply(1:length(index.chose), function(i) lines(nyears, rep0[i,nyears,ia,g], lwd=0.25, col='grey65'))  
    sapply(1:length(index.chose), function(i) lines(length(nyears):length(nyears.p), rep0[i,length(nyears):length(nyears.p),ia,g], lwd=0.25, col='grey80'))  
}) 
# end ---------------------------------------------------------------------

# case reports ------------------------------------------------------------
  ## Retrieve posteriors ----
  posts.mean.cr = list.posts$posts.mean.theta  ## mean
  posts.ci.cr	  = list.posts$posts.ci.theta    ## confidence intervals
  posts.mean.R  =  list.posts$posts.mean.R      ## mean NUMBER
  posts.ci.R	  = list.posts$posts.ci.R        ## confidence intervals NUMBER

## CR loop ----
lapply(seq.age, function(ia){
  
  ## choose posterior columns accordingly	
  index = grep(paste(',',ia,',',g,',',d,']', sep=''), rownames(posts.mean.cr))  
  indexR = grep(paste(',',ia,',',g,',',d,']', sep=''), rownames(posts.mean.R))  
  index.gender = grep(paste(',',g,',',d,']', sep=''), rownames(posts.mean.R)) 
  
  ## scale per 100k individuals
  R.per.100k = posts.mean.R[indexR,1]*100000/(data$N.pred[1:nrow(data$N.pred),ia,g,d])
  ci.R.per.100k = posts.ci.R[indexR,c(1,5)]*100000/(data$N.pred[1:nrow(data$N.pred),ia,g,d])
  
  rangeseq=seq(from=0, to=pretty(max(ci.R.per.100k))[2], length=5)
  ##pretty(min(ci.R.per.100k))[1]
  ## Plots ----
  plot(nyears.p, seq(from=min(rangeseq),to=max(rangeseq), length=length(nyears.p)), type='n', xaxt='n', yaxt='n', 
       ylab='Number of reported cases per 100k individuals', xlab='', cex.lab=1.25, cex.axis=1.25); 
  axis(side=1, at=c(nyears.p), seq(from=1999, length.out=length(nyears.p)), cex.axis=1.25) 
  axis(side=2, at=pretty(rangeseq), pretty(rangeseq), cex.axis=1.25) 
  
  ## Plot numbers ----	
  points(nyears, R.per.100k[nyears], col='purple', pch=1)
  points(length(nyears):length(nyears.p), R.per.100k[length(nyears):length(nyears.p)], col='purple', pch=1)
  
  ## Plot confidence intervals ----
  sapply(nyears, function(i) segments(i, ci.R.per.100k[i,1], i, ci.R.per.100k[i,2], col='purple'))
  sapply(length(nyears):length(nyears.p), function(i) segments(i, ci.R.per.100k[i,1], i, ci.R.per.100k[i,2], col='purple'))
  
  ## Plot shaded area ----
  polygon(c(nyears.p,rev(nyears.p)), c(ci.R.per.100k[nyears.p,1],rev(ci.R.per.100k[nyears.p,2])), 
          col=t_col('purple',65,'transpurple'), border=NA )
  points(nyears, (data$R.by.year[,ia,g,d]*popul.factor)*100000/(data$N.pred[1:20,ia,g,d]), pch=19, col='purple')
})
# end ---------------------------------------------------------------------

## Write details of specific run on last panel ----
plot(nyears.p, nyears.p, type='n', xaxt='n',  yaxt='n', ylab='', xlab='');
text(10,5, paste0('# chains: ', data$n.chains, '; # iterations per chain: ', data$n.iterations))
#text(10,4, paste0('allowance around screening rw: ', data$wig.scr))
text(10,3, paste0('screening-reporting scenario: ', screening.reporting))


# close pdf ---------------------------------------------------------------
dev.off()

setwd(overall.dir)


# closes sapply g ---------------------------------------------------------
})

# closes sapply d ---------------------------------------------------------
})



# Create combined excel file ----------------------------------------------
setwd(save.dir)

lapply(c('openxlsx'), require, character.only=TRUE)
csv.files = list.files()[ grep(paste0(screening.reporting,'.csv'), list.files()) ]

wb <- createWorkbook()
 
sapply(1:2, function(d){
  
  index.disease = paste0('d_',d)
  files = csv.files[ grep(index.disease, csv.files) ]
  
  files.incid = files[ grep('incid', files) ];
  if( sum( as.numeric(grep('mean_pct', files.incid)%in%c(3,4)))==2) {files.incid=files.incid} else
    if(sum( as.numeric(grep('mean_pct', files.incid)%in%c(3,4)))<2) {files.incid=files.incid[c(3,4,1,2)]}
  files.prev = files[ grep('prev', files) ]
  if( sum( as.numeric(grep('mean_pct', files.prev)%in%c(3,4)))==2) {files.prev=files.prev} else
    if(sum( as.numeric(grep('mean_pct', files.prev)%in%c(3,4)))<2) {files.prev=files.prev[c(3,4,1,2)]}

  readin.incid = lapply(files.incid, read.csv)
  readin.prev = lapply(files.prev, read.csv)

  totals.incid <- lapply(readin.incid, function(x) apply(x, 1,sum))
  totals.prev <- lapply(readin.prev, function(x) apply(x, 1,sum))

  readin.incid <- lapply(1:4, function(x) cbind(readin.incid[[x]], totals.incid[[x]]))
  readin.prev <- lapply(1:4, function(x) cbind(readin.prev[[x]], totals.prev[[x]]))

  colnms = c('Year', 'Ages 15-19','Ages 20-24','Ages 25-39', 'Total estimated by year')

  readin.incid <- lapply(readin.incid, setNames, colnms)
  readin.prev <- lapply(readin.prev, setNames, colnms)

  namedisease <- ifelse(d==1, 'Chlamydia', 'Gonorrhea')

  sapply( c("Incid", "Preval"), function(q){
    if(q=="Incid"){
      namequantity <<- 'Incidence'
      print(c(namedisease,namequantity))
      addWorksheet(wb, paste(namedisease, namequantity, sep=''))
      readin <<- readin.incid
    }
    else if(q=="Preval"){
      namequantity <<- 'Prevalence'
      print(c(namedisease,namequantity))

      addWorksheet(wb, paste(namedisease, namequantity, sep=''))
      readin <<- readin.prev
    }

    curr_row <- 4
    for(i in seq_along(readin.incid)) {
      writeData(wb, paste(namedisease, namequantity, sep=''), names(readin)[i], startCol = 2, startRow = curr_row)
      writeData(wb, paste(namedisease, namequantity, sep=''), readin[[i]], startCol = 1, startRow = curr_row+1)
      curr_row <- curr_row + nrow(readin[[i]]) + 2 }
    wb <<- wb
  })

})

saveWorkbook(wb, paste0("Summary_CG_",Sys.Date(),"_",screening.reporting,".xlsx"))


#DO ONLY BY HAND. REMOVES FILES. CHECK BEFORE RUNNING.
#bcb=''
#save.dir =paste0(bcb,'/gplab/mazzola/msm/BayesianModeling/Results/',SDt,'/')
#setwd(save.dir)
#file.remove( grep(list.files(), pattern=paste0(c('posts_complete','list.posts','data_allchain','Summary_CG','Females','Males'), collapse='|'), inv=T, value=T) )








