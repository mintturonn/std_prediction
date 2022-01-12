# run it
# setwd('/homes/mazzola/RCode/CDC_code_May20/Code_for_plots/')
# source('plotsJosh_Nov19_comment.R')


rm(list=ls())

setwd('/gplab/mazzola/msm/BayesianModeling')
overall.dir = getwd()
diff.days=0

Sys.Date=Sys.Date()-diff.days
#results.dir = paste(overall.dir, '/Results/',Sys.Date,sep='')
results.dir  = paste0(overall.dir, '/Results/',Sys.Date,'/')


setwd(file.path(results.dir)) 

library("tidyverse")


# code for creating Table 2 in the paper, and Figure 6 ----------------------------------------
sapply(1:2, function(d){

  # load population data ------------------------------------------------------------------------
  ls.pop =lapply(c(1,2), function(g){ lapply(1:3, function(k){
  string = paste0('*prev_pop_k_',k,'_g_',g,'_d_1*5pct*')
  load(file=list.files(pattern=glob2rx(string))[1])
  complete.pop }) })
  namestring = as.vector( sapply(c(1,2), function(g) {sapply(c(1:3), function(k){paste0('pop',k,g)})}))
  ls.pop.u = unlist(ls.pop, recursive=F)
  names(ls.pop.u)=namestring

  # load prevalence posterior chains ------------------------------------------------------------
  ls.prev =lapply(c(1,2), function(g){ lapply(1:3, function(k){
  string = paste0('*prev_post_chn_k_',k,'_g_',g,'_d_',d,'*5pct*')
  load(file=list.files(pattern=glob2rx(string))[1])
  posterior.chain }) })
  namestring = as.vector( sapply(c(1,2), function(g) {sapply(c(1:3), function(k){paste0('prev',k,g)})}))
  ls.prev.u = unlist(ls.prev, recursive=F)
  names(ls.prev.u)=namestring

  # incidence populations -----------------------------------------------------------------------
  ls.ipop   = ls.pop
  ls.ipop.u = ls.pop.u
  namestring = as.vector( sapply(c(1,2), function(g) {sapply(c(1:3), function(k){paste0('ipop',k,g)})}))
  ls.ipop.u = unlist(ls.pop, recursive=F)
  names(ls.ipop.u)=namestring

  # load incidence posterior chains ------------------------------------------------------------
  ls.incid =lapply(c(1,2), function(g){ lapply(1:3, function(k){
  string = paste0('*incid_post_chn_k_',k,'_g_',g,'_d_',d,'*5pct*')
  load(file=list.files(pattern=glob2rx(string))[1])
  posterior.chain }) })
  namestring = as.vector( sapply(c(1,2), function(g) {sapply(c(1:3), function(k){paste0('incid',k,g)})}))
  ls.incid.u = unlist(ls.incid, recursive=F)
  names(ls.incid.u)=namestring

# Prevalence ----------------------------------------------------------------------------------
  cpc = lapply(1:length(ls.pop.u), function(ll){ sapply(1:ncol(ls.prev.u[[ll]]), 
      function(col){ ls.pop.u[[ll]]*ls.prev.u[[ll]][,col]}) })
  namestring = as.vector( sapply(c(1,2), function(g) {sapply(c(1:3), function(k){paste0('chain.prev.cases',k,g)})}))
  names(cpc)=namestring

# Incidence -----------------------------------------------------------------------------------
  cic = lapply(1:length(ls.pop.u), function(ll){ sapply(1:ncol(ls.incid.u[[ll]]), 
  function(col){ ls.pop.u[[ll]]*ls.incid.u[[ll]][,col]}) })
  namestring = as.vector( sapply(c(1,2), function(g) {sapply(c(1:3), function(k){paste0('chain.incid.cases',k,g)})}))
  names(cic)=namestring
  
# Ages 15-19 and 20-24 combined ---------------------------------------------------------------
# 1. Prevalence -------------------------------------------------------------------------------
prev.overall_12_1 =  cpc[[1]]+cpc[[2]] 
prev.overall_12_2 =  cpc[[4]]+cpc[[5]] 

avg.prev_12_1 = apply(prev.overall_12_1, 1, mean)
cis_12_1      = apply(prev.overall_12_1, 1, function(x) quantile(x, c(0.025, 0.975)))
avg.prev_12_2 = apply(prev.overall_12_2, 1, mean)
cis_12_2      = apply(prev.overall_12_2, 1, function(x) quantile(x, c(0.025, 0.975)))

avg.prev_3_1 = apply(cpc[[3]], 1, mean)
cis_3_1      = apply(cpc[[3]], 1, function(x) quantile(x, c(0.025, 0.975)))
avg.prev_3_2 = apply(cpc[[6]], 1, mean)
cis_3_2      = apply(cpc[[6]], 1, function(x) quantile(x, c(0.025, 0.975)))

# - -------------------------------------------------------------------------------------------
library(stringr)

create.data.frame <- function(x, y){
  if(str_sub(deparse(substitute(x)),-1,-1)%in%1){gg='F'} else if (str_sub(deparse(substitute(x)),-1,-1)%in%2){gg='M'}
  if(str_sub(deparse(substitute(x)),-4,-3)%in%c('12')){ag='15-24'} else if(str_sub(deparse(substitute(x)),-4,-3)%in%c('_3')){ag='25-39'}
  return(data.frame(prevalence = x, gender = rep(gg, length(x)), 
  low95 = y[1,], up95 = y[2,], age = rep(ag, length(x)), 
  model=rep('Bayesian', length(x)))) }

v12F = create.data.frame(avg.prev_12_1,cis_12_1)
v12M = create.data.frame(avg.prev_12_2,cis_12_2)
v3F = create.data.frame(avg.prev_3_1, cis_3_1)
v3M = create.data.frame(avg.prev_3_2, cis_3_2)

ls.avg.prev = lapply(cpc, function(x){ apply(x, 1, mean) })
ls.cis.prev = lapply(cpc, function(x){ apply(x, 1, function(x) {quantile(x, c(0.025, 0.975))}) })

# choose year ---------------------------------------------------------------------------------
when.year = which(c(1999:2019)==2017)
# - -------------------------------------------------------------------------------------------

l1 =  lapply( lapply(ls.avg.prev, function(x) round(x/1000,0)), '[[', when.year)
l2 = lapply(lapply(ls.cis.prev, function(x) round(x/1000,0)) , function (x) x[,when.year])

keys = unique(c(names(l1), names(l2)))
sn =setNames(mapply(c, l1[keys], l2[keys]), keys)[1:3,]
#print('Prevalent cases, chosen year:'); #print(sn)
sn = matrix(as.vector(sn), byrow=T, nrow=2)
rownames(sn)=c('Females','Males'); 
colnames(sn)=c('Estim.15-19','2.5%','97.5%','Estim.20-24','2.5%','97.5%','Estim.25-39','2.5%','97.5%')

mean.quantile <- function(x){ apply(x, 1, function(y) c(mean(y), quantile(y, c(0.025, 0.975))))}
avg.prev.age1 = mean.quantile(ls.prev.u$prev11*ls.pop.u$pop11+ ls.prev.u$prev12*ls.pop.u$pop12)
avg.prev.age2 = mean.quantile(ls.prev.u$prev21*ls.pop.u$pop21+ ls.prev.u$prev22*ls.pop.u$pop22)
avg.prev.age3 = mean.quantile(ls.prev.u$prev31*ls.pop.u$pop31+ ls.prev.u$prev32*ls.pop.u$pop32)

print('Prevalent cases, both genders, chosen year:');
sn.tot = round( cbind(avg.prev.age1[,when.year],avg.prev.age2[,when.year],avg.prev.age3[,when.year])/1000, 0)
sn.tot = as.vector(sn.tot)
names(sn.tot)=c('Estim.15-19','2.5%','97.5%','Estim.20-24','2.5%','97.5%','Estim.25-39','2.5%','97.5%')

sn.ov.P = rbind(sn, sn.tot)
rownames(sn.ov.P)=c('Females','Males','Tot.'); 
#print(sn.ov.P)

# Incidence -----------------------------------------------------------------------------------
incid.overall_12_1 =  cic[[1]]+cic[[2]] 
incid.overall_12_2 =  cic[[4]]+cic[[5]] 

avg.incid_12_1 = apply(incid.overall_12_1, 1, mean)
icis_12_1      = apply(incid.overall_12_1, 1, function(x) quantile(x, c(0.025, 0.975)))
avg.incid_12_2 = apply(incid.overall_12_2, 1, mean)
icis_12_2      = apply(incid.overall_12_2, 1, function(x) quantile(x, c(0.025, 0.975)))

avg.incid_3_1 = apply(cic[[3]], 1, mean)
icis_3_1      = apply(cic[[3]], 1, function(x) quantile(x, c(0.025, 0.975)))
avg.incid_3_2 = apply(cic[[6]], 1, mean)
icis_3_2      = apply(cic[[6]], 1, function(x) quantile(x, c(0.025, 0.975)))

In12F = create.data.frame(avg.incid_12_1,icis_12_1)
In12M = create.data.frame(avg.incid_12_2,icis_12_2)
In3F = create.data.frame(avg.incid_3_1, icis_3_1)
In3M = create.data.frame(avg.incid_3_2, icis_3_2)

ls.avg.incid = lapply(cic, function(x){ apply(x, 1, mean) })
ls.cis.incid = lapply(cic, function(x){ apply(x, 1, function(x) {quantile(x, c(0.025, 0.975))}) })

l1 =  lapply( lapply(ls.avg.incid, function(x) round(x/1000,0)), '[[', when.year)
l2 = lapply(lapply(ls.cis.incid, function(x) round(x/1000,0)) , function (x) x[,when.year])

# Incidence, by age group by gender -----------------------------------------------------------
keys = unique(c(names(l1), names(l2)))
sn =setNames(mapply(c, l1[keys], l2[keys]), keys)[1:3,]
#print('Incident cases, chosen year:'); print(sn)
sn = matrix(as.vector(sn), byrow=T, nrow=2)
rownames(sn)=c('Females','Males'); 
colnames(sn)=c('Estim.15-19','2.5%','97.5%','Estim.20-24','2.5%','97.5%','Estim.25-39','2.5%','97.5%')

avg.incid.age1 = mean.quantile(ls.incid.u$incid11*ls.pop.u$pop11+ ls.incid.u$incid12*ls.pop.u$pop12)
avg.incid.age2 = mean.quantile(ls.incid.u$incid21*ls.pop.u$pop21+ ls.incid.u$incid22*ls.pop.u$pop22)
avg.incid.age3 = mean.quantile(ls.incid.u$incid31*ls.pop.u$pop31+ ls.incid.u$incid32*ls.pop.u$pop32)

# Incidence, by age group, genders combined ---------------------------------------------------
#print('Prevalent cases, both genders, chosen year:');
sn.tot = round( cbind(avg.incid.age1[,when.year],avg.incid.age2[,when.year],avg.incid.age3[,when.year])/1000, 0)

sn.tot = as.vector(sn.tot)
names(sn.tot)=c('Estim.15-19','2.5%','97.5%','Estim.20-24','2.5%','97.5%','Estim.25-39','2.5%','97.5%')

sn.ov.I = rbind(sn, sn.tot)
rownames(sn.ov.I)=c('Females','Males','Tot.'); 
#print(sn.ov.I)
# - -------------------------------------------------------------------------------------------
# - -------------------------------------------------------------------------------------------

# choose year ---------------------------------------------------------------------------------
Satt.year = which(c(1999:2019)==2008)
# - -------------------------------------------------------------------------------------------

dataprev = rbind(v12F[Satt.year,], v12M[Satt.year,], v3F[Satt.year,], v3M[Satt.year,])
dataincid = rbind(In12F[Satt.year,], In12M[Satt.year,], In3F[Satt.year,], In3M[Satt.year,])
rownames(dataprev)=rownames(dataincid)=NULL
colnames(dataincid)[1]='incidence'

# enter Satterwhite estimates -----------------------------------------------------------------
if(d==1){

# Satterwhite Chlamydia -----------------------------------------------------------------------
  satterwhite = data.frame(prevalence=c(660000,264000,342000,303000), gender=c('F','F','M','M'), 
  low95=c(660000*0.0226/0.0321,264000*0.004/0.0087,342000*0.0107/0.0166,303000*0.0056/0.0101 ), 
  up95=c(660000*0.0452/0.0321,264000*0.0186/0.0087,342000*0.0255/0.0166,303000*0.0181/0.0101 ), 
  age=rep(c('15-24','25-39'),2), model=rep('Satterwhite',4)) 
} else if (d==2){

# Satterwhite Gonorrhea -----------------------------------------------------------------------
  satterwhite = data.frame(prevalence=c(128000, 35000,67300, 39700), gender=c('F','F','M','M'),
  low95=c(128000*0.0038/0.0062, 3048, 67300*0.0012/0.0032, 15525 ),
  up95=c(128000*0.0103/0.0062, 77698, 67300*0.0084/0.0032, 42433), age=rep(c('15-24','25-39'),2), 
  model=rep('Satterwhite',4))
}

datatot = rbind(dataprev, satterwhite)

pop1524F = (ls.ipop.u$ipop11+ls.ipop.u$ipop21)[Satt.year]
pop2539F = ls.ipop.u$ipop31[Satt.year]
pop1524M = (ls.ipop.u$ipop12+ls.ipop.u$ipop22)[Satt.year]
pop2539M = ls.ipop.u$ipop32[Satt.year]

if(d==1){
  
# Satterwhite Chlamydia -----------------------------------------------------------------------
  satterwhite.incid = data.frame(incidence=c(957000,333000,833000,737000), gender=c('F','F','M','M'), 
   low95=c(pop1524F*0.0226/0.69,pop2539F*0.004/0.79, pop1524M*0.0107/0.41, pop2539M*0.0056/0.41 ), 
   up95=c(pop1524F*0.0452/0.69,pop2539F*0.0186/0.79,pop1524M*0.0255/0.41,pop2539M*0.0181/0.41 ), 
   age=rep(c('15-24','25-39'),2), model=rep('Satterwhite',4))
  
} else if(d==2){

  # Satterwhite Gonorrhea -----------------------------------------------------------------------
  satterwhite.incid = data.frame(incidence=c(277000,77000,293000,173000), gender=c('F','F','M','M'),
  low95=c(pop1524F*(0.0038/0.46), 3048/0.46, pop1524M*(0.0012/0.23), 15525/0.23 ),
  up95=c(pop1524F*(0.0103/0.46), 77698/0.46 , pop1524M*(0.0084/0.23), 42433/0.23 ), 
  age=rep(c('15-24','25-39'),2), model=rep('Satterwhite',4))
}  

datatot.incid = rbind(dataincid, satterwhite.incid)

# prepare for plots ---------------------------------------------------------------------------
datatot.gr = datatot %>% group_by(gender, age)

# Plots ---------------------------------------------------------------------------------------

namefile.prev = if(d==1){'/Fig6_Chlamydia_prevalence.pdf'} else if(d==2){'/Fig6_Gonorrhea_prevalence.pdf'}
title.prev    = if(d==1){'Chlamydia prevalence estimates for 2008: by age group, by model'} else 
  if(d==2){'Gonorrhea prevalence estimates for 2008: by age group, by model'}
namefile.incid = if(d==1){'/Fig6_Chlamydia_incidence.pdf'} else if(d==2){'/Fig6_Gonorrhea_incidence.pdf'}
title.incid    = if(d==1){'Chlamydia incidence estimates for 2008: by age group, by model'} else 
  if(d==2){'Gonorrhea incidence estimates for 2008: by age group, by model'}

#pdf(file=paste(results.dir,namefile.prev,sep=''), width=8.5, height=6)
#pdf(file=paste(results.dir,'/Fig6_Chlamydia_prevalence.pdf' ,sep=''), width=8.5, height=6)
# x11(width=8.5, height=6)
# cols = c('chartreuse3','cadetblue4')
# 
# plot.prev <- ggplot(datatot, aes(x=factor(age), y=prevalence, fill=factor(model)))+
#   geom_bar(position=position_dodge(width=0.55), stat='identity', width=0.5, alpha=0.75) + 
#   facet_wrap(~gender, labeller=as_labeller(c('F'='Females','M'='Males'))) +
#   geom_errorbar(aes(ymin=low95, ymax=up95), width=.05, position=position_dodge(.55), size=0.75)+
#   geom_text(data=datatot[datatot$model%in%'Bayesian',], aes(x=factor(age), y=prevalence, 
#         label=round(prevalence/1000,0)), vjust=-0.3, hjust=1.95, position=position_dodge(width=0.45), 
#         col='black', size=3.5) +
#   geom_text(data=datatot[datatot$model%in%'Satterwhite',], aes(x=factor(age), y=prevalence, 
#         label=round(prevalence/1000,0) ), vjust=-0.3, hjust=-0.95, position=position_dodge(width=0.45), 
#         col='black', size=3.5) +
#   xlab('Age group') + ylab('Prevalent cases (x 1000)') +
#   scale_y_continuous(breaks=seq(from=0, to=1250000, by=250000), labels=c('0','250','500','750','1000','1250')) +
#   scale_fill_manual(values=cols, name='Model') +
#   theme(strip.text.x = element_text(size =12, colour = "black", angle = 0))+
#   theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + 
#   ggtitle(title.prev)
# 
# print(plot.prev)
#dev.off()
#  
# x11(width=8.5, height=6)
# #pdf(file=paste(results.dir,namefile.incid,sep=''), width=8.5, height=6)
# plot.incid <- ggplot(datatot.incid, aes(x=factor(age), y=incidence, fill=factor(model)))+
#   geom_bar(position=position_dodge(width=0.55), stat='identity', width=0.5, alpha=0.75) + 
#   facet_wrap(~gender, labeller=as_labeller(c('F'='Females','M'='Males'))) +
#   geom_errorbar(aes(ymin=low95, ymax=up95), width=.05, position=position_dodge(.55), size=0.75)+
#   geom_text(data=datatot.incid[datatot.incid$model%in%'Bayesian',], aes(x=factor(age), y=incidence, 
#         label=round(incidence/1000,0)), vjust=-0.3, hjust=1.95, position=position_dodge(width=0.45), 
#         col='black', size=3.5) +
#   geom_text(data=datatot.incid[datatot.incid$model%in%'Satterwhite',], aes(x=factor(age), y=incidence, 
#         label=round(incidence/1000,0) ), vjust=-0.3, hjust=-0.95, position=position_dodge(width=0.45), 
#         col='black', size=3.5) +
#   xlab('Age group') + ylab('Incident cases (x 1000)') +
#   scale_y_continuous(breaks=seq(from=0, to=1250000, by=250000), labels=c('0','250','500','750','1000','1250')) +
#   scale_fill_manual(values=cols, name='Model') +
#   theme(strip.text.x = element_text(size =12, colour = "black", angle = 0))+
#   theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + 
#   ggtitle(title.incid)
# print(plot.incid)

#dev.off()
# - -------------------------------------------------------------------------------------------

# All ages combined ---------------------------------------------------------------------------
# 1. Prevalence -------------------------------------------------------------------------------
prev.overall_123_1 =  cpc[[1]]+cpc[[2]]+cpc[[3]] 
prev.overall_123_2 =  cpc[[4]]+cpc[[5]]+cpc[[6]]

avg.overall_123_1 = apply(prev.overall_123_1, 1, mean)
avg.overall_123_2 = apply(prev.overall_123_2, 1, mean)

cis_123_1      = apply(prev.overall_123_1, 1, function(x) quantile(x, c(0.025, 0.975)))
cis_123_2      = apply(prev.overall_123_2, 1, function(x) quantile(x, c(0.025, 0.975)))

overall_F = data.frame(prevalence = avg.overall_123_1, gender = rep('F', length(avg.overall_123_1)), 
      low95 = cis_123_1[1,], up95 = cis_123_1[2,], model=rep('Bayesian', length(avg.overall_123_1)))
overall_M = data.frame(prevalence = avg.overall_123_2, gender = rep('M', length(avg.overall_123_2)), 
      low95 = cis_123_2[1,], up95 = cis_123_2[2,], model=rep('Bayesian', length(avg.overall_123_2)))

satterwhite.sum = data.frame(id=c(30,30), prevalence=rbind( sum(satterwhite[c(1,2),1]), sum(satterwhite[c(3,4),1]))  , gender=c('F','M'), 
  low95=rbind(sum(satterwhite[c(1,2),3]), sum(satterwhite[c(3,4),3])) ,
  up95=rbind(sum(satterwhite[c(1,2),4]), sum(satterwhite[c(3,4),4])), model=rep('Satterwhite',2))

dataprev = rbind(overall_F, overall_M)
dataprev = cbind(id=rep(1:22,2), dataprev)

# in 2008 -------------------------------------------------------------------------------------
dataprev08 = rbind( dataprev[dataprev$id%in%c(Satt.year),], satterwhite.sum)
dataprev08$age = rep('combined', nrow(dataprev08))
dataprev08$id  = rep(Satt.year, nrow(dataprev08))

# in 2018 -------------------------------------------------------------------------------------
dataprev18 = rbind( dataprev[dataprev$id%in%c(when.year),])
dataprev18$age = rep('combined', nrow(dataprev18))
allages.P = round(dataprev18[,c(2,4,5)]/1000, 0)
colnames(allages.P)=c('Estim','2.5%','97.5%')
rownames(allages.P)=c('Females','Males')
#print(allages.P)

# 2. Incidence --------------------------------------------------------------------------------
incid.overall_123_1 =  cic[[1]]+cic[[2]]+cic[[3]]
incid.overall_123_2 =  cic[[4]]+cic[[5]]+cic[[6]]

I.overall_123_1 = apply(incid.overall_123_1, 1, mean)
I.overall_123_2 = apply(incid.overall_123_2, 1, mean)

Icis_123_1  = apply(incid.overall_123_1, 1, function(x) quantile(x, c(0.025, 0.975)))
Icis_123_2  = apply(incid.overall_123_2, 1, function(x) quantile(x, c(0.025, 0.975)))

Ioverall_F = data.frame(incidence = I.overall_123_1, gender = rep('F', length(I.overall_123_1)), 
          low95 = Icis_123_1[1,], up95 = Icis_123_1[2,], model=rep('Bayesian', length(I.overall_123_1)))
Ioverall_M = data.frame(incidence = I.overall_123_2, gender = rep('M', length(I.overall_123_2)), 
          low95 = Icis_123_2[1,], up95 = Icis_123_2[2,], model=rep('Bayesian', length(I.overall_123_2)))

Isatterwhite.sum = data.frame(id=c(30,30), incidence=rbind( sum(satterwhite.incid[c(1,2),1]), sum(satterwhite.incid[c(3,4),1]))  , gender=c('F','M'), 
  low95=rbind(sum(satterwhite.incid[c(1,2),3]), sum(satterwhite.incid[c(3,4),3])) ,
  up95=rbind(sum(satterwhite.incid[c(1,2),4]), sum(satterwhite.incid[c(3,4),4])), 
  model=rep('Satterwhite',2))

dataincid = rbind(Ioverall_F, Ioverall_M)
dataincid = cbind(id=rep(1:22,2), dataincid)


# in 2008 -------------------------------------------------------------------------------------
Idataincid08 = rbind( dataincid[dataincid$id%in%c(Satt.year),], Isatterwhite.sum)
Idataincid08$age = rep('combined', nrow(Idataincid08))
Idataincid08$id  = rep(Satt.year, nrow(dataprev08))
rownames(Idataincid08)=NULL

# in 2018 -------------------------------------------------------------------------------------
Idataincid18 = rbind( dataincid[dataincid$id%in%c(when.year),])
Idataincid18$age = rep('combined', nrow(Idataincid18))
rownames(Idataincid18)=NULL
Iallages = round(Idataincid18[,c(2,4,5)]/1000, 0)
colnames(Iallages)=c('Estim','2.5%','97.5%')
rownames(Iallages)=c('Females','Males')
#print(Iallages)

################################################################################################
avg.prev.ls = lapply(cpc, function(x){
  apply(x, 1, function(x) c(mean(x), quantile(x, c(0.025, 0.975)))) })
namestring = as.vector( sapply(c(1,2), function(g) {sapply(c(1:3), function(k){paste0('avg.prev_',k,g)})}))
names(avg.prev.ls)=namestring

create.data.frame.2 <- function(x){
 if(str_sub(deparse(substitute(x)),-1,-1)%in%1){gg='F'} else if (str_sub(deparse(substitute(x)),-1,-1)%in%2){gg='M'}
 if(str_sub(deparse(substitute(x)),-2,-2)%in%c('1')){ag='15-19'} else 
 if(str_sub(deparse(substitute(x)),-2,-2)%in%c('2')){ag='20-24'} else 
 if(str_sub(deparse(substitute(x)),-2,-2)%in%c('3')){ag='25-39'}
 return(data.frame(prevalence = x[1,], gender = rep(gg, length(x[1,])), 
 low95 = x[2,], up95 = x[3,], age = rep(ag, length(x[1,])), 
 model=rep('Bayesian', length(x[1,])))) }

F1=create.data.frame.2(avg.prev.ls$avg.prev_11)
F2=create.data.frame.2(avg.prev.ls$avg.prev_21)
F3=create.data.frame.2(avg.prev.ls$avg.prev_31)

M1=create.data.frame.2(avg.prev.ls$avg.prev_12)
M2=create.data.frame.2(avg.prev.ls$avg.prev_22)
M3=create.data.frame.2(avg.prev.ls$avg.prev_32)

# choose year ---------------------------------------------------------------------------------
sixteen = which(c(1999:2019)==2016)
eighteen = which(c(1999:2019)==2017)
# - -------------------------------------------------------------------------------------------

dataprev.year <- function(yr){
  dataprev.yr = rbind(F1[yr,], F2[yr,], F3[yr,], M1[yr,], M2[yr,], M3[yr,])
  colnames(dataprev.yr)[1]='prevalence'
  rownames(dataprev.yr)=NULL
  dataprev.yr$lab = round(dataprev.yr$prevalence/1000,0)
  return(dataprev.yr) }

dataprev16 = dataprev.year(sixteen)
dataprev18 = dataprev.year(eighteen)

# same with incidence -------------------------------------------------------------------------
avg.incid.ls = lapply(cic, function(x){
  apply(x, 1, function(x) c(mean(x), quantile(x, c(0.025, 0.975)))) })
namestring = as.vector( sapply(c(1,2), function(g) {sapply(c(1:3), function(k){paste0('avg.incid_',k,g)})}))
names(avg.incid.ls)=namestring

IF1=create.data.frame.2(avg.incid.ls$avg.incid_11)
IF2=create.data.frame.2(avg.incid.ls$avg.incid_21)
IF3=create.data.frame.2(avg.incid.ls$avg.incid_31)

IM1=create.data.frame.2(avg.incid.ls$avg.incid_12)
IM2=create.data.frame.2(avg.incid.ls$avg.incid_22)
IM3=create.data.frame.2(avg.incid.ls$avg.incid_32)

dataincid.year <- function(yr){
  dataincid.yr = rbind(IF1[yr,], IF2[yr,], IF3[yr,], IM1[yr,], IM2[yr,], IM3[yr,])
  colnames(dataincid.yr)[1]='incidence'
  rownames(dataincid.yr)=NULL
  dataincid.yr$lab = round(dataincid.yr$incidence/1000,0)
  return(dataincid.yr) }

dataincid16 = dataincid.year(sixteen)
dataincid18 = dataincid.year(eighteen)

################################################################################################
dataprev = rbind(overall_F, overall_M)
dataprev = cbind(id=rep(1:22,2), dataprev)
dataincid = rbind(Ioverall_F, Ioverall_M)
dataincid = cbind(id=rep(1:22,2), dataincid)


# Totals overall, ages and genders combined ---------------------------------------------------
PF = ls.prev.u$prev11*ls.pop.u$pop11+ls.prev.u$prev21*ls.pop.u$pop21+ls.prev.u$prev31*ls.pop.u$pop31
PM = ls.prev.u$prev12*ls.pop.u$pop12+ls.prev.u$prev22*ls.pop.u$pop22+ls.prev.u$prev32*ls.pop.u$pop32

IF = ls.incid.u$incid11*ls.pop.u$pop11+ls.incid.u$incid21*ls.pop.u$pop21+ls.incid.u$incid31*ls.pop.u$pop31
IM = ls.incid.u$incid12*ls.pop.u$pop12+ls.incid.u$incid22*ls.pop.u$pop22+ls.incid.u$incid32*ls.pop.u$pop32

Tot.Prev = PF + PM
Tot.Incid = IF + IM

Prev = apply(Tot.Prev, 1, mean); ci.P = apply(Tot.Prev, 1, function(x) quantile(x, c(0.025, 0.975)))
Incid = apply(Tot.Incid, 1, mean); ci.I = apply(Tot.Incid, 1, function(x) quantile(x, c(0.025, 0.975)))

prev.mat = round(c(Prev[when.year], ci.P[,when.year])/1000,0)
names(prev.mat) = c('Estim.','2.5%','97.5%')
  
incid.mat = round(c(Incid[when.year], ci.I[,when.year])/1000,0)
names(incid.mat) = c('Estim.','2.5%','97.5%')

#print( rbind(prev.mat, incid.mat) )

## prevalence
#print(cbind(sn.ov.P, rbind(allages.P, prev.mat)))
write.csv(cbind(sn.ov.P, rbind(allages.P, prev.mat)), file=paste0(results.dir,'Prevalence_table_d_',d,'.csv'))
## incidence
#print(cbind(sn.ov.I, rbind(Iallages, incid.mat)))
write.csv(cbind(sn.ov.I, rbind(Iallages, incid.mat)), file=paste0(results.dir,'Incidence_table_d_',d,'.csv'))

})

# create plots for screening and reporting ----------------------------------------------------
rm(list=ls())

diff.days= 0
setwd('/gplab/mazzola/msm/BayesianModeling')
overall.dir = getwd()
Sys.Date=Sys.Date()-diff.days
#results.dir = paste(overall.dir, '/Results/',Sys.Date,sep='')
results.dir  = paste0(overall.dir, '/Results/',Sys.Date,'/')
setwd(file.path(results.dir)) 

### total of data points case reports + prediction years
cr.pred <- 22
df.width <- 12 ## 2 genders, 2 disease, 3 ages = 3*2*2=12


library(tidyverse)

# function for transparent colors -------------------------------------------------------------
t_col <- function(color, percent = 50, name = NULL) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  invisible(t.col) }

# load all results and data from rjags model --------------------------------------------------
posts.complete <- load(file=list.files(pattern=glob2rx("*posts_complete*5pct*"))[1])
posts.complete <- fl
list.posts <- load(file=list.files(pattern=glob2rx("*list.posts*5pct*"))[1])
list.posts <- fl
data <- load(file=list.files(pattern=glob2rx("*data_allchain*5pct*"))[1])
data <- fl

# start with reporting completeness; create blank dataframe first -----------------------------
posts.rep = list.posts$rep.post
reporting.to.plot = data.frame(mean=rep(NA, cr.pred*df.width), 
      low95 = rep(NA, cr.pred*df.width), up95=rep(NA, cr.pred*df.width), 
      age.group=rep(NA, cr.pred*df.width), gender=rep(NA, cr.pred*df.width),
      infection=rep(NA, cr.pred*df.width), id=rep(1:cr.pred,df.width))

# populates data frame by going into the data and retrieving the right entries ----------------
invisible( 
  sapply(1:2, function(d){ sapply(1:2, function(g){ sapply(1:3, function(ia){
  index= grep(paste(',',ia,',',g,',',d,']', sep=''), colnames(posts.rep))
  posts.rep.d.g.ia = posts.rep[,index]
  mean.d.g.ia = apply(posts.rep.d.g.ia, 2, mean)
  quant.d.g.ia = apply(posts.rep.d.g.ia,2, function(x) quantile(x, c(0.025, 0.975)))
  
  reporting.to.plot[index, ] <<- cbind(mean.d.g.ia, quant.d.g.ia[1,], quant.d.g.ia[2,], 
       rep(ia, length(mean.d.g.ia)),
       rep(g, length(mean.d.g.ia)), rep(d, length(mean.d.g.ia)), 1:cr.pred)
  }) }) })
)

# rename columns to have correct labels for plots ---------------------------------------------
  reporting.to.plot[which(reporting.to.plot$age.group%in%1),'age.group']='15-19';  
  reporting.to.plot[which(reporting.to.plot$age.group%in%2),'age.group']='20-24';  
  reporting.to.plot[which(reporting.to.plot$age.group%in%3),'age.group']='25-39';  
  
  reporting.to.plot[which(reporting.to.plot$gender%in%1),'gender']='Females';  
  reporting.to.plot[which(reporting.to.plot$gender%in%2),'gender']='Males';  
 
  reporting.to.plot[which(reporting.to.plot$infection%in%1),'infection']='Chlamydia';  
  reporting.to.plot[which(reporting.to.plot$infection%in%2),'infection']='Gonorrhea'
  
# select from 2000 to 2017 for plots ----------------------------------------------------------
  index.years = which(c(1999:2019)%in%c(2000:2018))

  rep.chl = reporting.to.plot[reporting.to.plot$infection%in%'Chlamydia' & reporting.to.plot$id%in%index.years,]
  rep.gono = reporting.to.plot[reporting.to.plot$infection%in%'Gonorrhea' & reporting.to.plot$id%in%index.years,]
  
# plot ----------------------------------------------------------------------------------------
  cols = c('red','green','blue')
  #x11(width=11.6, height=6.2)
  pdf(file=paste(results.dir,'/Fig4_CT_rep_compl.pdf' ,sep=''), width=11.6, height=4.2)
  ggplot(rep.chl, aes(x=id, y=mean, color=age.group))+
    scale_color_manual(values=cols, name='Age groups')+ #scale_fill_manual(guide=FALSE)+
    geom_line(size=1, alpha=0.75 ) + facet_wrap(~gender)+
    geom_ribbon(aes(ymin=low95, ymax=up95, fill=age.group), alpha=0.2, linetype=0, show.legend=FALSE) +
    xlab('Year') + ylab('Percent reporting') +
    scale_y_continuous(limits = c(0.5,1), expand = c(0,0), labels=scales::percent ) +
    scale_x_continuous(breaks=seq(from=2, to=22, by=1), labels=as.character(2000:2020)) +
    theme(axis.text.x=element_text(angle= 20, size=7), strip.text.x=element_text(size=12, colour='black')) +
    ggtitle('Chlamydia')
  #  print(plot.C)
    dev.off()

  #x11(width=11.6, height=4.2)
  pdf(file=paste(results.dir,'/Fig4_GC_rep_compl.pdf' ,sep=''), width=11.6, height=4.2)
  ggplot(rep.gono, aes(x=id, y=mean, color=age.group))+
    scale_color_manual(values=cols, name='Age groups')+ #scale_fill_manual(guide=FALSE)+
    geom_line(size=1, alpha=0.75 ) + facet_wrap(~gender)+
    geom_ribbon(aes(ymin=low95, ymax=up95, fill=age.group), alpha=0.2, linetype=0, show.legend=FALSE) +
    xlab('Year') + ylab('Percent reporting') +
    scale_y_continuous(limits = c(0.5,1), expand = c(0,0), labels=scales::percent ) +
    scale_x_continuous(breaks=seq(from=2, to=22, by=1), labels=as.character(2000:2020)) +
    theme(axis.text.x=element_text(angle= 20, size=7), strip.text.x=element_text(size=12, colour='black')) +
    ggtitle('Gonorrhea')
  #  print(plot.G)
  dev.off()
   
# same idea for screening ---------------------------------------------------------------------
 posts.screen = posts.complete[[1]][,grep(paste('scr[',"[[:digit:]]+",',',"[[:digit:]]+",',',"[[:digit:]]+",',',"[[:digit:]]+",']', sep=''), 
                  colnames(posts.complete[[1]]))]
   
 screening.to.plot = data.frame(mean=rep(NA, cr.pred*df.width), 
    low95 = rep(NA, cr.pred*df.width), up95=rep(NA, cr.pred*df.width), 
    age.group=rep(NA, cr.pred*df.width), gender=rep(NA, cr.pred*df.width),
    infection=rep(NA, cr.pred*df.width), id=rep(1:cr.pred,df.width))
   

invisible(
    sapply(1:2, function(d){ sapply(1:2, function(g){ sapply(1:3, function(ia){
    index= grep(paste(',',ia,',',g,',',d,']', sep=''), colnames(posts.screen))
     
    posts.scr.d.g.ia = posts.screen[,index]
    mean.d.g.ia = apply(posts.scr.d.g.ia, 2, mean)
    quant.d.g.ia = apply(posts.scr.d.g.ia,2, function(x) quantile(x, c(0.025, 0.975)))
     
    screening.to.plot[index, ] <<- cbind(mean.d.g.ia, quant.d.g.ia[1,], quant.d.g.ia[2,], 
    rep(ia, length(mean.d.g.ia)), rep(g, length(mean.d.g.ia)), rep(d, length(mean.d.g.ia)), 1:cr.pred)
   }) }) })
)   
  
   screening.to.plot[which(screening.to.plot$age.group%in%1),'age.group']='15-19';  
   screening.to.plot[which(screening.to.plot$age.group%in%2),'age.group']='20-24';  
   screening.to.plot[which(screening.to.plot$age.group%in%3),'age.group']='25-39';  
   
   screening.to.plot[which(screening.to.plot$gender%in%1),'gender']='Females';  
   screening.to.plot[which(screening.to.plot$gender%in%2),'gender']='Males';  
   
   screening.to.plot[which(screening.to.plot$infection%in%1),'infection']='Chlamydia';  
   screening.to.plot[which(screening.to.plot$infection%in%2),'infection']='Gonorrhea'
   
   scr.chl = screening.to.plot[screening.to.plot$infection%in%'Chlamydia' & screening.to.plot$id%in%index.years,]
   scr.gono = screening.to.plot[screening.to.plot$infection%in%'Gonorrhea' & screening.to.plot$id%in%index.years,]
   
   cols = c('red','green','blue'); facet.labels = c('Chlamydia and Gonorrhea')
   pdf(file=paste(results.dir,'/Fig5_CT_screening.pdf' ,sep=''), width=11.6, height=4.2)
   #x11(width=11.6, height=4.2)
   ggplot(scr.chl, aes(x=id, y=mean, color=age.group))+
   scale_color_manual(values=cols, name='Age groups')+
   geom_line(size=1, alpha=0.75 ) + facet_wrap(~gender)+
   geom_ribbon(aes(ymin=low95, ymax=up95, fill=age.group), alpha=0.2, linetype=0, show.legend=FALSE) +
   xlab('Year') + ylab('Screening coverage (%)') +
   scale_y_continuous(limits = c(0,1.008), expand = c(0,0), labels=scales::percent ) +
   scale_x_continuous(breaks=seq(from=2, to=22, by=1), labels=as.character(2000:2020)) +
   theme(axis.text.x=element_text(angle= 20, size=7), strip.text.x=element_text(size=12, colour='black')) +
   ggtitle('Chlamydia')
   dev.off()
   
   pdf(file=paste(results.dir,'/Fig5_GC_screening.pdf' ,sep=''), width=11.6, height=4.2)
   ggplot(scr.gono, aes(x=id, y=mean, color=age.group))+
   scale_color_manual(values=cols, name='Age groups')+
   geom_line(size=1, alpha=0.75 ) + facet_wrap(~gender)+
   geom_ribbon(aes(ymin=low95, ymax=up95, fill=age.group), alpha=0.2, linetype=0, show.legend=FALSE) +
   labs(x='Year', y="Screening coverage (%)") +
   scale_y_continuous(limits = c(0,1), expand = c(0,0), labels=scales::percent ) +
   scale_x_continuous(breaks=seq(from=2, to=22, by=1), labels=as.character(2000:2020)) +
   theme(axis.text.x=element_text(angle= 20, size=7), strip.text.x=element_text(size=12, colour='black'))+
   ggtitle('Gonorrhea')
   dev.off()
   
# similarly for prevalence --------------------------------------------------------------------
  posts.mean.w    = list.posts$posts.mean.prev.w    
  posts.ci.w      = list.posts$posts.ci.prev.w              
  prevalence.to.plot = data.frame(mean=rep(NA, cr.pred*df.width), 
          low95 = rep(NA, cr.pred*df.width), up95=rep(NA, cr.pred*df.width), 
          age.group=rep(NA, cr.pred*df.width), gender=rep(NA, cr.pred*df.width),
          infection=rep(NA, cr.pred*df.width), id=rep(1:cr.pred,df.width))
  
invisible(
    sapply(1:2, function(d){ sapply(1:2, function(g){ sapply(1:3, function(ia){
    index= grep(paste(',',ia,',',g,',',d,']', sep=''), rownames(posts.mean.w))
    
    posts.prev.d.g.ia = posts.mean.w[index,1]
    ci.prev.d.g.ia = posts.ci.w[index,c(1,5)]
    
    prevalence.to.plot[index, ] <<- cbind(posts.prev.d.g.ia, ci.prev.d.g.ia[,1], ci.prev.d.g.ia[,2], 
    rep(ia, length(posts.prev.d.g.ia)),rep(g, length(posts.prev.d.g.ia)), 
    rep(d, length(posts.prev.d.g.ia)), 1:cr.pred)
  }) }) })
)

  prevalence.to.plot[which(prevalence.to.plot$age.group%in%1),'age.group']='15-19';  
  prevalence.to.plot[which(prevalence.to.plot$age.group%in%2),'age.group']='20-24';  
  prevalence.to.plot[which(prevalence.to.plot$age.group%in%3),'age.group']='25-39';  
  
  prevalence.to.plot[which(prevalence.to.plot$gender%in%1),'gender']='Females';  
  prevalence.to.plot[which(prevalence.to.plot$gender%in%2),'gender']='Males';  
  
  prevalence.to.plot[which(prevalence.to.plot$infection%in%1),'infection']='Chlamydia';  
  prevalence.to.plot[which(prevalence.to.plot$infection%in%2),'infection']='Gonorrhea'
  
  cols = c('red','green','blue')

  prev.chl = prevalence.to.plot[prevalence.to.plot$infection%in%'Chlamydia' & prevalence.to.plot$id%in%index.years,]
  prev.gono = prevalence.to.plot[prevalence.to.plot$infection%in%'Gonorrhea' & prevalence.to.plot$id%in%index.years,]
  
sapply(c('C','G'), function(inf){ #print(inf)
    
if(inf%in%c('C')){dd=prev.chl; main.title = 'Prevalence estimates for Chlamydia by age groups and gender'} else 
if(inf%in%c('G')){dd=prev.gono; main.title = 'Prevalence estimates for Gonorrhea by age groups and gender'}
    
source(paste(overall.dir,'/data_prep_for_mcmc.R', sep='') );

if(inf=='C'){ main.title = main.title 
  #'Prevalence estimates for Chlamydia by age groups and gender'; 
  d=1; rows=9; infe='Chlamydia'; dddF=data$P.hat[,,1,1]; dddM=data$P.hat[,,2,1]} else 
if(inf=='G'){ main.title = main.title
  #'Prevalence estimates for Gonorrhea by age groups and gender'; 
  d=2; rows=5; infe='Gonorrhea'; 
  dddF=data$P.hat[,,1,2]; dddM=data$P.hat[,,2,2]}
  
  # blank data frame first, then populate -------------------------------------------------------
  data.to.plot =data.frame( prev= rep(NA,rows*3*2), 
                age.group=rep(c(rep('15-19',rows),rep('20-24',rows),rep('25-39',rows)),2), 
                gender=c(rep('Females',rows*3) , rep('Males',rows*3)), infection= c(rep(infe,rows*3*2)))
  
  data.to.plot$prev[1:rows]=data$P.hat[1:rows,1,1,d]
  data.to.plot$prev[(rows+1):(2*rows)]=data$P.hat[1:rows,2,1,d]
  data.to.plot$prev[(2*rows+1):(3*rows)]=data$P.hat[1:rows,3,1,d]
  data.to.plot$prev[(3*rows+1):(4*rows)]=data$P.hat[1:rows,1,2,d]
  data.to.plot$prev[(4*rows+1):(5*rows)]=data$P.hat[1:rows,2,2,d]
  data.to.plot$prev[(5*rows+1):(6*rows)]=data$P.hat[1:rows,3,2,d]
  
  data.to.plot$x = if(inf=='C'){ rep(seq(from=1, to=18, by=2)+0.5, 6) } else 
    if(inf=='G'){rep(seq(from=1, to=10, by=2)+0.5, 6)}
  
  ag = c('15-19','20-24','25-39')
  ciF =ciM = NULL
  ciF = lapply(1:3, function(ia){
  cint = paste('095_',ag[ia], sep='')
  if(inf=='C'){dataci = chl.data.F} else if(inf=='G'){dataci=gon.data.F}
  ciF[[ia]] = dataci[,grep(cint, colnames(dataci))]
  })
  
  ci.to.plotF = data.frame(low95=rep(NA,rows*3), up95=rep(NA,rows*3), age.group=c(rep('15-19',rows),rep('20-24',rows),rep('25-39',rows)) )
  ci.to.plotF$low95 = unlist(lapply(ciF, '[',1))
  ci.to.plotF$up95 = unlist(lapply(ciF, '[',2))
  
  ciM = lapply(1:3, function(ia){
    cint = paste('095_',ag[ia], sep='')
    if(inf=='C'){dataci = chl.data.M} else if(inf=='G'){dataci=gon.data.M}
    ciM[[ia]] = dataci[,grep(cint, colnames(dataci))]
  })
  
  ci.to.plotM = data.frame(low95=rep(NA,rows*3), up95=rep(NA,rows*3), age.group=c(rep('15-19',rows),rep('20-24',rows),rep('25-39',rows)) )
  ci.to.plotM$low95 = unlist(lapply(ciM, '[',1))
  ci.to.plotM$up95 = unlist(lapply(ciM, '[',2))
  
  ci.to.plot = rbind(ci.to.plotF, ci.to.plotM)
  ci.to.plot$gender = c(rep("Females",rows*3), rep("Males",rows*3))
  ci.to.plot$x = if(inf=='C'){ rep(seq(from=1, to=18, by=2)+0.5, 6) } else if(inf=='G'){rep(seq(from=1, to=10, by=2)+0.5, 6)}
  
  
  data.to.plot = cbind(data.to.plot, ci.to.plot)
  data.to.plot = data.to.plot[,c('prev','low95','up95','age.group','gender','infection','x')]
  data.to.plot[is.na(data.to.plot)]=0
  
  data.to.plot[,c(1:3)] <- lapply(data.to.plot[,c(1:3)], function(x) as.numeric(x))

# plots ---------------------------------------------------------------------------------------
pdf(file=paste(results.dir,'/Fig2_',inf,'_Prevalence.pdf' ,sep=''), width=14.6, height=5)
ggplot(dd, aes(x=id, y=mean, color=age.group))+
    scale_color_manual(values=cols, name='Age groups')+
    geom_line(size=1, alpha=0.75 ) + facet_wrap(~gender)+
    geom_ribbon(aes(ymin=low95, ymax=up95, fill=age.group), alpha=0.2, linetype=0, show.legend=FALSE) +
    xlab('Year') + ylab('Average percent') +
   scale_x_continuous(breaks = seq(from=2, to=22, by=1), labels=as.character(2000:2020)) +
    scale_y_continuous(breaks = seq(from=0, to=max(dd[,c(1:3)]), by=round(max(dd[,c(1:3)]), digits=2)/4), labels=scales::percent)+
    expand_limits(y=0)+
    theme(axis.text.x=element_text(angle= 40, size=6.5), strip.text.x=element_text(size=12, colour='black')) +
    ggtitle(main.title)
    #print(plot.prev)
dev.off()

pdf(file=paste(results.dir,'/',inf,'Fig2A_Prevalence_with_data.pdf' ,sep=''), width=14.6, height=7.5)
ggplot(dd, aes(x=id, y=mean, color=age.group))+
    scale_color_manual(values=cols, name='Age groups')+
    geom_line(size=1, alpha=0.75 ) + facet_wrap(~gender+age.group)+
    geom_ribbon(aes(ymin=low95, ymax=up95, fill=age.group), alpha=0.2, linetype=0, show.legend=FALSE) +
    geom_pointrange(data=data.to.plot, aes(x=x, y=prev, ymin=low95, ymax=up95), col='darkred', size=0.35, shape=19, alpha=0.75) +
    xlab('Year') + ylab('Average percent') +
    scale_x_continuous(breaks = seq(from=2, to=22, by=1), labels=as.character(2000:2020)) +
    scale_y_continuous(breaks = seq(from=0, to=max(data.to.plot[,c(1:3)]), by=round(max(data.to.plot[,c(1:3)]), digits=2)/4), labels=scales::percent)+
    expand_limits(y=0)+
    theme(axis.text.x=element_text(angle= 40, size=6.5), strip.text.x=element_text(size=12, colour='black')) +
    ggtitle(main.title)
    #print(plot.prev.data)
dev.off()

})
 
# same for incidence --------------------------------------------------------------------------
 posts.mean.incid    = list.posts$posts.mean.incid    
 posts.ci.incid      = list.posts$posts.ci.incid              
 incidence.to.plot = data.frame(mean=rep(NA, cr.pred*df.width), 
        low95 = rep(NA, cr.pred*df.width), up95=rep(NA, cr.pred*df.width), 
        age.group=rep(NA, cr.pred*df.width), gender=rep(NA, cr.pred*df.width),
        infection=rep(NA, cr.pred*df.width), id=rep(1:cr.pred,df.width))
 
invisible( 
 sapply(1:2, function(d){ sapply(1:2, function(g){ sapply(1:3, function(ia){
   index= grep(paste(',',ia,',',g,',',d,']', sep=''), rownames(posts.mean.w))
   
   posts.incid.d.g.ia = posts.mean.incid[index,1]
   ci.incid.d.g.ia = posts.ci.incid[index,c(1,5)]
   
   incidence.to.plot[index, ] <<- cbind(posts.incid.d.g.ia, ci.incid.d.g.ia[,1], ci.incid.d.g.ia[,2], 
          rep(ia, length(posts.incid.d.g.ia)), rep(g, length(posts.incid.d.g.ia)), 
          rep(d, length(posts.incid.d.g.ia)), 1:cr.pred)
 }) }) })
)
 incidence.to.plot[which(incidence.to.plot$age.group%in%1),'age.group']='15-19';  
 incidence.to.plot[which(incidence.to.plot$age.group%in%2),'age.group']='20-24';  
 incidence.to.plot[which(incidence.to.plot$age.group%in%3),'age.group']='25-39';  
 
 incidence.to.plot[which(incidence.to.plot$gender%in%1),'gender']='Females';  
 incidence.to.plot[which(incidence.to.plot$gender%in%2),'gender']='Males';  
 
 incidence.to.plot[which(incidence.to.plot$infection%in%1),'infection']='Chlamydia';  
 incidence.to.plot[which(incidence.to.plot$infection%in%2),'infection']='Gonorrhea'
 
 incid.chl = incidence.to.plot[incidence.to.plot$infection%in%'Chlamydia' & incidence.to.plot$id%in%c(2:22),]
 incid.gono = incidence.to.plot[incidence.to.plot$infection%in%'Gonorrhea' & incidence.to.plot$id%in%c(2:22),]

 sapply(c('C','G'), function(inf){
   
  if(inf%in%c('C')){dd=incid.chl; main.title= 'Incidence estimates for Chlamydia by age groups and gender'} else 
  if(inf%in%c('G')){dd=incid.gono; main.title= 'Incidence estimates for Gonorrhea by age groups and gender'}
 
#x11(width=11.6, height=4.2)
pdf(file=paste(results.dir,'/Fig3_',inf,'_Incidence.pdf' ,sep=''), width=14.6, height=5)
ggplot(dd, aes(x=id, y=mean, color=age.group))+
   scale_color_manual(values=cols, name='Age groups')+ 
   geom_line(size=1, alpha=0.75 ) + facet_wrap(~gender)+
   geom_ribbon(aes(ymin=low95, ymax=up95, fill=age.group), alpha=0.2, linetype=0, show.legend=FALSE) +
   xlab('Year') + ylab('Average percent') + 
   scale_x_continuous(breaks = seq(from=2, to=22, by=1), labels=as.character(2000:2020)) +
   scale_y_continuous(breaks = seq(from=0, to=max(dd[,c(1:3)]), by=round(max(dd[,c(1:3)]), digits=2)/4), 
                      labels=scales::percent)+ expand_limits(y=0)+
   theme(axis.text.x=element_text(angle= 40, size=6.5), strip.text.x=element_text(size=12, colour='black')) +
   ggtitle(main.title)
#print(plot.incid)
dev.off() 
}) 
 
# case reports --------------------------------------------------------------------------------
posts.mean.cr = list.posts$posts.mean.theta  ## mean
posts.ci.cr	  = list.posts$posts.ci.theta    ## confidence intervals
posts.mean.R =  list.posts$posts.mean.R     ## mean NUMBER
posts.ci.R	  = list.posts$posts.ci.R       ## confidence intervals NUMBER

cr.to.plot = data.frame(mean=rep(NA, cr.pred*df.width), 
            low95 = rep(NA, cr.pred*df.width), up95=rep(NA, cr.pred*df.width), age.group=rep(NA, cr.pred*df.width),
            gender=rep(NA, cr.pred*df.width),infection=rep(NA, cr.pred*df.width), id=rep(1:cr.pred,df.width))

invisible(
sapply(1:2, function(d){ sapply(1:2, function(g){ sapply(1:3, function(ia){
  index= grep(paste(',',ia,',',g,',',d,']', sep=''), rownames(posts.mean.R))
  posts.cr.d.g.ia = posts.mean.R[index,1]*100000/data$N.pred[,ia,g,d]
  ci.cr.d.g.ia = posts.ci.R[index,c(1,5)]*100000/data$N.pred[,ia,g,d]
  cr.to.plot[index, ] <<- cbind(posts.cr.d.g.ia, ci.cr.d.g.ia[,1], ci.cr.d.g.ia[,2], 
    rep(ia, length(posts.cr.d.g.ia)),rep(g, length(posts.cr.d.g.ia)), rep(d, length(posts.cr.d.g.ia)), 1:cr.pred)
}) }) })
)

cr.to.plot[which(cr.to.plot$age.group%in%1),'age.group']='15-19';  
cr.to.plot[which(cr.to.plot$age.group%in%2),'age.group']='20-24';  
cr.to.plot[which(cr.to.plot$age.group%in%3),'age.group']='25-39';  

cr.to.plot[which(cr.to.plot$gender%in%1),'gender']='Females';  
cr.to.plot[which(cr.to.plot$gender%in%2),'gender']='Males';  

cr.to.plot[which(cr.to.plot$infection%in%1),'infection']='Chlamydia';  
cr.to.plot[which(cr.to.plot$infection%in%2),'infection']='Gonorrhea'

cols = c('red','green','blue')

cr.chl = cr.to.plot[cr.to.plot$infection%in%'Chlamydia' & cr.to.plot$id%in%c(2:22),]
cr.gono = cr.to.plot[cr.to.plot$infection%in%'Gonorrhea' & cr.to.plot$id%in%c(2:22),]

sapply(c('C','G'), function(inf){

if(inf%in%c('C')){dd=cr.chl; main.title= 'Case reports estimates for Chlamydia by age groups and gender'} else 
if(inf%in%c('G')){dd=cr.gono; main.title= 'Case reports estimates for Gonorrhea by age groups and gender'}

if(inf=='C'){ main.title = main.title 
  d=1; rows=22; infe='Chlamydia'; Bbreaks = seq(from=0, to=4000, by=500)
  dddF=(data$R.by.year[,,1,1])*100000/data$N.by.year[,,1,1]; 
  dddM=(data$R.by.year[,,2,1])*100000/data$N.by.year[,,2,1]} else 
if(inf=='G'){ main.title = main.title 
  d=2; rows=22; infe='Gonorrhea'; Bbreaks = seq(from=0, to=750, by=150)
  dddF=(data$R.by.year[,,1,2])*100000/data$N.by.year[,,1,2]; 
  dddM=(data$R.by.year[,,2,2])*100000/data$N.by.year[,,2,2]
}

## data
data.to.plot =data.frame( cr = rep(NA,rows*3*2), age.group=rep(c(rep('15-19',rows),rep('20-24',rows),rep('25-39',rows)),2), 
   gender=c(rep('Females',rows*3) , rep('Males',rows*3)), infection= c(rep(infe,rows*3*2)))

data.to.plot$cr = c(c(dddF[,1],NA,NA),c(dddF[,2],NA,NA),c(dddF[,3],NA,NA),c(dddM[,1],NA,NA),c(dddM[,2],NA,NA),c(dddM[,3],NA,NA))
data.to.plot$id = rep(1:22, 6)
data.to.plot = data.to.plot[data.to.plot$id%in%index.years,]

dd = dd[dd$id%in%index.years,]

#x11(width=11.6, height=4.5)
pdf(file=paste(results.dir,'/Fig3A_',inf,'_CaseRep_with_data.pdf' ,sep=''), width=11.6, height=4.5)
ggplot(dd, aes(x=id, y=mean, color=age.group))+
  scale_color_manual(values=cols, name='Age groups')+
  geom_line(size=1, alpha=0.75 ) + facet_wrap(~gender)+
  geom_ribbon(aes(ymin=low95, ymax=up95, fill=age.group), alpha=0.2, linetype=0, show.legend=FALSE) +
  geom_point(data=data.to.plot, aes(x=id, y=cr), color='darkred', size=1.5) +
  xlab('Year') + ylab('Average number x 100k individuals') +
  scale_x_continuous(breaks = seq(from=2, to=22, by=1), labels=as.character(2000:2020)) +
  scale_y_continuous(breaks = Bbreaks)+ expand_limits(y=0)+
  theme(axis.text.x=element_text(angle= 40, size=6.5), strip.text.x=element_text(size=12, colour='black')) +
  ggtitle(main.title)
#print(plot.cr.data)
dev.off() 

})

# other quantities (durations, etc) -----------------------------------------------------------
posts.m     = list.posts$m.post   
posts.dt	  = list.posts$dt.post      
posts.du    = list.posts$du.post      
posts.vs	  = list.posts$vs.post   

m.base.chlam = m.base.gono= rbeta(nrow(posts.m), 1,1)

prior.m = data.frame("m[1,1]"= data$m.resc[1,1,1]+(data$m.resc[1,2,1]-data$m.resc[1,1,1])*m.base.chlam,
    "m[2,1]"= data$m.resc[1,1,1]+(data$m.resc[1,2,1]-data$m.resc[1,1,1])*m.base.chlam,
    "m[1,2]"= data$m.resc[1,1,2]+(data$m.resc[1,2,2]-data$m.resc[1,1,2])*m.base.gono,
    "m[2,2]"= data$m.resc[2,1,2]+(data$m.resc[2,2,2]-data$m.resc[2,1,2])*m.base.gono )

# boxplots ------------------------------------------------------------------------------------
pdf(file=paste(results.dir,'/Fig_priorpost_m.pdf' ,sep=''), width=10, height=10); 

MM = matrix(c(1:4), byrow=T, ncol=2)
layout.show(layout(MM))
par(mar=c(4,4.5,4,0.5)+0.1)

# Prob.symptomatic case -----------------------------------------------------------------------
plot(1, type='n', xlim=c(0,1), ylim=c(0,.5), xlab='Probability of symptomatic case', yaxt='n', ylab='', axes=F); 
axis(1, at=seq(from=0, to=1, by=0.25))

  sapply(1:2, function(d){ sapply(1:2, function(g){
  colindex = grep(paste0(c(g,d), collapse='.'), colnames(prior.m)); #print(colindex)
  boxplot(prior.m[, colindex], boxwex=0.1, horizontal=T, add=T, bty='n', axes=F, xlab='', at=(colindex/8)-1/32, cex=0.5)
  boxplot(as.vector(posts.m[,paste0('m[',g,',',d,']') ]), boxwex=0.1, horizontal=T, add=T, bty='n', axes=F, xlab='', at=(colindex/8)-3/32,
          col=t_col('darkgreen',50,'darkgreenhue'), cex=0.5)
  if(g%in%1){G='Females'} else if(g%in%2){G='Males'}
  if(d%in%1){D='Chlamydia'} else if(d%in%2){D='Gonorrhea'}
  mtext(paste0(D,', ', G), side = 2, line=-4, at = colindex/8-1/16 , las=1, cex=0.9)
  })})
  abline(h=c(1/8,2/8,3/8), col='grey70')
  legend('bottomright', col=c('white',t_col('darkgreen',50,'darkgreenhue')), 
  legend = c('prior distribution', 'posterior distribution'), fill=c('white',t_col('darkgreen',50,'darkgreenhue')), bty='n')

# prob.symptomatic case detected --------------------------------------------------------------
vs.base = rbeta(nrow(posts.vs), 1,1)
prior.vs = data.frame("vs[1]"= 0.8+(0.9-0.8)*vs.base,"vs[2]"= 0.8+(0.9-0.8)*vs.base)

plot(1, type='n', xlim=c(0.5,1), ylim=c(0,.5), xlab='Probability that symptomatic case is detected', yaxt='n', ylab='', axes=F); 
axis(1, at=seq(from=0.5, to=1, by=0.25))

sapply(1:2, function(d){ 
  colindex = grep(paste0(c(d), collapse='.'), colnames(prior.vs)); #print(colindex)
  boxplot(prior.vs[, colindex], boxwex=0.1, horizontal=T, add=T, bty='n', axes=F, xlab='', at=(colindex+1)/8-1/32, cex=0.5)
  boxplot(as.vector(posts.vs[,paste0('vs[',d,']') ]), boxwex=0.1, horizontal=T, add=T, bty='n', axes=F, xlab='', at=(colindex+1)/8-3/32,
          col=t_col('darkgreen',50,'darkgreenhue'), cex=0.5)
  if(d%in%1){D='Chlamydia'} else if(d%in%2){D='Gonorrhea'}
  mtext(paste0(D,', Females and Males'), side = 2, line=-10, at = (colindex+1)/8-1/16 , las=1, cex=0.9)
})
legend('bottomright', col=c('white',t_col('darkgreen',50,'darkgreenhue')), 
legend = c('prior distribution', 'posterior distribution'), fill=c('white',t_col('darkgreen',50,'darkgreenhue')), bty='n')
abline(h=c(2/8), col='grey70')

# durations -----------------------------------------------------------------------------------
# priors directly from parameters in the data -------------------------------------------------
dt.base.beta = du.base.beta = rbeta(nrow(posts.dt), data$dur.parms[1,1,1],data$dur.parms[1,2,1])

prior.dt = data.frame("dt[1,1]"= data$duration.resc[2,1,1]+(data$duration.resc[2,2,1]-data$duration.resc[2,1,1])*dt.base.beta,
                     "dt[2,1]"= data$duration.resc[2,1,1]+(data$duration.resc[2,2,1]-data$duration.resc[2,1,1])*dt.base.beta,
                     "dt[1,2]"= data$duration.resc[2,1,2]+(data$duration.resc[2,2,2]-data$duration.resc[2,1,2])*dt.base.beta,
                     'dt[2,2]'= data$duration.resc[2,1,2]+(data$duration.resc[2,2,2]-data$duration.resc[2,1,2])*dt.base.beta )

prior.du = data.frame("du[1,1]"= data$duration.resc[1,1,1]+(data$duration.resc[1,2,1]-data$duration.resc[1,1,1])*dt.base.beta,
                      "du[2,1]"= data$duration.resc[1,1,1]+(data$duration.resc[1,2,1]-data$duration.resc[1,1,1])*dt.base.beta,
                      "du[1,2]"= data$duration.resc[1,1,2]+(data$duration.resc[1,2,2]-data$duration.resc[1,1,2])*dt.base.beta,
                      'du[2,2]'= data$duration.resc[1,1,2]+(data$duration.resc[1,2,2]-data$duration.resc[1,1,2])*dt.base.beta )

# duration treated, symptomatic ---------------------------------------------------------------
plot(1, type='n', xlim=c(0,0.3), ylim=c(0,.5), xlab='Duration of treated, symptomatic infection (years)', yaxt='n', ylab='', axes=F); 
axis(1, at=seq(from=0, to=0.3, by=0.1))

sapply(1:2, function(d){ sapply(1:2, function(g){
  colindex = grep(paste0(c(g,d), collapse='.'), colnames(prior.dt))
  boxplot(prior.dt[,colindex], boxwex=0.1, horizontal=T, add=T, bty='n', axes=F, xlab='', at=colindex/8-1/32, cex=0.5)
  boxplot(as.vector(posts.dt[,paste0('dt[',g,',',d,']') ]), boxwex=0.1, horizontal=T, add=T, bty='n', axes=F, xlab='', at=colindex/8-3/32,
          col=t_col('darkgreen',50,'darkgreenhue'), cex=0.5)
  if(g%in%1){G='Females'} else if(g%in%2){G='Males'}
  if(d%in%1){D='Chlamydia'} else if(d%in%2){D='Gonorrhea'}
  mtext(paste0(D,', ', G), side = 2, line=-4, at = colindex/8-1/16 , las=1, cex=0.9)
}) })
legend('topright', col=c('white',t_col('darkgreen',50,'darkgreenhue')), 
legend = c('prior distribution', 'posterior distribution'), fill=c('white',t_col('darkgreen',50,'darkgreenhue')), bty='n')
abline(h=c(1/8,2/8,3/8), col='grey70')

# duration untreated --------------------------------------------------------------------------
plot(1, type='n', xlim=c(0,2), ylim=c(0,.5), xlab='Duration of untreated infection (years)', yaxt='n', ylab='', axes=F); 
axis(1, at=seq(from=0, to=2, by=0.5))

sapply(1:2, function(d){ sapply(1:2, function(g){
  colindex = grep(paste0(c(g,d), collapse='.'), colnames(prior.dt))
  boxplot(prior.du[,colindex], boxwex=0.1, horizontal=T, add=T, bty='n', axes=F, xlab='', at=colindex/8-1/32, cex=0.5)
  boxplot(as.vector(posts.du[,paste0('du[',g,',',d,']') ]), boxwex=0.1, horizontal=T, add=T, bty='n', axes=F, xlab='', at=colindex/8-3/32,
          col=t_col('darkgreen',50,'darkgreenhue'), cex=0.5)
}) })
legend('topright', col=c('white',t_col('darkgreen',50,'darkgreenhue')), 
       legend = c('prior distribution', 'posterior distribution'), fill=c('white',t_col('darkgreen',50,'darkgreenhue')), bty='n')
abline(h=c(1/8,2/8,3/8), col='grey70')

dev.off()
# and we’re done ------------------------------------------------------------------------------
