rm(list=ls())
library(openxlsx)


# Set directories and file names ------------------------------------------
# overall.dir = "/gplab/mazzola/msm/BayesianModeling/"
# code.dir    = "/gplab/mazzola/msm/BayesianModeling/RCode/"
# data.dir    = "/gplab/mazzola/msm/BayesianModeling/Datafiles/"

bcb=''
overall.dir = paste0(bcb,"/homes/mazzola/")
code.dir    = paste0(bcb,"/homes/mazzola/RCode/")
data.dir    = paste0(bcb,"/homes/mazzola/Datafiles/")


data.dir1 = paste0(data.dir,'2020-05-11','/')
dir.create(file.path(data.dir1))
setwd(data.dir1)

data.chlamydia = "STI Incidence Inputs CH 10.25.18.xlsx"

#setwd(data.dir)


# Set fixed quantities/parameters -----------------------------------------
age.groups  = c('18-19','20-24','25-29','30-34','35-39', '25-39')
columns     = c('Prev','Std.err','low.095','up.095', 'n.obs')
labels      = sapply(columns, function(i) paste(i, age.groups, sep='_'))
names       = c('year', as.vector(t(labels)))


# Read-in data ------------------------------------------------------------

## parameters for read.xlsx function ###
sI = 1;             # sheet Index
cI = c(1, c(7:36))  # column Index
read.CH = function(x, rowRange){ read.xlsx(x, sheet=sI, colNames=TRUE, rows=rowRange, cols=cI ) }
########################################
chl.neg.F = read.CH(data.chlamydia, c(4:13)) #read.xlsx(data.chlamydia, sheetIndex=sI, header=TRUE, rowIndex=c(4:13), colIndex=cI )
chl.pos.F = read.CH(data.chlamydia, c(14:23)) #read.xlsx(data.chlamydia, sheetIndex=sI, header=TRUE, rowIndex=c(14:23),colIndex=cI )
chl.neg.M = read.CH(data.chlamydia, c(26:35)) #read.xlsx(data.chlamydia, sheetIndex=sI, header=TRUE, rowIndex=c(26:35), colIndex=cI )
chl.pos.M = read.CH(data.chlamydia, c(36:45)) #read.xlsx(data.chlamydia, sheetIndex=sI, header=TRUE, rowIndex=c(36:45), colIndex=cI )

## call all the same
colnames(chl.neg.F)=colnames(chl.pos.F)=colnames(chl.neg.M)=colnames(chl.pos.M)=names

##  handling of missing data (only on female data)
chl.neg.F[chl.neg.F=="."] = NA
chl.pos.F[chl.pos.F=="." | chl.pos.F=="(no observations)" | chl.pos.F=="NA"] = NA


# Set fixed quantities/parameters -----------------------------------------
age.groups=c('15-19','20-24','25-29','30-34','35-39','40-44','45-54','55-64','65+')
columns = c('cases','rate')
labels = sapply(columns, function(i) paste(i, age.groups, sep='_'))
names  = c('year', as.vector(t(labels)))

# Read-in Case reports data -----------------------------------------------

## parameters for read.xlsx function ###
sI = 2;             # sheet Index
cI = c(1:19)  # column Index
read.CH2 = function(x, rowRange,colRange){ read.xlsx(data.chlamydia, sheet=sI, colNames=TRUE, rows=rowRange, cols=colRange )}

########################################
cr.F = read.CH2(data.chlamydia, c(4:26), c(1:19) ) #read.xlsx(data.chlamydia, sheetIndex=sI, header=TRUE, rowIndex=c(4:26), colIndex=cI )
cr.M = read.CH2(data.chlamydia, c(27:49), c(1:19) ) #read.xlsx(data.chlamydia, sheetIndex=sI, header=TRUE, rowIndex=c(27:49), colIndex=cI )
cr.F_9699_40p = read.CH2(data.chlamydia, c(22:26), c(1,12:13) ) #read.xlsx(data.chlamydia, sheetIndex=sI, header=TRUE, rowIndex=c(22:26), colIndex=c(1,12:13) )
cr.M_9699_40p = read.CH2(data.chlamydia, c(45:49), c(1,12:13) ) #read.xlsx(data.chlamydia, sheetIndex=sI, header=TRUE, rowIndex=c(45:49), colIndex=c(1,12:13) )
colnames(cr.F)=colnames(cr.M)=names
colnames(cr.F_9699_40p)=colnames(cr.M_9699_40p) = c('year','cases_40+','rate_40+')

cr.F[c(19:22),grep('40-44',colnames(cr.F))]=cr.M[c(19:22),grep('40-44',colnames(cr.M))]=NA
### check
### cr.F[c(19:22),grep('40-44', colnames(cr.F))]
cr.F=cr.F[-18,]; cr.M=cr.M[-18,]
cr.F[cr.F=="NA" | is.na(cr.F)]=cr.M[cr.M=="NA" | is.na(cr.M)]=NA

### additional data (case reports 2017 AND 2018)
cr.2017.2018 = read.xlsx("Data for STI Incidence_MB5_2018_YM.xlsx", sheet=1, colNames=TRUE, rows=c(30:42), cols=c(2:9,11:17) )

females.2017.2018 = cr.2017.2018[ cr.2017.2018[,'Age.Group']%in%age.groups, grep('Age.Group|Female', colnames(cr.2017.2018)) ]
males.2017.2018 = cr.2017.2018[ cr.2017.2018[,'Age.Group']%in%age.groups, grep('Age.Group|Male', colnames(cr.2017.2018)) ]

females.2017.2018[,'Age.Group']=paste0('cases_',females.2017.2018[,'Age.Group'])
males.2017.2018[,'Age.Group']=paste0('cases_',males.2017.2018[,'Age.Group'])

cr.F = rbind(rep(NA, ncol(cr.F)), rbind(rep(NA, ncol(cr.F)), cr.F))
cr.M = rbind(rep(NA, ncol(cr.M)), rbind(rep(NA, ncol(cr.M)), cr.M))

cr.F[2, 2:ncol(cr.F)]  = unlist( lapply(age.groups, function(i){
  females.2017.2018[ grep(i, females.2017.2018[,'Age.Group']), c('Female','Female.1')] }) )
cr.F[1, 2:ncol(cr.F)]  = unlist( lapply(age.groups, function(i){
  females.2017.2018[ grep(i, females.2017.2018[,'Age.Group']), c('Female.2','Female.3')] }) )

cr.M[2, 2:ncol(cr.M)]  = unlist( lapply(age.groups, function(i){
  males.2017.2018[ grep(i, males.2017.2018[,'Age.Group']), c('Male','Male.1')] }) )
cr.M[1, 2:ncol(cr.M)]  = unlist( lapply(age.groups, function(i){
  males.2017.2018[ grep(i, males.2017.2018[,'Age.Group']), c('Male.2','Male.3')] }) )

levels(cr.F[,1]) <- c(levels(cr.F[,1]),'2017','2018')
levels(cr.M[,1]) <- c(levels(cr.M[,1]),'2017','2018')

cr.F[2,1] = cr.M[2,1] = '2017'
cr.F[1,1] = cr.M[1,1] = '2018'

# Population denominators -------------------------------------------------

## parameters for read.xlsx function ###
sI =3;             # sheet Index
cI = c(1:24)  # column Index
########################################

deno.F = read.CH(data.chlamydia, c(3:14)) #read.xlsx(data.chlamydia, sheetIndex=sI, header=TRUE, rowIndex=c(3:14), colIndex=cI )
deno.M = read.CH(data.chlamydia, c(15:26)) #read.xlsx(data.chlamydia, sheetIndex=sI, header=TRUE, rowIndex=c(15:26), colIndex=cI )

columns = as.character(c(c(1996:2009),'Jul_1_2010',c(2011:2018)))
rows = gsub('\\.','',deno.F[,1])
rows = gsub(' to ','-',rows)

colnames(deno.F)=colnames(deno.M)=c('',columns)
rownames(deno.F)=rownames(deno.M)=rows

## data in thousands 1996-1999a
deno.F[,colnames(deno.F)%in%as.character(1996:1999)] = deno.F[,colnames(deno.F)%in%as.character(1996:1999)]*1000
deno.M[,colnames(deno.M)%in%as.character(1996:1999)] = deno.M[,colnames(deno.M)%in%as.character(1996:1999)]*1000


deno.F = deno.F[,-1]
deno.M = deno.M[,-1]
################################################
## objects out:
##
## chl.pos.F/chl.neg.F = estimated prevalence, std.error and 95%CIs for Females who tested positive/negative to chlamyidia from NHANES data
## chl.pos.M/chl.neg.M = estimated prevalence, std.error and 95%CIs for Males who tested positive/negative to chlamyidia from NHANES data
## cr.F/cr.M		   = number of reported cases among Females/Males
## cr.F_9699_40p/cr.M_9699_40p = number of reported cases 1996-1999 in "40+" age group
## deno.F/deno.M	   = population denominator for Females/Males


# Save case reports and denominator data ----------------------------------
save(cr.F, file='cr_F_18.rda')
save(cr.M, file='cr_M_18.rda')
save(deno.F, file='deno_F_18.rda')
save(deno.M, file='deno_M_18.rda')


# Prevelence ages 15-19 ---------------------------------------------------
prev.F.1519 = read.xlsx("New RDC Results 6.29.18.xlsx", sheet=1, colNames=TRUE, rows=c(79:88), cols=c(1:7) )
  #read.xlsx("NHANES trends adolescents chlamydia.xlsx", sheetIndex=1, header=TRUE, rowIndex=c(2,10), colIndex=c(2:8) )
prev.M.1519 = read.xlsx("New RDC Results 6.29.18.xlsx", sheet=1, colNames=TRUE, rows=c(98:107), cols=c(1:7) )
  #read.xlsx("NHANES trends adolescents chlamydia.xlsx", sheetIndex=1, header=TRUE, rowIndex=c(2,9), colIndex=c(2:8) )

colnames(prev.F.1519) = colnames(prev.M.1519) = c("year", "n.obs_15-19","deno_15-19","Prev_15-19","Std.err_15-19","low.095_15-19","up.095_15-19")
# vals.F = cbind(unlist(lapply(prev.F.1519, as.character)))[,1]
# prev_F_1519 = as.numeric(unlist(lapply(1:length(vals.F), function(i) unlist(strsplit(vals.F[i], '[()]'))[1])))
# prev_F_1519 = c(prev_F_1519, NA)
# names(prev_F_1519)=c( substring(gsub('\\.','-',names(prev.F.1519) ),2), '2013-2014')
# names(prev_F_1519)[3:4]= substring(names(prev_F_1519)[3:4],1,nchar(names(prev_F_1519)[3])-1)
# prev_F_1519=as.data.frame(prev_F_1519)
# colnames(prev_F_1519)='Prev_15-19'
# 
# 
# ci.F.1519 = cbind(unlist(lapply(1:length(vals.F), function(i) unlist(strsplit(vals.F[i], '[()]'))[2]) ))
# ci.F.1519 = strsplit(ci.F.1519[,1],',')
# ci.F.1519 = matrix(as.numeric(unlist(ci.F.1519)), byrow=T, ncol=2)
# ci.F.1519 = rbind(ci.F.1519,c(NA,NA))
# rownames(ci.F.1519)=rownames(prev_F_1519)
# colnames(ci.F.1519)=c('low.095_15-19','up.095_15-19')
# data1519F = cbind(prev_F_1519/100, ci.F.1519/100)
# data1519F = rbind( data1519F,rep(NA, ncol(data1519F)))
# rownames(data1519F)[nrow(data1519F)]='2015-2016'
chl.pos.F = cbind(prev.F.1519[,-1], chl.pos.F)
# 
# 
# vals.M = cbind(unlist(lapply(prev.M.1519, as.character)))[,1]
# prev_M_1519 = as.numeric(unlist(lapply(1:length(vals.M), function(i) unlist(strsplit(vals.M[i], '[()]'))[1])))
# prev_M_1519 = c(prev_M_1519, NA)
# 
# names(prev_M_1519)=c(substring(gsub('\\.','-',names(prev.M.1519) ),2), '2013-2014')
# names(prev_M_1519)[3:4]= substring(names(prev_M_1519)[3:4],1,nchar(names(prev_M_1519)[3])-1)
# prev_M_1519=as.data.frame(prev_M_1519)
# colnames(prev_M_1519)='Prev_15-19'
# 
# 
# ci.M.1519 = cbind(unlist(lapply(1:length(vals.M), function(i) unlist(strsplit(vals.M[i], '[()]'))[2]) ))
# ci.M.1519 = strsplit(ci.M.1519[,1],',')
# ci.M.1519 = matrix(as.numeric(unlist(ci.M.1519)), byrow=T, ncol=2)
# ci.M.1519 = rbind(ci.M.1519,c(NA,NA))
# rownames(ci.M.1519)=rownames(prev_M_1519)
# colnames(ci.M.1519)=c('low.095_15-19','up.095_15-19')
# data1519M = cbind(prev_M_1519/100, ci.M.1519/100)
# data1519M = rbind( data1519M,rep(NA, ncol(data1519M)))
# rownames(data1519M)[nrow(data1519M)]='2015-2016'
# 
 chl.pos.M = cbind(prev.M.1519[,-1], chl.pos.M)


# Save prevalence data ----------------------------------------------------
#save(prev_F_1519, file='prev_F_1519.rda')
#save(ci.F.1519, file='ci.F.1519.rda')
#save(prev_M_1519, file='prev_M_1519.rda')
#save(ci.M.1519, file='ci.M.1519.rda')

save(chl.neg.F, file='chl_neg_F.rda')
save(chl.pos.F, file='chl_pos_F.rda')
save(chl.neg.M, file='chl_neg_M.rda')
save(chl.pos.M, file='chl_pos_M.rda')