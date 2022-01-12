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


data.gono = "STI Incidence Inputs GC 4.4.18.xlsx"

data.dir1 = paste0(data.dir,'2020-05-11','/')
dir.create(file.path(data.dir1))
setwd(data.dir1)



#setwd(data.dir)


# Set fixed quantities/parameters -----------------------------------------
age.groups=c('18-19','20-24','25-29','30-34','35-39','25-39')
columns = c('Prev','Std.err','low.095','up.095', 'n.obs')
labels = sapply(columns, function(i) paste(i, age.groups, sep='_'))
names  = c('year', as.vector(t(labels)))

# Read-in data ------------------------------------------------------------

## parameters for read.xlsx function ###
sI = 1;       # sheet Index
cI = c(1:31)  # column Index
#read.gono =function(x, rowRange){read.xlsx(x, sheetIndex=sI, header=TRUE, rowIndex=rowRange, colIndex=cI)}
read.gono =function(x, rowRange){read.xlsx(x, sheet=sI, colNames=TRUE, rows=rowRange, cols=cI, na.strings=c('NA','',' '))}


########################################
gon.neg.F = read.gono(data.gono,c(4:10))
gon.pos.F = read.gono(data.gono, c(11:17))
gon.neg.M = read.gono(data.gono,c(20:26) )
gon.pos.M = read.gono(data.gono, c(27:33)) 
##
colnames(gon.neg.F) <- colnames(gon.pos.F) <- names
colnames(gon.neg.M) <- names[-25]
colnames(gon.pos.M) <- names[-c(23:25)]

  
## missing data 
gon.neg.F[gon.neg.F=="."] = NA
gon.pos.F[gon.pos.F=="." | gon.pos.F=="(no observations)" | gon.pos.F=="NA"] = NA
gon.neg.M[gon.neg.M=="."] = NA
gon.pos.M[gon.pos.M=="." | gon.pos.M=="(no observations)" | gon.pos.M=="NA"] = NA

# Set fixed quantities/parameters -----------------------------------------
age.groups=c('15-19','20-24','25-29','30-34','35-39','40-44','45-54','55-64','65+', 'Age 18', 'Age 19')
columns = c('cases','rate')
labels = sapply(columns, function(i) paste(i, age.groups, sep='_'))
names  = c('year', as.vector(t(labels)))

# Read-in Case reports data -----------------------------------------------

## parameters for read.xlsx function ###
sI = 2;             # sheet Index
#read.gono2 =function(x, rowRange, colRange){read.xlsx(x, sheetIndex=sI, header=TRUE, rowIndex=rowRange, colIndex=colRange)}
read.gono2 =function(x, rowRange, colRange){read.xlsx(x, sheet=sI, colNames=TRUE, rows=rowRange, cols=colRange)}
########################################

crGon.F = read.gono2(data.gono, c(4:26), c(1:23)) #read.xlsx(datafile, sheetIndex=2, header=TRUE, rowIndex=c(4:26), colIndex=c(1:23) )
crGon.M = read.gono2(data.gono,c(27:49), c(1:19)) #read.xlsx(datafile, sheetIndex=2, header=TRUE, rowIndex=c(27:49), colIndex=c(1:19) )
crGon.F_9699_40p = read.gono2(data.gono, c(23:26), c(1,12:13)) #read.xlsx(datafile, sheetIndex=2, header=TRUE, rowIndex=c(23:26), colIndex=c(1,12:13) )
crGon.M_9699_40p = read.gono2(data.gono, c(46:49), c(1,12:13)) #read.xlsx(datafile, sheetIndex=2, header=TRUE, rowIndex=c(46:49), colIndex=c(1,12:13) )

colnames(crGon.F)=names
colnames(crGon.M)=names[1:19]
colnames(crGon.F_9699_40p)=colnames(crGon.M_9699_40p) = c('year','cases_40+','rate_40+')

crGon.F[c(18:22),grep('40-44',colnames(crGon.F))]=crGon.M[c(18:22),grep('40-44',colnames(crGon.M))]=NA
### check
### cr.F[c(18:21),grep('40-44', colnames(cr.F))]

crGon.F=crGon.F[-18,]; crGon.M=crGon.M[-18,]
crGon.F[crGon.F=="NA" | is.na(crGon.F)]=crGon.M[crGon.M=="NA" | is.na(crGon.M)]=NA

### additional data (case reports 2017 AND 2018)
#cr.2017 = read.xlsx("Data for STI Incidence_MB5.xlsx", sheetIndex=2, header=TRUE, rowIndex=c(25:37), colIndex=c(2:9) )
cr.2017.2018 = read.xlsx("Data for STI Incidence_MB5_2018_YM.xlsx", sheet=2, colNames=TRUE, rows=c(25:37), cols=c(2:9,11:17) )

# females.2017 = cr.2017[ cr.2017[,'Age.Group']%in%age.groups, grep('Age.Group|Female', colnames(cr.2017)) ]
# males.2017 = cr.2017[ cr.2017[,'Age.Group']%in%age.groups, grep('Age.Group|Male', colnames(cr.2017)) ]
# 
# females.2017[,'Age.Group']=paste0('cases_',females.2017[,'Age.Group'])
# males.2017[,'Age.Group']=paste0('cases_',males.2017[,'Age.Group'])

females.2017.2018 = cr.2017.2018[ cr.2017.2018[,'Age.Group']%in%age.groups, grep('Age.Group|Female', colnames(cr.2017.2018)) ]
males.2017.2018 = cr.2017.2018[ cr.2017.2018[,'Age.Group']%in%age.groups, grep('Age.Group|Male', colnames(cr.2017.2018)) ]

females.2017.2018[,'Age.Group']=paste0('cases_',females.2017.2018[,'Age.Group'])
males.2017.2018[,'Age.Group']=paste0('cases_',males.2017.2018[,'Age.Group'])

# crGon.F = rbind(rep(NA, ncol(crGon.F)), crGon.F)
# crGon.M = rbind(rep(NA, ncol(crGon.M)), crGon.M)
# 
# crGon.F[1, grep(paste0(age.groups[1:9],collapse='|'), colnames(crGon.F))]  = unlist( lapply(age.groups, function(i){
#   females.2017[ grep(i, females.2017[,'Age.Group']), c('Female','Female.1')] }) )
# crGon.M[1, grep(paste0(age.groups[1:9],collapse='|'), colnames(crGon.M))]  = unlist( lapply(age.groups, function(i){
#   males.2017[ grep(i, males.2017[,'Age.Group']), c('Male','Male.1')] }) )
# 
# levels(crGon.F[,1]) <- c(levels(crGon.F[,1]),'2017')
# levels(crGon.M[,1]) <- c(levels(crGon.M[,1]),'2017')
# crGon.F[1,1] =crGon.M[1,1] = '2017'

crGon.F = rbind(rep(NA, ncol(crGon.F)), rbind(rep(NA, ncol(crGon.F)), crGon.F))
crGon.M = rbind(rep(NA, ncol(crGon.M)), rbind(rep(NA, ncol(crGon.M)), crGon.M))

whichages <- age.groups[grep(paste0(age.groups, collapse='|'),females.2017.2018$Age.Group)]
whichcols <- colnames(crGon.F)[grep(paste0(whichages, collapse='|'), colnames(crGon.F))]
whichcols <- which(colnames(crGon.F)%in%whichcols)

crGon.F[2, whichcols]  = unlist( lapply(whichages, function(i){
  females.2017.2018[ grep(i, females.2017.2018[,'Age.Group']), c('Female','Female.1')] }) )
crGon.F[1, whichcols]  = unlist( lapply(whichages, function(i){
  females.2017.2018[ grep(i, females.2017.2018[,'Age.Group']), c('Female.2','Female.3')] }) )

crGon.M[2, whichcols]  = unlist( lapply(whichages, function(i){
  males.2017.2018[ grep(i, males.2017.2018[,'Age.Group']), c('Male','Male.1')] }) )
crGon.M[1, whichcols]  = unlist( lapply(whichages, function(i){
  males.2017.2018[ grep(i, males.2017.2018[,'Age.Group']), c('Male.2','Male.3')] }) )

levels(crGon.F[,1]) <- c(levels(crGon.F[,1]),'2017','2018')
levels(crGon.M[,1]) <- c(levels(crGon.M[,1]),'2017','2018')

crGon.F[2,1] = crGon.M[2,1] = '2017'
crGon.F[1,1] = crGon.M[1,1] = '2018'


# Population denominators -------------------------------------------------
## parameters for read.xlsx function ###
sI = 3;       # sheet Index
cI = c(1:24)  # column Index
########################################
data.chlamydia = "STI Incidence Inputs CH 10.25.18.xlsx"

denoGon.F = read.gono(data.chlamydia, c(3:14)) #read.xlsx(data.chlamydia, sheetIndex=sI, header=TRUE, rowIndex=c(3:14), colIndex=cI )
denoGon.M = read.gono(data.chlamydia, c(15:26)) #read.xlsx(data.chlamydia, sheetIndex=sI, header=TRUE, rowIndex=c(15:26), colIndex=cI )

columns = as.character(c(c(1996:2009),'Jul_1_2010',c(2011:2018)))
rows = gsub('\\.','',denoGon.F[,1])
rows = gsub(' to ','-',rows)

colnames(denoGon.F)=colnames(denoGon.M)=c('',columns)
rownames(denoGon.F)=rownames(denoGon.M)=rows

## data in thousands 1996-1999a
denoGon.F[,colnames(denoGon.F)%in%as.character(1996:1999)] = denoGon.F[,colnames(denoGon.F)%in%as.character(1996:1999)]*1000
denoGon.M[,colnames(denoGon.M)%in%as.character(1996:1999)] = denoGon.M[,colnames(denoGon.M)%in%as.character(1996:1999)]*1000

denoGon.F = denoGon.F[,-1]
denoGon.M = denoGon.M[,-1]

# denoGon.F = read.gono(data.gono, c(3:15))  #read.xlsx(datafile, sheetIndex=3, header=TRUE, rowIndex=c(3:15), colIndex=c(1:22) )
# denoGon.M = read.gono(data.gono, c(16:28)) #read.xlsx(datafile, sheetIndex=3, header=TRUE, rowIndex=c(16:28),colIndex=c(1:22) )
# columns = as.character(c(c(1996:2009),'Jul_1_2010',c(2011:2016)))
# rows = gsub('\\.','',denoGon.F[,1])
# rows = gsub(' to ','-',rows)
# colnames(denoGon.F)=colnames(denoGon.M)=c('',columns)
# rownames(denoGon.F)=rownames(denoGon.M)=rows
# ## data in thousands 1996-1999a
# denoGon.F[,colnames(denoGon.F)%in%as.character(1996:1999)] = denoGon.F[,colnames(denoGon.F)%in%as.character(1996:1999)]*1000
# denoGon.M[,colnames(denoGon.M)%in%as.character(1996:1999)] = denoGon.M[,colnames(denoGon.M)%in%as.character(1996:1999)]*1000
# denoGon.F = denoGon.F[,-1]
# denoGon.M = denoGon.M[,-1]
################################################
## objects out:
##
## chl.pos.F/chl.neg.F = estimated prevalence, std.error and 95%CIs for Females who tested positive/negative to chlamyidia from NHANES data
## chl.pos.M/chl.neg.M = estimated prevalence, std.error and 95%CIs for Males who tested positive/negative to chlamyidia from NHANES data
## cr.F/cr.M		   = number of reported cases among Females/Males
## cr.F_9699_40p/cr.M_9699_40p = number of reported cases 1996-1999 in "40+" age group
## deno.F/deno.M	   = population denominator for Females/Males

# Save case reports and denominator data ----------------------------------
save(crGon.F, file='crGon_F.rda')
save(crGon.M, file='crGon_M.rda')
save(denoGon.F, file='denoGon_F.rda')
save(denoGon.M, file='denoGon_M.rda')


# Prevelence ages 15-19 ---------------------------------------------------
# prev.F.1519 = read.xlsx("NHANES RDC Request - 170705 - MR.xlsx", sheetIndex=2, header=TRUE, rowIndex=c(41:47), colIndex=c(1:7) )
# prev.M.1519 = read.xlsx("NHANES RDC Request - 170705 - MR.xlsx", sheetIndex=2, header=TRUE, rowIndex=c(51:57), colIndex=c(1:7) )
prev.F.1519 = read.xlsx("NHANES RDC Request - 170705 - MR.xlsx", sheet=2, colNames=TRUE, rows=c(41:47), cols=c(1:7) )
prev.M.1519 = read.xlsx("NHANES RDC Request - 170705 - MR.xlsx", sheet=2, colNames=TRUE, rows=c(51:57), cols=c(1:7) )


prev_F_1519 = prev.F.1519[-1,c(1:7)]
prev_F_1519[,c(2:5)]=sapply(2:5, function(i) as.numeric(as.character(prev_F_1519[,i])))
colnames(prev_F_1519) = c('year', 'n.with.gono','denominator', 'Prev_15-19', 'Std.err_15-19','low.095_15-19','up.095_15-19')
prev_F_1519[,1] = c('1999-2000','2001-2002','2003-2004','2005-2006','2007-2008')

prev_M_1519 = prev.M.1519[-1,c(1:7)]
prev_M_1519[,c(2:5)]=sapply(2:5, function(i) as.numeric(as.character(prev_M_1519[,i])))
colnames(prev_M_1519) = c('year', 'n.with.gono','denominator', 'Prev_15-19', 'Std.err_15-19','low.095_15-19','up.095_15-19')
prev_M_1519[,1] = c('1999-2000','2001-2002','2003-2004','2005-2006','2007-2008')

#vals.F = cbind(unlist(lapply(prev.F.1519, as.character)))[,1]
# prev_F_1519 = as.numeric(unlist(lapply(1:length(vals.F), function(i) unlist(strsplit(vals.F[i], '[()]'))[1])))
# prev_F_1519 = c(prev_F_1519, NA)
# names(prev_F_1519)=c( substring(gsub('\\.','-',names(prev.F.1519) ),2), '2013-2014')
# names(prev_F_1519)[3:4]= substring(names(prev_F_1519)[3:4],1,nchar(names(prev_F_1519)[3])-1)
# prev_F_1519=as.data.frame(prev_F_1519)
# colnames(prev_F_1519)='Prev_15-19'


# ci.F.1519 = cbind(unlist(lapply(1:length(vals.F), function(i) unlist(strsplit(vals.F[i], '[()]'))[2]) ))
# ci.F.1519 = strsplit(ci.F.1519[,1],',')
# ci.F.1519 = matrix(as.numeric(unlist(ci.F.1519)), byrow=T, ncol=2)
# ci.F.1519 = rbind(ci.F.1519,c(NA,NA))
# rownames(ci.F.1519)=rownames(prev_F_1519)
# colnames(ci.F.1519)=c('low.095_15-19','up.095_15-19')
# data1519F = cbind(prev_F_1519/100, ci.F.1519/100)
# chl.pos.F = cbind(data1519F, chl.pos.F)

save(prev_F_1519, file='gon_prev_F_1519.rda')
save(prev_M_1519, file='gon_prev_M_1519.rda')
 
 # save(ci.F.1519, file='ci.F.1519.rda')

# vals.M = cbind(unlist(lapply(prev.M.1519, as.character)))[,1]
# prev_M_1519 = as.numeric(unlist(lapply(1:length(vals.M), function(i) unlist(strsplit(vals.M[i], '[()]'))[1])))
# prev_M_1519 = c(prev_M_1519, NA)

# names(prev_M_1519)=c(substring(gsub('\\.','-',names(prev.M.1519) ),2), '2013-2014')
# names(prev_M_1519)[3:4]= substring(names(prev_M_1519)[3:4],1,nchar(names(prev_M_1519)[3])-1)
# prev_M_1519=as.data.frame(prev_M_1519)
# colnames(prev_M_1519)='Prev_15-19'


# ci.M.1519 = cbind(unlist(lapply(1:length(vals.M), function(i) unlist(strsplit(vals.M[i], '[()]'))[2]) ))
# ci.M.1519 = strsplit(ci.M.1519[,1],',')
# ci.M.1519 = matrix(as.numeric(unlist(ci.M.1519)), byrow=T, ncol=2)
# ci.M.1519 = rbind(ci.M.1519,c(NA,NA))
# rownames(ci.M.1519)=rownames(prev_M_1519)
# colnames(ci.M.1519)=c('low.095_15-19','up.095_15-19')
# data1519M = cbind(prev_M_1519/100, ci.M.1519/100)
# chl.pos.M = cbind(data1519M, chl.pos.M)

# save(prev_M_1519, file='prev_M_1519.rda')
# save(ci.M.1519, file='ci.M.1519.rda')

save(gon.neg.F, file='gon_neg_F.rda')
save(gon.pos.F, file='gon_pos_F.rda')
save(gon.neg.M, file='gon_neg_M.rda')
save(gon.pos.M, file='gon_pos_M.rda')




