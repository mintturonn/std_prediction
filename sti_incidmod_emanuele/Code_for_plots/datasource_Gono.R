setwd(data.dir)
load('denoGon_M.rda'); load('denoGon_F.rda'); load('crGon_M.rda'); load('crGon_F.rda'); load('gon_pos_M.rda');
load('gon_neg_M.rda');load('gon_pos_F.rda'); load('gon_neg_F.rda'); load('gon_prev_F_1519.rda');load('gon_prev_M_1519.rda')

#setwd('/Users/mazzola/Dropbox (Personal)/BayesianModeling/')
#source('/Users/mazzola/Dropbox (Personal)/BayesianModeling/Code/NHANES_data2.R')

####################### input data & data treatment to collect inputs from data.

###### format data, choice between males and females
year 	= as.character(gon.pos.F[1:5,'year']) 
age.grp = c('20-24','25-39')   ## '25-29','30-34','35-39'
y 		= year
ag 		= age.grp  

gonhor.data.F=gon.pos.F
reported.cases.F=crGon.F
denominator.F=denoGon.F
gonhor.data.M=gon.pos.M
reported.cases.M=crGon.M 
denominator.M=denoGon.M

gon.year.F=gonhor.data.F[gonhor.data.F$year%in%y,] 
gon.data.F=gon.year.F[,c(grep(paste(ag,collapse='|'), names(gon.year.F)))]
rownames(gon.data.F)=gon.year.F[,1]
gon.year.M=gonhor.data.M[gonhor.data.M$year%in%y,] 
gon.data.M=gon.year.M[,c(grep(paste(ag,collapse='|'), names(gon.year.M)))]
rownames(gon.data.M)=gon.year.M[,1]

########################
## Case reports data		
cr.year.F=reported.cases.F[reported.cases.F$year%in%c(1999:2017) , ]  ##grep( paste(gsub('-','|',y), collapse='|'), reported.cases.F[,1] )
cr.data1.F=cbind(cr.year.F[,1], cr.year.F[,grep(paste(c('15-19','20-24','25-29','30-34','35-39') ,collapse='|'), names(cr.year.F))])
cr.data.F=cbind(cr.data1.F[,1], cr.data1.F[,grep('cases',colnames(cr.data1.F))])
cr.data.F=cr.data.F[order(cr.data.F[,1]),]
rownames(cr.data.F)=cr.data.F[,1]
cr.data.F=cr.data.F[,c(2:ncol(cr.data.F))]
cr.data.F=cbind(cr.data.F[,1:2], apply(cr.data.F[,3:5],1,sum))
colnames(cr.data.F)[3]='cases_25-39'


###
cr.year.M=reported.cases.M[reported.cases.F$year%in%c(1999:2017) , ] ## grep( paste(gsub('-','|',y), collapse='|'), reported.cases.M[,1] )
cr.data1.M=cbind(cr.year.M[,1], cr.year.M[,grep(paste(c('15-19','20-24','25-29','30-34','35-39') ,collapse='|'), names(cr.year.M))])
cr.data.M=cbind(cr.data1.M[,1], cr.data1.M[,grep('cases',colnames(cr.data1.F))])
cr.data.M=cr.data.M[order(cr.data.M[,1]),]
rownames(cr.data.M)=cr.data.M[,1]
cr.data.M=cr.data.M[,c(2:ncol(cr.data.M))]
cr.data.M=cbind(cr.data.M[,1:2], apply(cr.data.M[,3:5],1,sum))
colnames(cr.data.M)[3]='cases_25-39'

##########################
## Denominators

colnames(denominator.F)[grep('Jul',colnames(denominator.F))]=gsub('Jul_1_','',colnames(denominator.F)[grep('Jul',colnames(denominator.F))])
colnames(denominator.M)[grep('Jul',colnames(denominator.M))]=gsub('Jul_1_','',colnames(denominator.M)[grep('Jul',colnames(denominator.M))])

deno.year.F=denominator.F[ ,grep(paste(c(1999:2016) , collapse='|'), colnames(denominator.F)) ] ##paste(gsub('-','|', y)
deno.year.M=denominator.M[ ,grep( paste(c(1999:2016), collapse='|'), colnames(denominator.M)) ]  ##gsub('-','|', y)


deno.data.F=deno.year.F[grep(paste(c('15-19','20-24','25-29','30-34','35-39'),collapse='|'), rownames(deno.year.F)),]
deno.data.F=rbind(deno.data.F[1:2,], apply(deno.data.F[,3:5],1,sum))
deno.data.F=t(deno.data.F)
colnames(deno.data.F)[3]='25-39 years'

deno.data.M=deno.year.M[grep(paste(c('15-19','20-24','25-29','30-34','35-39'),collapse='|'), rownames(deno.year.M)),]
deno.data.M=rbind(deno.data.M[1:2,], apply(deno.data.M[,3:5],1,sum))
deno.data.M=t(deno.data.M)
colnames(deno.data.M)[3]='25-39 years'


####################################################################################
##print(list(chl.data, cr.data, deno.data))
####################################################################################
rownames(prev_F_1519)=rownames(prev_M_1519)=gon.year.F[,1]
gon.data.F = cbind(prev_F_1519[,-1], gon.data.F)
gon.data.M = cbind(prev_M_1519[,-1], gon.data.M)

cis.F=gon.data.F[,grep('095',colnames(gon.data.F))]
cis.M=gon.data.M[,grep('095',colnames(gon.data.M))]


s.over.rootn.F=sapply(1:(ncol(cis.F)/2), function(i) {(cis.F[,2*i]-cis.F[,(2*i-1)])/(2*qnorm(0.975,0,1))})
s.over.rootn.F[is.na(s.over.rootn.F)]=0.01
s.over.rootn.M=sapply(1:(ncol(cis.M)/2), function(i) {(cis.M[,2*i]-cis.M[,(2*i-1)])/(2*qnorm(0.975,0,1))})
s.over.rootn.M[is.na(s.over.rootn.M)]=0.02
s.over.rootn.M[s.over.rootn.M%in%c(0)]=0.001

s.over.rootn=rbind(cbind(s.over.rootn.F, sex=rep(1, nrow(s.over.rootn.F))), cbind(s.over.rootn.M, sex=rep(2, nrow(s.over.rootn.M))))

###########################################################################


##Additions
##Prevalence


## 1 females, 2 males

Prev=array(c(as.matrix(gon.data.F[,grep('Prev',colnames(gon.data.F))]), as.matrix(gon.data.M[,grep('Prev',colnames(gon.data.M))] )), dim=c(5,3,2))
##Prev[which(Prev==0)]=NA
CR=array(c(as.matrix(cr.data.F), as.matrix(cr.data.M) ), dim=c(18,4,2))
DN=array(c(as.matrix(deno.data.F), as.matrix(deno.data.M) ), dim=c(18,4,2))
SORn=array(c(as.matrix(s.over.rootn.F), as.matrix(s.over.rootn.M) ), dim=c(5,3,2))
