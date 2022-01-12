#rm(list=ls())

bcb=''

# Directories -------------------------------------------------------------
overall.dir = paste0(bcb,"/homes/mazzola/")
code.dir    = paste0(bcb,"/homes/mazzola/RCode/")
data.dir    = paste0(bcb,"/homes/mazzola/Datafiles/")

data.dir1 = paste0(data.dir,'2020-05-11','/')
setwd(data.dir1)


# Read in all data --------------------------------------------------------
load('deno_M_18.rda'); load('deno_F_18.rda'); load('cr_M_18.rda'); load('cr_F_18.rda'); 
load('chl_pos_M.rda'); load('chl_neg_M.rda');load('chl_pos_F.rda');
load('chl_neg_F.rda'); load('project.popul.array.rda'); load('project.popul.rda')


###### format data, choice between males and females
year 	= as.character(chl.pos.F[,'year']) 
age.grp = c('15-19','20-24','25-39')   ## '25-29','30-34','35-39'
y 		= year
ag 		= age.grp  

chlamydia.data.F=chl.pos.F
chlamydia.data.F.neg = chl.neg.F
reported.cases.F=cr.F
denominator.F=deno.F

chlamydia.data.M=chl.pos.M
chlamydia.data.M.neg = chl.neg.M
reported.cases.M=cr.M 
denominator.M=deno.M

chl.year.F=chlamydia.data.F[chlamydia.data.F$year%in%y,] 
chl.data.F=chl.year.F[,c(grep(paste(ag,collapse='|'), names(chl.year.F)))]
chl.year.M=chlamydia.data.M[chlamydia.data.M$year%in%y,] 
chl.data.M=chl.year.M[,c(grep(paste(ag,collapse='|'), names(chl.year.M)))]

#chl.data.F[8,c(1:3)]=c(0.025185, 0.0088154, 0.04155475)
#chl.data.M[8,c(1:3)]=c(0.024566, 0.0109916, 0.03814134)

########################
## Case reports data		

y[9]="2015-2016"
cr.year.F=reported.cases.F[ grep( paste(c(gsub('-','|',y),'2017','2018'), collapse='|'), reported.cases.F[,1] ), ]
cr.data1.F=cbind(cr.year.F[,1], cr.year.F[,grep(paste(c('15-19','20-24','25-29','30-34','35-39') ,collapse='|'), names(cr.year.F))])
cr.data.F=cbind(cr.data1.F[,1], cr.data1.F[,grep('cases',colnames(cr.data1.F))])
cr.data.F=cr.data.F[order(cr.data.F[,1]),]
rownames(cr.data.F)=cr.data.F[,1]
cr.data.F=cr.data.F[,c(2:ncol(cr.data.F))]
cr.data.F=cbind(cr.data.F[,1:2], apply(cr.data.F[,3:5],1,sum))
colnames(cr.data.F)[3]='cases_25-39'

cr.year.M=reported.cases.M[ grep( paste(c(gsub('-','|',y),'2017','2018'), collapse='|'), reported.cases.M[,1] ), ]
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

deno.year.F=denominator.F[ ,grep( paste(c(gsub('-','|', y),'2017','2018'), collapse='|'), colnames(denominator.F)) ]
deno.year.M=denominator.M[ ,grep( paste(c(gsub('-','|', y),'2017','2018'), collapse='|'), colnames(denominator.M)) ]

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

cis.F=chl.data.F[,grep('095',colnames(chl.data.F))]
cis.M=chl.data.M[,grep('095',colnames(chl.data.M))]
s.over.rootn.F=sapply(1:(ncol(cis.F)/2), function(i) {(cis.F[,2*i]-cis.F[,(2*i-1)])/(2*qnorm(0.975,0,1))})
s.over.rootn.F[is.na(s.over.rootn.F)]=0.1
s.over.rootn.M=sapply(1:(ncol(cis.M)/2), function(i) {(cis.M[,2*i]-cis.M[,(2*i-1)])/(2*qnorm(0.975,0,1))})
s.over.rootn.M[is.na(s.over.rootn.M)]=0.1

s.over.rootn=rbind(cbind(s.over.rootn.F, sex=rep(1, nrow(s.over.rootn.F))), cbind(s.over.rootn.M, sex=rep(2, nrow(s.over.rootn.M))))

############################################################################

##Additions
##Prevalence

Prev.females = as.matrix(chl.data.F[,grep('Prev',colnames(chl.data.F))])
Prev.males = as.matrix(chl.data.M[,grep('Prev',colnames(chl.data.M))])

### std.errors
SE.females = as.matrix(chl.data.F[,grep('Std.err',colnames(chl.data.F))])
SE.males = as.matrix(chl.data.M[,grep('Std.err',colnames(chl.data.M))])

## S.over.root.n
sornF = as.matrix(s.over.rootn.F)
sornM = as.matrix(s.over.rootn.M)

## case reports & denominators
caserepF = as.matrix(cr.data.F)
caserepM = as.matrix(cr.data.M)


popF = as.matrix(deno.data.F)
popM = as.matrix(deno.data.M)

## sample sizes
samplesizeF = as.matrix(chl.data.F[,grep('n.obs',colnames(chl.data.F))])
samplesizeM = as.matrix(chl.data.M[,grep('n.obs',colnames(chl.data.M))])

samplesizeF.neg =cbind( chl.data.F[,2], as.matrix(chl.neg.F[,grep('n.obs_20-24|n.obs_25-39',colnames(chl.neg.F))]) )
samplesizeM.neg =cbind( chl.data.M[,2], as.matrix(chl.neg.M[,grep('n.obs_20-24|n.obs_25-39',colnames(chl.neg.M))]) )

## 1 females, 2 males

Prev.C= array(c(Prev.females, Prev.males), dim=c(nrow(Prev.females),3,2))
SE.C = array(c(SE.females, SE.males), dim=c(nrow(Prev.females),3,2))
CR.C= array(c(caserepF, caserepM), dim=c(nrow(caserepF),3,2))
DN.C= array(c(popF, popM), dim=c(nrow(popF),3,2))
SORn.C= array(c(sornF, sornM), dim=c(nrow(sornF),3,2))
Sample.C = array(c(samplesizeF, samplesizeM), dim=c(nrow(samplesizeF), 3, 2))
Sample.C.neg = array(c(samplesizeF.neg, samplesizeM.neg), dim=c(nrow(samplesizeF), 3, 2))
#Prev.C[8,1,1] = 0.025
#Prev.C[8,1,2] = 0.024
#SORn.C[8,1,1] = 0.008258
#SORn.C[8,1,2] = 0.006848

####### Gono
load('denoGon_M.rda'); load('denoGon_F.rda'); load('crGon_M.rda'); load('crGon_F.rda'); load('gon_pos_M.rda');
load('gon_neg_M.rda');load('gon_pos_F.rda'); load('gon_neg_F.rda'); load('gon_prev_F_1519.rda');load('gon_prev_M_1519.rda')

####################### input data & data treatment to collect inputs from data.

###### format data, choice between males and females
year 	= as.character(gon.pos.F[1:5,'year']) 
age.grp = c('20-24','25-39')   ## '25-29','30-34','35-39'
y 		= year
ag 		= age.grp  

gonhor.data.F=gon.pos.F
gonhor.data.F.neg = gon.neg.F
reported.cases.F=crGon.F
denominator.F=denoGon.F
gonhor.data.M=gon.pos.M
gonhor.data.M.neg = gon.neg.M
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
cr.year.F=reported.cases.F[reported.cases.F$year%in%c(1999:2018) , ]  ##grep( paste(gsub('-','|',y), collapse='|'), reported.cases.F[,1] )
cr.data1.F=cbind(cr.year.F[,1], cr.year.F[,grep(paste(c('15-19','20-24','25-29','30-34','35-39') ,collapse='|'), names(cr.year.F))])
cr.data.F=cbind(cr.data1.F[,1], cr.data1.F[,grep('cases',colnames(cr.data1.F))])
cr.data.F=cr.data.F[order(cr.data.F[,1]),]
rownames(cr.data.F)=cr.data.F[,1]
cr.data.F=cr.data.F[,c(2:ncol(cr.data.F))]
cr.data.F=cbind(cr.data.F[,1:2], apply(cr.data.F[,3:5],1,sum))
colnames(cr.data.F)[3]='cases_25-39'


###
cr.year.M=reported.cases.M[reported.cases.F$year%in%c(1999:2018) , ] ## grep( paste(gsub('-','|',y), collapse='|'), reported.cases.M[,1] )
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

deno.year.F=denominator.F[ ,grep(paste(c(1999:2018) , collapse='|'), colnames(denominator.F)) ] ##paste(gsub('-','|', y)
deno.year.M=denominator.M[ ,grep( paste(c(1999:2018), collapse='|'), colnames(denominator.M)) ]  ##gsub('-','|', y)


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
namescis <- rownames(cis.F)

cis.F = sapply(cis.F, as.numeric)
cis.M = sapply(cis.M, as.numeric)
rownames(cis.F) <- rownames(cis.M) <- namescis

s.over.rootn.F=sapply(1:(ncol(cis.F)/2), function(i) { (cis.F[,2*i]-cis.F[,(2*i-1)])/(2*qnorm(0.975,0,1))})
s.over.rootn.F[is.na(s.over.rootn.F)]=0.01
s.over.rootn.M=sapply(1:(ncol(cis.M)/2), function(i) {(cis.M[,2*i]-cis.M[,(2*i-1)])/(2*qnorm(0.975,0,1))})
s.over.rootn.M[is.na(s.over.rootn.M)]=0.02
s.over.rootn.M[s.over.rootn.M%in%c(0)]=0.001

s.over.rootn=rbind(cbind(s.over.rootn.F, sex=rep(1, nrow(s.over.rootn.F))), cbind(s.over.rootn.M, sex=rep(2, nrow(s.over.rootn.M))))

###########################################################################

##Additions
##Prevalence
## 1 females, 2 males

Prev.G=array(c(as.matrix(gon.data.F[,grep('Prev',colnames(gon.data.F))]), as.matrix(gon.data.M[,grep('Prev',colnames(gon.data.M))] )), dim=c(5,3,2))
SE.G = array(c(as.matrix(gon.data.F[,grep('Std.err',colnames(gon.data.F))]), as.matrix(gon.data.M[,grep('Std.err',colnames(gon.data.M))] )), dim=c(5,3,2))
#Prev[which(Prev==0)]=NA
CR.G=array(c(as.matrix(cr.data.F), as.matrix(cr.data.M) ), dim=c(nrow(cr.data.F),3,2))
DN.G=array(c(as.matrix(deno.data.F), as.matrix(deno.data.M) ), dim=c(nrow(deno.data.F),3,2))
SORn.G=array(c(as.matrix(s.over.rootn.F), as.matrix(s.over.rootn.M) ), dim=c(5,3,2))
Sample.G = array(c(as.matrix(gon.data.F[,grep('n.obs|n.with.gono',colnames(gon.data.F))]), as.matrix(gon.data.M[,grep('n.obs|n.with.gono',colnames(gon.data.M))] )), dim=c(5,3,2))

Sample.G.neg = array(c(
    cbind( gon.data.F[,2],as.matrix(gonhor.data.F.neg[1:5,grep('n.obs_20-24|n.obs_25-39|n.with.gono',colnames(gonhor.data.F.neg))])) , 
    cbind( gon.data.M[,2],as.matrix(gonhor.data.M.neg[1:5,grep('n.obs_20-24|n.obs_25-39|n.with.gono',colnames(gonhor.data.M.neg))]))
    ), dim=c(5,3,2))



# Array preparation for rjags input ---------------------------------------
# prevalence
Prev = array(c( matrix(rep(NA, nrow(Prev.C)*ncol(Prev.C)), byrow=T, ncol=ncol(Prev.C)),
       matrix(rep(NA, nrow(Prev.C)*ncol(Prev.C)), byrow=T, ncol=ncol(Prev.C)),
       matrix(rep(NA, nrow(Prev.C)*ncol(Prev.C)), byrow=T, ncol=ncol(Prev.C)),
       matrix(rep(NA, nrow(Prev.C)*ncol(Prev.C)), byrow=T, ncol=ncol(Prev.C))),
       dim = c(nrow(Prev.C), ncol(Prev.C), 2, 2))
# case reports/denominators
CR = DN = array(c( matrix(rep(NA, nrow(CR.C)*ncol(CR.C)), byrow=T, ncol=ncol(CR.C)),
    matrix(rep(NA, nrow(CR.C)*ncol(CR.C)), byrow=T, ncol=ncol(CR.C)),
    matrix(rep(NA, nrow(CR.C)*ncol(CR.C)), byrow=T, ncol=ncol(CR.C)),
    matrix(rep(NA, nrow(CR.C)*ncol(CR.C)), byrow=T, ncol=ncol(CR.C))),
    dim = c(nrow(CR.C), ncol(CR.C), 2, 2))
# s over sqrt(n)
SORn = array(c( matrix(rep(NA, nrow(SORn.C)*ncol(SORn.C)), byrow=T, ncol=ncol(SORn.C)),
    matrix(rep(NA, nrow(SORn.C)*ncol(SORn.C)), byrow=T, ncol=ncol(SORn.C)),
    matrix(rep(NA, nrow(SORn.C)*ncol(SORn.C)), byrow=T, ncol=ncol(SORn.C)),
    matrix(rep(NA, nrow(SORn.C)*ncol(SORn.C)), byrow=T, ncol=ncol(SORn.C))),
    dim = c(nrow(SORn.C), ncol(SORn.C), 2, 2))

## std.errors
SE = array(c( matrix(rep(NA, nrow(SE.C)*ncol(SE.C)), byrow=T, ncol=ncol(SE.C)),
    matrix(rep(NA, nrow(SE.C)*ncol(SE.C)), byrow=T, ncol=ncol(SE.C)),
    matrix(rep(NA, nrow(SE.C)*ncol(SE.C)), byrow=T, ncol=ncol(SE.C)),
    matrix(rep(NA, nrow(SE.C)*ncol(SE.C)), byrow=T, ncol=ncol(SE.C))),
    dim = c(nrow(SE.C), ncol(SE.C), 2, 2))

SampleSizes = array(c( matrix(rep(NA, nrow(Sample.C)*ncol(Sample.C)), byrow=T, ncol=ncol(Sample.C)),
    matrix(rep(NA, nrow(Sample.C)*ncol(Sample.C)), byrow=T, ncol=ncol(Sample.C)),
    matrix(rep(NA, nrow(Sample.C)*ncol(Sample.C)), byrow=T, ncol=ncol(Sample.C)),
    matrix(rep(NA, nrow(Sample.C)*ncol(Sample.C)), byrow=T, ncol=ncol(Sample.C))),
    dim = c(nrow(Sample.C), ncol(Sample.C), 2, 2))

SampleSizes.neg = array(c( matrix(rep(NA, nrow(Sample.C)*ncol(Sample.C)), byrow=T, ncol=ncol(Sample.C)),
    matrix(rep(NA, nrow(Sample.C)*ncol(Sample.C)), byrow=T, ncol=ncol(Sample.C)),
    matrix(rep(NA, nrow(Sample.C)*ncol(Sample.C)), byrow=T, ncol=ncol(Sample.C)),
    matrix(rep(NA, nrow(Sample.C)*ncol(Sample.C)), byrow=T, ncol=ncol(Sample.C))),
    dim = c(nrow(Sample.C), ncol(Sample.C), 2, 2))


## Combination ----
for(d in 1:2){ if(d==1){
  for(y in 1:nrow(Prev.C)){ Prev[y,,,d] = Prev.C[y,,]}
  for(y in 1:nrow(CR.C)){ CR[y,,,d]   = CR.C[y,,]}
  for(y in 1:nrow(DN.C)){ DN[y,,,d]   = DN.C[y,,]}
  for(y in 1:nrow(SORn.C)){ SORn[y,,,d] = SORn.C[y,,] }
  for(y in 1:nrow(SE.C)){SE[y,,,d] = SE.C[y,,] }
  for(y in 1:nrow(Sample.C)){SampleSizes[y,,,d]=Sample.C[y,,]}
  for(y in 1:nrow(Sample.C.neg)){SampleSizes.neg[y,,,d]=Sample.C.neg[y,,]}} else if(d==2){ 
  for(y in 1:nrow(Prev.G)){ Prev[y,,,d] =Prev.G[y,,]}
  for(y in 1:nrow(CR.G)){CR[y,,,d]   = CR.G[y,,]}
  for(y in 1:nrow(DN.G)){   DN[y,,,d]   = DN.G[y,,]}
  for(y in 1:nrow(SORn.G)){   SORn[y,,,d] = SORn.G[y,,] } 
  for(y in 1:nrow(SE.G)){SE[y,,,d] = SE.G[y,,]}
  for(y in 1:nrow(Sample.G)){SampleSizes[y,,,d]=Sample.G[y,,]}
  for(y in 1:nrow(Sample.G.neg)){SampleSizes.neg[y,,,d]=Sample.G.neg[y,,]}}
}

SORn[is.na(SORn)]=0.1

#####################################################  additions from new Meghan's file
library(openxlsx)
add.data.Chl = read.xlsx("New RDC Results 6.29.18.xlsx", sheet=1, rows=c(78:114), cols=c(1:7))
add.data.Gon = read.xlsx("New RDC Results 6.29.18.xlsx", sheet=2, rows=c(42:60), cols=c(1:7))
save(add.data.Chl, file='add_data_Chl.rda')
save(add.data.Gon, file='add_data_Gon.rda')
load('add_data_Chl.rda'); load("add_data_Gon.rda")


#Prev[,1,1,1]=as.numeric(as.character(add.data.Chl[2:10,4]))
#Prev[,1,2,1]=as.numeric(as.character(add.data.Chl[20:28,4]))
Prev[1:5,1,1,2] = as.numeric(as.character(add.data.Gon[2:6,4]))
Prev[1:3,1,2,2] = as.numeric(as.character(add.data.Gon[11:13,4]))

#SORn[,1,1,1]=(as.numeric(as.character(add.data.Chl[2:10,7]))-as.numeric(as.character(add.data.Chl[2:10,6])))/(2*qnorm(0.975,0,1))
#SORn[,1,2,1]=(as.numeric(as.character(add.data.Chl[20:28,7]))-as.numeric(as.character(add.data.Chl[20:28,6])))/(2*qnorm(0.975,0,1))
SORn[1:5,1,1,2]=(as.numeric(as.character(add.data.Gon[2:6,7]))-as.numeric(as.character(add.data.Gon[2:6,6])))/(2*qnorm(0.975,0,1))
SORn[1:3,1,2,2]=(as.numeric(as.character(add.data.Gon[11:13,7]))-as.numeric(as.character(add.data.Gon[11:13,6])))/(2*qnorm(0.975,0,1))



