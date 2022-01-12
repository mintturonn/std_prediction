rm(list=ls())

library(openxlsx)

bcb=''
overall.dir = paste0(bcb,"/homes/mazzola/")
code.dir    = paste0(bcb,"/homes/mazzola/RCode/")
data.dir    = paste0(bcb,"/homes/mazzola/Datafiles/")


data.dir1 = paste0(data.dir,'2020-05-11','/')
dir.create(file.path(data.dir1))
setwd(data.dir1)

pop.data = read.csv(file='NP2014_D1_Tab1Nick.csv')

pop.data.select = pop.data[pop.data$'origin'==0 & pop.data$'race'==0, c('sex','year', paste('pop',15:39, sep='_'))]
pop.data.select = pop.data.select[pop.data.select$sex%in%c(1,2),]

pop.data.select$sex = ifelse(pop.data.select$sex==1,'M', 'F')
pop.data.select = pop.data.select[pop.data.select$year%in%c(2017:2020),]

## 1 = males; 2=females

ind.1519 = grep(paste0( paste('pop',15:19, sep='_'), collapse='|' ), colnames(pop.data.select))
ind.2024 = grep(paste0( paste('pop',20:24, sep='_'), collapse='|' ), colnames(pop.data.select))
ind.2539 = grep(paste0( paste('pop',25:39, sep='_'), collapse='|' ), colnames(pop.data.select))

pop.data.select$`1519` = apply(pop.data.select[,ind.1519],1,sum)
pop.data.select$`2024` = apply(pop.data.select[,ind.2024],1,sum)
pop.data.select$`2539` = apply(pop.data.select[,ind.2539],1,sum)

pop.data.select = pop.data.select[,c('sex','year','1519','2024','2539')]
