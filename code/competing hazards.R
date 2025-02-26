
library(rriskDistributions)

betavars <- function(mu1, var1){
  alpha1 = ((1 - mu1) / var1 - 1 / mu1) * mu1 ^ 2;
  beta1 = alpha1 * (1 / mu1 - 1);
  
  return(list(alpha=alpha1, beta=beta1))
}

## data
read_excel(here("data/nhanes_nsfg.xlsx"), sheet = "nhanes") %>%
  filter(age != "all" & year > 2011 ) -> nhnsd

read_excel(here("data/nhanes_nsfg.xlsx"), sheet = "nsfg") %>%
  filter(age != "all" & year > 2011 & year < 2018) -> nsfgd

read_excel(here("data/nhanes_nsfg.xlsx"), sheet = "params") -> durpars

###############################################################################
### Natural clearance rate distribution
p <- c(0.25, 0.50, 0.75) # percentiles given in kreisel 2021

# WOMEN natural clearance; duration given in days, change to monthly rate
q <- c( 1/ ( durpars$p75[durpars$Param=="clear.f"]/30.437 ),
        1/ ( durpars$Median[durpars$Param=="clear.f"]/30.437),
        1/ ( durpars$p25[durpars$Param=="clear.f"]/30.437 ))

# fit.clear.f <- rriskFitdist.perc(p, q, show.output = FALSE)

fit.clear.f <- get.beta.par(p, q, show.output = FALSE, plot = TRUE, tol = 0.00001)

quantile(rbeta(100000, fit.clear.f[1], fit.clear.f[2]), probs = c(0.25, 0.5, 0.75))
q
fit.clear.f

# MEN natural clearnace duration given in days, change to monthly rate
q <- c( 1/ ( durpars$p75[durpars$Param=="clear.m"]/30.437 ),
        1/ ( durpars$Median[durpars$Param=="clear.m"]/30.437),
        1/ ( durpars$p25[durpars$Param=="clear.m"]/30.437 ))

fit.clear.m <- get.beta.par(p, q, show.output = FALSE, plot = TRUE, tol = 0.001)
quantile(rbeta(100000, fit.clear.m[1], fit.clear.m[2]), probs = c(0.25, 0.5, 0.75))
q
fit.clear.m

##### TESTING RATE TO DISTRIBUTION
Te <- matrix(nsfgd$t[nsfgd$gender=="female"], 2) 
TN <- matrix(nsfgd$t_N[nsfgd$gender=="female"], 2)
Tse <- sqrt(ct_data2$Tp*(1-ct_data2$Tp)/ct_data2$TN)

Tm <- matrix(nsfgd$t[nsfgd$gender=="male"], 2) 
TmN <- matrix(nsfgd$t_N[nsfgd$gender=="male"], 2)
Tmse <- sqrt(ct_data2$Tmp*(1-ct_data2$Tmp)/ct_data2$TNm)

tCI <- Te /(TN)
tmCI <- Tm /(TmN)

# turned to a monthly rate
trate <- -log(1-tCI)/12
tmrate <- -log(1-tmCI)/12

trate_se <- trate * Tse/tCI
tmrate_se <- tmrate * Tmse/tmCI

test.dist.shape <- betavars(trate, trate_se^2)
testm.dist.shape <- betavars(tmrate, tmrate_se^2)

####### Average estimate for total duration -->  going into P = incid * duration

# WOMEN prop symptomatic
q <- c( durpars$p25[durpars$Param=="psympt.f"],
        durpars$Median[durpars$Param=="psympt.f"],
        durpars$p75[durpars$Param=="psympt.f"] )


fit.psympt.f <- get.beta.par(p, q, show.output = FALSE, plot = TRUE, tol = 0.00001)

quantile(rbeta(100000, fit.psympt.f[1], fit.psympt.f[2]), probs = c(0.25, 0.5, 0.75))
q
fit.psympt.f

# MEN prop symptomatic
q <- c( durpars$p25[durpars$Param=="psympt.m"],
        durpars$Median[durpars$Param=="psympt.m"],
        durpars$p75[durpars$Param=="psympt.m"] )


fit.psympt.m <- get.beta.par(p, q, show.output = FALSE, plot = TRUE, tol = 0.00001)

quantile(rbeta(100000, fit.psympt.m[1], fit.psympt.m[2]), probs = c(0.25, 0.5, 0.75))
q
fit.psympt.m


# WOMEN rate of screening
q <- c( durpars$p25[durpars$Param=="screen.f1"],
        durpars$Median[durpars$Param=="screen.f1"],
        durpars$p75[durpars$Param=="screen.f1"] )

fit.screen.f1 <- get.beta.par(p, q, show.output = FALSE, plot = TRUE, tol = 0.01)

quantile(rbeta(100000, fit.screen.f1[1], fit.screen.f1[2]), probs = c(0.25, 0.5, 0.75))
q
fit.screen.f1

q <- c( durpars$p25[durpars$Param=="screen.f2"],
        durpars$Median[durpars$Param=="screen.f2"],
        durpars$p75[durpars$Param=="screen.f2"] )

fit.screen.f2 <- get.beta.par(p, q, show.output = FALSE, plot = TRUE, tol = 0.1)

quantile(rbeta(100000, fit.screen.f2[1], fit.screen.f2[2]), probs = c(0.25, 0.5, 0.75))
q
fit.screen.f2

# MEN rate of screening

q <- c( durpars$p25[durpars$Param=="screen.m1"],
        durpars$Median[durpars$Param=="screen.m1"],
        durpars$p75[durpars$Param=="screen.m1"] )

fit.screen.m1 <- get.beta.par(p, q, show.output = FALSE, plot = TRUE, tol = 0.01)

quantile(rbeta(100000, fit.screen.m1[1], fit.screen.m1[2]), probs = c(0.25, 0.5, 0.75))
q
fit.screen.m1

q <- c( durpars$p25[durpars$Param=="screen.m2"],
        durpars$Median[durpars$Param=="screen.m2"],
        durpars$p75[durpars$Param=="screen.m2"] )

fit.screen.m2 <- get.beta.par(p, q, show.output = FALSE, plot = TRUE, tol = 0.1)

quantile(rbeta(100000, fit.screen.m2[1], fit.screen.m2[2]), probs = c(0.25, 0.5, 0.75))
q
fit.screen.m2

# WOMEN duration with symptomatic infection, until test seeking
# change to monthly rate

q <- c(  1/ ( durpars$p75[durpars$Param=="dursympt.f"]/30.437 ),
         1/ ( durpars$Median[durpars$Param=="dursympt.f"]/30.437),
         1/ ( durpars$p25[durpars$Param=="dursympt.f"]/30.437 ))

# rriskFitdist.perc(p, q, show.output = FALSE)

fit.symptclear.f <- get.lnorm.par(p, q, show.output = FALSE, plot = TRUE, tol = 0.01)

quantile(rlnorm(100000, fit.symptclear.f[1], fit.symptclear.f[2]), probs = c(0.25, 0.5, 0.75))
q
fit.symptclear.f

# MEN duration with symptomatic infection, until test seeking

q <- c(  1/ ( durpars$p75[durpars$Param=="dursympt.m"]/30.437 ),
         1/ ( durpars$Median[durpars$Param=="dursympt.m"]/30.437),
         1/ ( durpars$p25[durpars$Param=="dursympt.m"]/30.437 ))

fit.symptclear.m <- get.lnorm.par(p, q, show.output = FALSE, plot = TRUE, tol = 0.01)

quantile(rlnorm(100000, fit.symptclear.m[1], fit.symptclear.m[2]), probs = c(0.25, 0.5, 0.75))
q
fit.symptclear.m



################################################################################
####### SAMPLE FROM DISTRIBUTIONS --> ratios going into D = incid * prop_tested

sn <- 100000

# women
wom_ratio <- NULL

for (i in 1:2){
  for (j in 1:3){
   # print(paste(i, "i", j, "j"))
    wom_ratio <- cbind(wom_ratio, rbeta(sn, test.dist.shape$alpha[i,j], test.dist.shape$beta[i,j])/
      (rbeta(sn, test.dist.shape$alpha[i,j], test.dist.shape$beta[i,j]) + rbeta(sn, fit.clear.f[1], fit.clear.f[2])))
  }
}


quants <- c(0.001, 0.025, 0.1, 0.25,0.50,0.75,0.90,0.975, 0.999)
womquant <- apply( wom_ratio , 2 , quantile , probs = quants , na.rm = TRUE )
fit.ratio <- NULL

for (i in 1:length(womquant[1,])){
   fit.ratio <- rbind(fit.ratio, get.beta.par(quants, womquant[,i], show.output = FALSE, plot = TRUE, tol = 0.0001))
}

priors.fratio <- as.data.frame(fit.ratio)

# men
men_ratio <- NULL

for (i in 1:2){
  for (j in 1:3){
   # print(paste(i, "i", j, "j"))
    men_ratio <- cbind(men_ratio, rbeta(sn, testm.dist.shape$alpha[i,j], testm.dist.shape$beta[i,j])/
                         (rbeta(sn, testm.dist.shape$alpha[i,j], testm.dist.shape$beta[i,j]) + rbeta(sn, fit.clear.m[1], fit.clear.m[2])))
  }
}

menquant <- apply( men_ratio , 2 , quantile , probs = quants , na.rm = TRUE )
fit.ratio.m <- NULL

for (i in 1:length(menquant[1,])){
  fit.ratio.m <- rbind(fit.ratio.m, get.beta.par(quants, menquant[,i], show.output = FALSE, plot = TRUE, tol = 0.0001))
}

priors.mratio <- as.data.frame(fit.ratio.m)

# GOING INTO  Prevalence = incidence * average duration
# women and men, average duration of infection

spf <-rbeta(sn, fit.psympt.f[1], fit.psympt.f[2])
spm <-rbeta(sn, fit.psympt.m[1], fit.psympt.m[2])

avrate0 <- data.frame(f1 =  ((spf * rlnorm(sn, fit.symptclear.f[1], fit.symptclear.f[2]) + (1-spf) * rbeta(sn, fit.screen.f1[1],  fit.screen.f1[2])) + rbeta(sn, fit.clear.f[1],  fit.clear.f[2])),  
                      f2 =  ((spf * rlnorm(sn, fit.symptclear.f[1], fit.symptclear.f[2]) + (1-spf) * rbeta(sn, fit.screen.f2[1],  fit.screen.f2[2])) + rbeta(sn, fit.clear.f[1],  fit.clear.f[2])), 
                      m1 =  ((spm * rlnorm(sn, fit.symptclear.m[1], fit.symptclear.m[2]) + (1-spm) * rbeta(sn, fit.screen.m1[1],  fit.screen.m1[2])) + rbeta(sn, fit.clear.m[1],  fit.clear.m[2])), 
                      m2 =  ((spm * rlnorm(sn, fit.symptclear.m[1], fit.symptclear.m[2]) + (1-spm) * rbeta(sn, fit.screen.m2[1],  fit.screen.m2[2])) + rbeta(sn, fit.clear.m[1],  fit.clear.m[2])))


avrateuants <- apply( avrate0 , 2 , quantile , probs = quants , na.rm = TRUE )

avdur <- data.frame(f1 = 1/(12*((spf * rlnorm(sn, fit.symptclear.f[1], fit.symptclear.f[2]) + (1-spf) * rbeta(sn, fit.screen.f1[1],  fit.screen.f1[2])) + rbeta(sn, fit.clear.f[1],  fit.clear.f[2]))),  
                    f2 = 1/(12*((spf * rlnorm(sn, fit.symptclear.f[1], fit.symptclear.f[2]) + (1-spf) * rbeta(sn, fit.screen.f2[1],  fit.screen.f2[2])) + rbeta(sn, fit.clear.f[1],  fit.clear.f[2]))), 
                    m1 = 1/(12*((spm * rlnorm(sn, fit.symptclear.m[1], fit.symptclear.m[2]) + (1-spm) * rbeta(sn, fit.screen.m1[1],  fit.screen.m1[2])) + rbeta(sn, fit.clear.m[1],  fit.clear.m[2]))), 
                    m2 = 1/(12*((spm * rlnorm(sn, fit.symptclear.m[1], fit.symptclear.m[2]) + (1-spm) * rbeta(sn, fit.screen.m2[1],  fit.screen.m2[2])) + rbeta(sn, fit.clear.m[1],  fit.clear.m[2]))))

avdurquants <- apply( avdur , 2 , quantile , probs = quants , na.rm = TRUE )
# 
# rriskFitdist.perc(quants, avdurquants[,1], show.output = FALSE)

fit.avdur <- NULL
for (i in 1:length(avdurquants[1,])){
  fit.avdur <- rbind(fit.avdur, get.weibull.par(quants, avdurquants[,i], show.output = FALSE, plot = TRUE, tol = 0.0001))
}

priors.avdur <- as.data.frame(fit.avdur)

priors.avdur$shape2 <- priors.avdur$shape/2


###########################################################################

wom_ratio %>%
  melt() %>%
  ggplot(aes(x=value)) +
  geom_density(alpha=0.4, position='identity',  fill = "maroon") +
  facet_wrap(~Var2, nrow = 2) +
  xlab("years") + ylab("") + ggtitle("Women: probability incident infection is tested") +
  theme_minimal() + xlim(c(0, 0.4)) + theme(legend.position = "none")

men_ratio %>%
  melt() %>%
  ggplot(aes(x=value)) +
  geom_density(alpha=0.4, position='identity',  fill = "steelblue3") +
  facet_wrap(~Var2, nrow = 2) +
  xlab("years") + ylab("") + ggtitle("Men: probability incident infection is tested") +
  theme_minimal() + xlim(c(0, 0.4)) + theme(legend.position = "none")
  
# figures 
avdur %>%
  melt() %>%
  ggplot(aes(x=value, fill=variable, color=variable)) +
  geom_histogram(alpha=0.4, position='identity', bins = 100) +
  xlab("") + ylab("") + ggtitle("average duration") +
  theme_minimal() + xlim(c(0, 0.6))
  
avrate0 %>%
  melt() %>%
  ggplot(aes(x=1/(12*value), fill=variable, color=variable)) +
  geom_histogram(alpha=0.4, position='identity', bins = 100) +
  xlab("") + ylab("") + ggtitle("average rate") +
  theme_minimal() + xlim(c(0, 1))

hist(1/12/(spf * rlnorm(sn, fit.symptclear.f[1], fit.symptclear.f[2]) + (1-spf) * rbeta(sn, fit.screen.f1[1],  fit.screen.f1[2])), breaks=100, border=F, col='red', main='WOMEN, Duration of treated infection', xlab='years', xlim=c(0, 1.5))
hist(1/12/(spf * rlnorm(sn, fit.symptclear.f[1], fit.symptclear.f[2]) + (1-spf) * rbeta(sn, fit.screen.f2[1],  fit.screen.f2[2])), breaks=100, border=F, col='pink', add=TRUE)

hist(1/12/(spm * rlnorm(sn, fit.symptclear.m[1], fit.symptclear.m[2]) + (1-spm) * rbeta(sn, fit.screen.m1[1],  fit.screen.m1[2])), breaks=100, border=F, col='blue', main='MEN, Duration of treated infection', xlab='years', xlim=c(0, 1.5))
hist(1/12/(spm * rlnorm(sn, fit.symptclear.m[1], fit.symptclear.m[2]) + (1-spm) * rbeta(sn, fit.screen.m2[1],  fit.screen.m2[2])), breaks=100, border=F, col='steelblue2', add=TRUE)

hist(1/12/rbeta(sn, fit.clear.f[1],  fit.clear.f[2]), col='red', main='Duration of natural infection', xlab='years', xlim=c(0, 1.5))
hist(1/12/rbeta(sn, fit.clear.m[1],  fit.clear.m[2]), col='blue', add=TRUE)


hist(rweibull(sn, priors.avdur[1,1]*0.5, priors.avdur[1,2]), breaks = 100, border=F, col='steelblue4', xlim=c(0, 0.7))
hist(rweibull(sn, priors.avdur[2,1]*0.5, priors.avdur[2,2]), breaks = 100, add=TRUE, border=F, col='steelblue3', xlim=c(0, 0.7))
hist(rweibull(sn, priors.avdur[3,1]*0.5, priors.avdur[3,2]), breaks = 100, add=TRUE, border=F, col='steelblue2', xlim=c(0, 0.7))
hist(rweibull(sn, priors.avdur[4,1]*0.5, priors.avdur[4,2]), breaks = 100, add=TRUE, border=F, col='steelblue1', xlim=c(0, 0.7))


