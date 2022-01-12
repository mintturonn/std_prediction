plot.screening.fractiontrt = function(g,d,dsy, dsy1, dim.post, prior.matrix, index.chose, seq.age, nyears.p, nyears){

  ## Screening loop ----
lapply(seq.age, function(ia){
  cat('age.grp.scr: ',ia,"\n")
  ## Selection of posterior values from saved data for fraction treated (screening, gamma) ----
  if(data$n.chains>1){list.gamma=lapply(1:data$n.chains, function(i) posts[[i]][,grep('\\bgam\\b',colnames(posts[[i]]))]); posts.gamma=do.call(rbind, list.gamma)} else {
    posts.gamma = posts[[1]][,grep( paste('gam[',"[[:digit:]]+",',',ia,',',g,',',d,']', sep=''), colnames(posts[[1]]))]}   ##grep('gam',colnames(posts[[1]]))
  ## Selection of the right columns according to current age groups and gender ----
  #index = grep(paste(',',ia,',',g,']', sep=''), colnames(posts.gamma))
  posts.gamma.r = posts.gamma[index.chose,]
  
  ## Same for screening ----
  if(data$n.chains>1){list.screen=lapply(1:data$n.chains, function(i) posts[[i]][,grep('\\bscr\\b',colnames(posts[[i]]))]); posts.screen=do.call(rbind, list.screen)} else {
    posts.screen = posts[[1]][,grep( paste('scr[',"[[:digit:]]+",',',ia,',',g,',',d,']', sep=''), colnames(posts[[1]]))]} #posts[[1]][, grep('\\bscr\\b',colnames(posts[[1]]))]}
  #index = grep(paste(',',ia,',',g,']', sep=''), colnames(posts.bezier))
  posts.screen.r=posts.screen[index.chose,]
  
  s1=Sys.time()
  
  ## Plot fraction treated ----
  plot(nyears.p, nyears.p, type='n', xaxt='n', xlab='', ylab='Fraction treated', ylim=c(0, 1), cex.lab=1.25, cex.axis=1.25 ) #, gamma.prior.r
  axis(side=1, at=c(nyears.p), seq(from=1999, length.out=length(nyears.p)), cex.axis=1.25)
  sapply(1:nrow(posts.gamma.r), function(i) lines(nyears, posts.gamma.r[i,nyears], col='red'))
  sapply(1:nrow(posts.gamma.r), function(i) lines(length(nyears):length(nyears.p), posts.gamma.r[i,length(nyears):length(nyears.p)],
                                                  col=t_col('red', 75, 'transred')))
  
  ## Plot screening ----
  plot(nyears.p, nyears.p, type='n', xaxt='n',  ylab='P(asymptomatic case is treated)', xlab='', ylim=c(0,1.0),  cex.lab=1.25, cex.axis=1.25); 
  axis(side=1, at=c(nyears.p), seq(from=1999, length.out=length(nyears.p)), cex.axis=1.25) 
  sapply(1:nrow(posts.screen.r), function(i) lines(nyears, posts.screen.r[i,nyears], col='red'))
  sapply(1:nrow(posts.screen.r), function(i) lines(length(nyears):length(nyears.p), posts.screen.r[i,length(nyears):length(nyears.p)], lwd=0.5, col=t_col('red', 65, 'transred')))
  ## Screening priors ----
  sapply(1:howmany, function(i) lines(nyears, scr.prior[i,nyears,ia,g,d], lwd=0.25, col='grey65'))
  sapply(1:howmany, function(i) lines(length(nyears):length(nyears.p), scr.prior[i,length(nyears):length(nyears.p),ia,g,d], lwd=0.25,col='grey80'))
})

}