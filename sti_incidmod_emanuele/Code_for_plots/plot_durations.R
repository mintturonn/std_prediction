plot.durations = function(g,d, dt, du, dt.post, du.post, m, m.post, dnew, dnew.post, vs, vs.post,dur.post, prior.matrix, gd.index, 
                          seq.age, nyears.p, nyears, index.chose, current.folder){

## Duration loop ----
lapply(seq.age, function(ia){

  ## choice of posterior according to current age group and gender
  index = grep(paste(ia,',',g,',',d,']', sep=''), colnames(dur.post))
  
  ## posterior samples are chosen using "index.chose
  dur.post.r = dur.post[index.chose,index]
  
  ## Plots ----
  plot(nyears.p, nyears.p, type='n', xaxt='n',  ylab='Overall duration', xlab='', ylim=c(0,1.3),  cex.lab=1.25, cex.axis=1.25) 
  axis(side=1, at=c(nyears.p), seq(from=1999, length.out=length(nyears.p)), cex.axis=1.25) 
  sapply(1:nrow(dur.post.r), function(i) lines(nyears, dur.post.r[i,nyears], col='red'))
  sapply(1:nrow(dur.post.r), function(i) lines(length(nyears):length(nyears.p), dur.post.r[i,length(nyears):length(nyears.p)], col=t_col('red', 65, 'transred')))
})


## INDIVIDUAL DURATIONS ----

## load from saved data the posterior parameters
load(paste(current.folder,screening.reporting,'prior.matrix.rda', sep=''))

  ## plot parameters ----
cx.ax=1.25; cx.lb=1.25; red.prior.trans=t_col('red',75,'transred'); grey.strong='grey75'; grey.light='grey90'

lapply(1:5, function(plot.ind){
  if(plot.ind==1){
    ## Dur. treated symptomatic ----
    y.lab = 'dur.trt|symptom.[dt]'
    y.lim = c(0, max(dt[,g],dt.post))
    
    plot(nyears.p, nyears.p, type='n', xaxt='n', xlab=c('Year'), ylab=y.lab, ylim=y.lim, cex.axis=cx.ax,  cex.lab=cx.lb); 
    axis(side=1, at=c(nyears.p), seq(from=1999, length.out=length(nyears.p)), cex.axis=cx.ax) 
    
    sapply(1:length(index.chose), function(i) lines(nyears, rep(dt.post[index.chose[i]], each=length(nyears)), col='red'))
    sapply(1:length(index.chose), function(i) lines(length(nyears):length(nyears.p), rep(dt.post[index.chose[i]], each=(length(nyears.p)-length(nyears)+1)), col=red.prior.trans))
    sapply(1:nrow(prior.matrix[index.chose,]), function(i) lines(nyears, rep(prior.matrix[index.chose[i],'dt'], each=length(nyears) ), col=grey.strong))
    sapply(1:nrow(prior.matrix[index.chose,]), function(i) lines(length(nyears):length(nyears.p), rep(prior.matrix[index.chose[i],'dt'], each=(length(nyears.p)-length(nyears)+1) ), col=grey.light))
    
  } else if(plot.ind==2){
    ## Dur. untreated ----
    y.lab = 'dur.untrt.[du]'
    y.lim = c(0, max(du[,g],du.post))
    
    plot(nyears.p, nyears.p, type='n', xaxt='n', xlab=c('Year'), ylab=y.lab, ylim=y.lim, cex.axis=cx.ax,  cex.lab=cx.lb); 
    axis(side=1, at=c(nyears.p), seq(from=1999, length.out=length(nyears.p)), cex.axis=cx.ax) 
    
    sapply(1:length(index.chose), function(i) lines(nyears, rep(du.post[index.chose[i]], each=length(nyears)), col='red'))
    sapply(1:length(index.chose), function(i) lines(length(nyears):length(nyears.p), rep(du.post[index.chose[i]], each=(length(nyears.p)-length(nyears)+1)), col=red.prior.trans))
    sapply(1:nrow(prior.matrix[index.chose,]), function(i) lines(nyears, rep(prior.matrix[index.chose[i],'du'], each=length(nyears)), col=grey.strong))
    sapply(1:nrow(prior.matrix[index.chose,]), function(i) lines(length(nyears):length(nyears.p), rep(prior.matrix[index.chose[i],'du'], each=(length(nyears.p)-length(nyears)+1)), col=grey.light))
    
  } else if(plot.ind==3){
    ## Dur. treated asymptomatic ----
    y.lab = 'dur.trt|asymptom.[dnew]'
    y.lim = c(0, max(dnew[,g],dnew.post)) 
    
    plot(nyears.p, nyears.p, type='n', xaxt='n', xlab=c('Year'), ylab=y.lab, ylim=y.lim, cex.axis=cx.ax,  cex.lab=cx.lb); 
    axis(side=1, at=c(nyears.p), seq(from=1999, length.out=length(nyears.p)), cex.axis=cx.ax) 
    
    sapply(1:length(index.chose), function(i) lines(nyears, rep(dnew.post[index.chose[i]], each=length(nyears)), col='red'))
    sapply(1:length(index.chose), function(i) lines(length(nyears):length(nyears.p), rep(dnew.post[index.chose[i]], each=(length(nyears.p)-length(nyears)+1)), col=red.prior.trans))
    sapply(1:nrow(prior.matrix[index.chose,]), function(i) lines(nyears, rep(prior.matrix[index.chose[i],'dnew'], each=length(nyears)), col=grey.strong))
    sapply(1:nrow(prior.matrix[index.chose,]), function(i) lines(length(nyears):length(nyears.p), rep(prior.matrix[index.chose[i],'dnew'], each=(length(nyears.p)-length(nyears)+1)), col=grey.light))
    
  } else if(plot.ind==4){
    ## Prob.being symptomatic ----
    y.lab = 'Prob.symptomatic[m]'
    y.lim = c(0, max(m,m.post)) 
    
    plot(nyears.p, nyears.p, type='n', xaxt='n', xlab=c('Year'), ylab=y.lab, ylim=y.lim, cex.axis=cx.ax,  cex.lab=cx.lb); 
    axis(side=1, at=c(nyears.p), seq(from=1999, length.out=length(nyears.p)), cex.axis=1.25) 
    
    sapply(1:length(index.chose), function(i) lines(nyears, rep(m.post[index.chose[i]], each=length(nyears)), col='red'))
    sapply(1:length(index.chose), function(i) lines(length(nyears):length(nyears.p), rep(m.post[index.chose[i]], each=(length(nyears.p)-length(nyears)+1)), col=red.prior.trans))
    sapply(1:nrow(prior.matrix[index.chose,]), function(i) lines(nyears, rep(prior.matrix[index.chose[i],'m'], each=length(nyears)), col=grey.strong))
    sapply(1:nrow(prior.matrix[index.chose,]), function(i) lines(length(nyears):length(nyears.p), rep(prior.matrix[index.chose[i],'m'], each=(length(nyears.p)-length(nyears)+1)), col=grey.light))
    
  } else if(plot.ind==5){
    ## Prob.detection when symptomatic (vs)
    y.lab = 'Prob.detection when symptomatic [vs]'
    y.lim = c(0, max(vs,vs.post)) 
    
    plot(nyears.p, nyears.p, type='n', xaxt='n', xlab=c('Year'), ylab=y.lab, ylim=y.lim, cex.axis=cx.ax,  cex.lab=cx.lb); 
    axis(side=1, at=c(nyears.p), seq(from=1999, length.out=length(nyears.p)), cex.axis=cx.ax) 
    
    sapply(1:length(index.chose), function(i) lines(nyears, rep(vs.post[index.chose[i]], each=length(nyears)), col='red'))
    sapply(1:length(index.chose), function(i) lines(length(nyears):length(nyears.p), rep(vs.post[index.chose[i]], each=(length(nyears.p)-length(nyears)+1)), col=red.prior.trans))
    sapply(1:nrow(prior.matrix[index.chose,]), function(i) lines(nyears, rep(prior.matrix[index.chose[i],'vs'], each=length(nyears)), col=grey.strong))
    sapply(1:nrow(prior.matrix[index.chose,]), function(i) lines(length(nyears):length(nyears.p), rep(prior.matrix[index.chose[i],'vs'], each=(length(nyears.p)-length(nyears)+1)), col=grey.light))
  }
})

}