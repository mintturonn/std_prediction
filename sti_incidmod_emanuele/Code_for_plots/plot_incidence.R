plot.incidence=function(g,d, seq.age, nyears.p, nyears, dur.post,pred.single.years, current.folder){
  
  ## Retrieve quantities from posterior data ----
  posts.mean.incid = list.posts$posts.mean.incid  ## mean incidence
  posts.ci.incid   = list.posts$posts.ci.incid    ## confidence intervals
  dur.chain.mat 	 = dur.post					   ## also, duration
  
  ## Incidence loop ----
  lapply(seq.age, function(ia){
    
    ## choose indexes in mean incidence and confidence intervals according to age group & gender
    index.w = grep(paste(',',ia,',',g,',',d,']', sep=''), rownames(posts.mean.incid))
    index.w.ci = grep(paste(',',ia,',',g,',',d,']', sep=''), rownames(posts.ci.incid))
    
    ## set y.max for plots as per Josh's requests
    if(g==1 & ia%in%c(1,2,3)){ y.max=0.15 } else if(g==2 & ia%in%c(1,2,3)){ y.max=0.1 }; cx.ax= 1.25; 
    seq.at=seq(from=0, to=y.max, by=0.025)  			
    
    ## Plots ----
    plot(nyears.p, nyears.p, type='n', xaxt='n', yaxt='n', ylab='Incidence', xlab='', ylim=c(0,y.max), cex.lab=1.25, cex.axis=cx.ax)
    axis(side=1, at=c(nyears.p), seq(from=1999, length.out=length(nyears.p)), cex.axis=cx.ax) 
    axis(side=2, at=seq.at, seq.at, cex.axis=cx.ax) 
    
    points(nyears, posts.mean.incid[index.w[nyears],1], col='darkblue', pch=19, cex=1)
    sapply(nyears, function(i) segments(i, posts.ci.incid[index.w.ci[i],1], i, posts.ci.incid[index.w.ci[i],5], col='darkblue'))
    
    index.years.choose=c(10,18)
    
    estimated.i = matrix(               
      round(c(posts.mean.incid[index.w[index.years.choose],1]*100, 
              posts.ci.incid[index.w[index.years.choose],1]*100, 
              posts.ci.incid[index.w[index.years.choose],5]*100),2), 
      byrow=T, ncol=2)[c(2,1,3),]
    colnames(estimated.i) =c(index.years.choose)
    
    if(g==1){ print(paste('INCIDENCE females, age group ',ia, ': ', sep='')); print(estimated.i)} else if(g==2){ 
      print(paste('INCIDENCE males, age group:',ia, sep='')); 
      print(estimated.i) }
    
    #if(g==1){ cat( paste('INCIDENCE females, age group:',ia, sep=''), round(c(posts.mean.incid[index.w[length(nyears)],1]*100, 
    #posts.ci.incid[index.w.ci[length(nyears)],1]*100, 
    #posts.ci.incid[index.w.ci[length(nyears)],5]*100),2), '\n')} else if(g==2){ 
    #cat( paste('INCIDENCE males, age group:',ia, sep=''), round(c(posts.mean.incid[index.w[length(nyears)],1]*100, 
    #posts.ci.incid[index.w.ci[length(nyears)],1]*100, 
    #posts.ci.incid[index.w.ci[length(nyears)],5]*100),2), '\n')}
    
    ## Calculation of 'prevalence/duration' ----
    ## then selects the means
    
    ## Coefficient matrix (with proper entries for age group & gender) ----
    a.chain= list.posts$a.chain.prev
    a.chain.mat = as.matrix(a.chain)
    imat=grep( paste(',',ia,',',g,',',d,']', sep=''), gsub('a','',colnames(a.chain)))
    a.chain.mat = a.chain.mat[,imat]
    a.chain.cols = grep(paste(',',ia,',', g,',',d,']', sep=''), colnames(a.chain.mat))
    
    ## Duration chain from posterior (index.w is the "right"index for current age group & gender)
    namescol=sapply(1:length(index.w), function(jj) substring(rownames(posts.mean.incid[index.w,])[jj],4 ) )
    namesdur=sapply(1:length(colnames(dur.post)), function(jj) substring(colnames(dur.post)[jj],4 ) )
    
    #cat('age.grp: ',ia,'\n')
    #cat('index.w: ', index.w, '\n')
    dur.chain.mat = dur.post[,which(namesdur%in%namescol)]
    
    ## Function for spline ----
    posterior.chain = sapply(1:nrow(a.chain.mat), function(j) {
      sapply(1:(data$n.single.years+pred.single.years), function(i) {
        exp(data$prev.spline.mat[i, ,d]%*%a.chain.mat[j,a.chain.cols] )/(1+exp(data$prev.spline.mat[i, ,d]%*%a.chain.mat[j,a.chain.cols]))})/dur.chain.mat[j,]
    })
    cis = apply(posterior.chain, 1, function(x) quantile(x, c(0.025, 0.975)))
    
    ## Adds to the plots ----
    mean.pct.incid = apply(posterior.chain,1,mean)
    lines(nyears.p, mean.pct.incid, col='darkblue', lty=2, lwd=1)
    mycol = t_col('lightblue',percent=80, name='transblue')
    polygon(c(nyears.p,rev(nyears.p)), c(cis[1,1:length(nyears.p)], rev(cis[2,1:length(nyears.p)])), col=mycol, border=NA )
    
    save(posterior.chain, file=paste('incid_post_chn_k_',ia,'_g_',g,'_d_',d,'_',casereports,'_', screening.reporting,'.rda', sep=''))
  
    ## Satterwhite data
    if(d==1){    
    ## Satterwhite data plots (function) ----
    plot.satt = function(y,g,ia){
      x = which(seq(from=1999, length.out=length(nyears.p))%in%c('2008'))
      points(x, y, pch=4, col='firebrick1', lwd=1.5);
      ## Satterwhite values for confidence intervals
      if(g==1 & ia%in%c(1,2)){y1.segm=0.0226/0.69; y2.segm=0.0452/0.69;
      legend('top', bty='n', c('Estim.2008 (ages 15-24)\n from Satterwhite et al.'), col='firebrick1', lwd=2.5, cex=1.5)}
      else if(g==1 & ia==3){y1.segm=0.004/0.79; y2.segm=0.0186/0.79; 
      legend('top', bty='n', c('Estim.2008 (ages 25-39)\n from Satterwhite et al.'), col='firebrick1', lwd=2.5, cex=1.5)}
      else if(g==2 & ia%in%c(1,2)){y1.segm=0.0107/0.41; y2.segm=0.0255/0.41; 
      legend('top', bty='n', c('Estim.2008 (ages 15-24)\n from Satterwhite et al.'), col='firebrick1', lwd=2.5, cex=1.5)}
      else if(g==2 & ia==3){y1.segm=0.0056/0.41; y2.segm=0.0181/0.41; 
      legend('topright', bty='n', c('Estim.2008 (ages 25-39)\n from Satterwhite et al.'), col='firebrick1', lwd=2.5, cex=1.5)}
      
      segments(x, y1.segm, x, y2.segm, col='firebrick1', lwd=1.5 )
      x.seq = seq(from=(x-0.25), to=(x+0.25), by=0.01)
      polygon(c(x.seq, rev(x.seq)), c(rep(y1.segm, length(x.seq)),rep(y2.segm, length(x.seq))), col=t_col('firebrick1',75,'transpfire'), border=NA) }
    ## Actually plot points ----
    if(g==1 & ia==1){ 
      plot.satt(0.0321/0.69, g, ia)} else if(g==1 & ia==2){ 
        plot.satt(0.0321/0.69, g, ia)} else if (g==1 & ia==3){ 
          plot.satt(0.0087/0.79, g, ia)} else if (g==2 & ia==1){ 
            plot.satt(0.0166/0.41, g, ia)} else if (g==2 & ia==2){ 
              plot.satt(0.0166/0.41, g, ia)} else if (g==2 & ia==3){ 
                plot.satt(0.0101/0.41, g, ia)}
    
 
    
} else if (d==2){
  
     if(g==1 & ia%in%seq.age){
        x.satt = which(seq(from=1999, length.out=length(nyears.p))%in%c('2008'))
        points(x.satt, 0.0032/0.46 , pch=4, col='firebrick1', lwd=1.5);
        segments(x.satt, 0.0016/0.46, x.satt, 0.0057/0.46, col='firebrick1', lwd=1.5 )

        if(ia==1){points(x.satt, 0.0062/0.46 , pch=4, col='firebrick3', lwd=1.5);
        segments(x.satt, 0.0038/0.46, x.satt, 0.0103/0.46, col='firebrick3', lwd=1.5 ) }

        x.satt.seq = seq(from=(x.satt-0.25), to=(x.satt+0.25), by=0.01)
        polygon(c(x.satt.seq, rev(x.satt.seq)), c(rep(0.0016/0.46, length(x.satt.seq)),rep(0.0057/0.46, length(x.satt.seq))),  col=t_col('firebrick1',75,'transpfire'), border=NA)

        if(ia==1){
        polygon(c(x.satt.seq, rev(x.satt.seq)), c(rep(0.0038/0.46, length(x.satt.seq)),rep(0.0103/0.46, length(x.satt.seq))),  col=t_col('firebrick3',75,'transpfire'), border=NA)
        legend('topright', bty='n', c('Estim.2008 (ages 15-39) from Satterwhite et al.', 'Estim.2008 (ages 15-24) from Satterwhite et al.'), col=c('firebrick1','firebrick3'), lwd=2) 
        }
      }
     if(g==2 & ia%in%seq.age){
        x.satt = which(seq(from=1999, length.out=length(nyears.p))%in%c('2008'))
        points(x.satt, 0.0021/0.23 , pch=4, col='firebrick1', lwd=1.5);
        segments(x.satt, 0.0008/0.23, x.satt, 0.0043/0.23, col='firebrick1', lwd=1.5 )

        if(ia==1){
        points(x.satt, 0.0032/0.23 , pch=4, col='firebrick3', lwd=1.5);
        segments(x.satt, 0.0012/0.23, x.satt, 0.0084/0.23, col='firebrick3', lwd=1.5 ) }

        x.satt.seq = seq(from=(x.satt-0.25), to=(x.satt+0.25), by=0.01)
        polygon(c(x.satt.seq, rev(x.satt.seq)), c(rep(0.0008/0.23, length(x.satt.seq)),rep(0.0043/0.23, length(x.satt.seq))),  col=t_col('firebrick1',75,'transpfire'), border=NA)

        if(ia==1){polygon(c(x.satt.seq, rev(x.satt.seq)), c(rep(0.0012/0.23, length(x.satt.seq)),rep(0.0084/0.23, length(x.satt.seq))),  col=t_col('firebrick3',75,'transpfire'), border=NA)
        legend('topright', bty='n', c('Estim.2008 (ages 15-39) from Satterwhite et al.', 'Estim.2008 (ages 15-24) from Satterwhite et al.'), col=c('firebrick1','firebrick3'), lwd=2) 
        }}}
  
    
    
    ## Number of incident cases ----
    if(g==1){g1=2} else if(g==2){g1=1}
    observed.pop  = data$N.by.year[,ia,g,d]*popul.factor
    projected.pop = proj.pop3[-1,c(2,ia+2),g1]
    projected.pop = projected.pop[1:pred.single.years,2]
    complete.pop = c(observed.pop, projected.pop)

    save(complete.pop, file=paste('incid_pop_k_',ia,'_g_',g,'_d_',d,'_',casereports,'_',screening.reporting, '.rda',sep='') )

    incident.cases[,ia,g,d] <<- mean.pct.incid*complete.pop 
    mean.pct.incid[,ia,g,d]<<- mean.pct.incid
       if(g==1){ 
     	cat( paste('Incident cases, year ', c(1999:2016)[10], 'FEMALES, age group',ia,':'), c( incident.cases[10,ia,g,d],cis[1,10]*complete.pop[10], cis[2,10]*complete.pop[10]) , '\n')
     	cat( paste('Incident cases, year ', c(1999:2016)[18], 'FEMALES, age group',ia,':'), c( incident.cases[length(nyears),ia,g,d],cis[1,length(nyears)]*complete.pop[length(nyears)],
     	cis[2,length(nyears)]*complete.pop[length(nyears)]) , '\n') } else if(g==2){ 	
     	cat( paste('Incident cases, year ', c(1999:2016)[10], 'MALES, age group',ia,':'), c( incident.cases[10,ia,g,d],cis[1,10]*complete.pop[10], cis[2,10]*complete.pop[10]) , '\n')
     	cat( paste('Incident cases, year ', c(1999:2016)[18], 'MALES, age group',ia,':'), c( incident.cases[length(nyears),ia,g,d], cis[1,length(nyears)]*complete.pop[length(nyears)], 
     	cis[2,length(nyears)]*complete.pop[length(nyears)]), '\n' ) }
    
    #confint.0816II[ia,]<<- c( c( cis[1,10]*complete.pop[10], cis[2,10]*complete.pop[10]), 
    #                          c( cis[1,length(nyears)]*complete.pop[length(nyears)], cis[2,length(nyears)]*complete.pop[length(nyears)]) )
  })    
  
  rownames(incident.cases)=seq(1999, length=(data$n.single.years+data$pred))
  #save(incident.cases, file=paste(current.folder, subfolder,'incident.cases','.rda', sep=''))
  write.csv(incident.cases[,,g,d], file= paste(current.folder,'/incid_cases_','g_',g,'_d_',d,'_',screening.reporting,'.csv', sep='')) #, sheetName="Incident cases")
  write.csv(mean.pct.incid[,,g,d], file= paste(current.folder,'/mean_pct_incid_','g_',g,'_d_',d,'_',screening.reporting,'.csv', sep='')) #, sheetName="Prevalence pct")
  #write.xlsx(mean.pct.incidII[,,g,d], file= paste(current.folder,'/Mean_pct_incid',g,'_',d,'.xlsx', sep=''), sheetName="Incidence pct")
  #write.xlsx(confint.0816II, file= paste(current.folder,'/confintII',g,'_',d,'.xlsx', sep=''), sheetName="confint")
  
}