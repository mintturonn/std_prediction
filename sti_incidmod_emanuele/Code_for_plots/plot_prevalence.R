plot.prevalence = function(g,d, seq.age, pred.single.years, cut, nyears.p, nyears, nyears.fillin, current.folder){
  
  # Retrieve posterior values ----
  posts.median.j  = list.posts$posts.median.prev.j 
  posts.mean.j    = list.posts$posts.mean.prev.j 
  posts.ci.j      = list.posts$posts.ci.prev.j         
  posts.median.w  = list.posts$posts.median.prev.w 
  posts.mean.w    = list.posts$posts.mean.prev.w    
  posts.ci.w      = list.posts$posts.ci.prev.w              
  
  # Prevalence loop (on each age group) ----
  lapply(seq.age, function(ia){
    
    # select posteriors from saved values (for single years -w- for combined years-j- and confidence intervals for single years) ----
    index.w = grep(paste(',',ia,',',g,',',d,']', sep=''), rownames(posts.mean.w))
    index.j = grep(paste(',',ia,',',g,',',d,']', sep=''), rownames(posts.mean.j))
    index.w.ci = grep(paste(',',ia,',',g,',',d,']', sep=''), rownames(posts.ci.w))
    
    
    cat('ag:', ag, '\n')
    ## prevalence data, chosen by gender ----
    prev = data$P.hat[,,g,d][,ia] 
    cint = paste('095_',ag[ia], sep='')
    
    ## choice of max for graphics ----
    if(d==1){
    if(g==1){cint = chl.data.F[,grep(cint, colnames(chl.data.F))]
      y.max=0.15 } else if(g==2){
      cint = chl.data.M[,grep(cint, colnames(chl.data.M))]
      y.max=0.1 }
    } else if(d==2){
      if(g==1){
        ###### Females
        prev = data$P.hat[,,g,d][,ia]
        cint = paste('095_',ag[ia], sep='')
        cint = gon.data.F[,grep(cint, colnames(gon.data.F))]
        y.max = 0.06 } else if(g==2){
        ###### Males
        prev = data$P.hat[,,g,d][,ia]
        cint = paste('095_',ag[ia], sep='')
        if(ia%in%c(1:3)){cint = gon.data.M[,grep(cint, colnames(gon.data.M))]}
        y.max = 0.075}
    }
   
    ## Main plots ----
    ## parameters ----
    cx.lb=1.5; cx.ax=1.25; seq.at = seq(from=0, to=y.max, by=0.025)
    plot(nyears.p, nyears.p, type='n', xaxt='n', yaxt='n', ylab='Prevalence', xlab='', ylim=c(0,y.max), cex.lab=cx.lb, cex.axis=cx.ax, main=paste('Ages ', ag[ia] ,sep=''), cex.main=2) 
    axis(side=1, at=c(nyears.p), seq(from=1999, length.out=length(nyears.p)), cex.axis=cx.ax) 
    axis(side=2, at=seq.at, seq.at, cex.axis=cx.ax) 
    ####
    
    ## Plot posterior prevalence ----
    points(nyears, posts.mean.w[index.w[nyears],1], col='darkgreen', pch=19, cex=1)
    points(seq(from=1, to=18, by=2)+0.5, posts.mean.j[index.j,1], col='forestgreen', pch=5, cex=1)
    sapply(nyears, function(i) segments(i, posts.ci.w[index.w.ci[i],1], i, posts.ci.w[index.w.ci[i],5], col='darkgreen'))
    
    index.years.choose=c(10,18)
    
    estimated = matrix(               
      round(c(posts.mean.w[index.w[index.years.choose],1]*100, 
      posts.ci.w[index.w[index.years.choose],1]*100, 
      posts.ci.w[index.w[index.years.choose],5]*100),2), 
    byrow=T, ncol=2)[c(2,1,3),]
    
    colnames(estimated) =c(index.years.choose)
    ## index.w[length(nyears)]; index.w.ci[length(nyears)]  paste('PREVALENCE females, age group ',ia, ': ', sep='') ,
    if(g==1){ print(paste('PREVALENCE females, age group ',ia, ': ', sep='')); print(estimated)} else if(g==2){ 
      print(paste('PREVALENCE males, age group:',ia, sep='')); 
    #round(c(posts.mean.w[index.years.choose,1]*100, 
    #posts.ci.w[index.years.choose,1]*100, 
    #posts.ci.w[index.years.choose,5]*100),2), '\n')
      print(estimated) }
    
    ## Coefficient matrix from saved data ----
    a.chain= list.posts$a.chain.prev
    a.chain.mat = as.matrix(a.chain)
    imat=grep( paste(',',ia,',',g,',',d,']', sep=''), gsub('a','',colnames(a.chain)))
    a.chain.mat = a.chain.mat[,imat]
    a.chain.cols = grep(paste(',',ia,',', g,',',d,']', sep=''), colnames(a.chain.mat))
    
    ## Smooth version (spline) ----
    posterior.chain = sapply(1:nrow(a.chain.mat), function(j) {
      sapply(1:(data$n.single.years+pred.single.years), function(i) {
        exp(data$prev.spline.mat[i, ,d]%*%a.chain.mat[j,a.chain.cols] )/(1+exp(data$prev.spline.mat[i, ,d]%*%a.chain.mat[j,a.chain.cols] ))
      }) })
    cis  = apply(posterior.chain, 1, function(x) quantile(x, c(0.025, 0.975)))
    
    ## Curve plots added ----
    mycol = t_col('lightgreen',percent=80, name='transgreen')
    mean.pct.prev = apply(posterior.chain,1,mean)
    
    lines(nyears.p, mean.pct.prev, col='darkgreen', lty=2, lwd=1)
    polygon(c(nyears.p,rev(nyears.p)), c(cis[1,1:length(nyears.p)], rev(cis[2,1:length(nyears.p)])), col=mycol, border=NA )

    save(posterior.chain, file=paste('prev_post_chn_k_',ia,'_g_',g,'_d_',d,'_',casereports,'_', screening.reporting,'.rda', sep=''))
    
if(d==1){    
    ## Data plots ----
    points(seq(from=1, to=18, by=2)+0.5, prev[1:9], pch=19, col='darkred')
    if(data$n.single.years>17){lll = length(seq(from=17, to=data$n.single.years, by=2)+0.5)
    points(seq(from=17, to=data$n.single.years, by=2)+0.5, prev[9:11][1:lll], pch=19, col='darkred', cex=1.25)}
    
    cat('cint1', cint[,1], '\n')
    cat('cint1', cint[,2], '\n')
    
    sapply(1:9, function(i) segments( (2*i-1)+0.5, cint[i,1], (2*i-1)+0.5, cint[i,2], col='darkred', lwd=1.5) )
    
    ## Satterwhite data plots ----
    plot.satt = function(y,g,ia){
      x = which(seq(from=1999, length.out=length(nyears.p))%in%c('2008'))
      points(x, y, pch=4, col='firebrick1', lwd=1.5);
      ## Satterwhite values for confidence intervals
      if(g==1 & ia%in%c(1,2)){y1.segm=0.0226; y2.segm=0.0452; legend('top', bty='n', c('Estim.2008 (ages 15-24)\n from Satterwhite et al.'), col='firebrick1', lwd=2.5, cex=1.5)}
      else if(g==1 & ia==3){y1.segm=0.004; y2.segm=0.0186; legend('top', bty='n', c('Estim.2008 (ages 25-39)\n from Satterwhite et al.'), col='firebrick1', lwd=2.5, cex=1.5)}
      else if(g==2 & ia%in%c(1,2)){y1.segm=0.0107; y2.segm=0.0255; legend('top', bty='n', c('Estim.2008 (ages 15-24)\n from Satterwhite et al.'), col='firebrick1', lwd=2.5, cex=1.5)}
      else if(g==2 & ia==3){y1.segm=0.0056; y2.segm=0.0181; legend('topright', bty='n', c('Estim.2008 (ages 25-39)\n from Satterwhite et al.'), col='firebrick1', lwd=2.5, cex=1.5)}
      segments(x, y1.segm, x, y2.segm, col='firebrick1', lwd=1.5 )
      x.seq = seq(from=(x-0.25), to=(x+0.25), by=0.01)
      polygon(c(x.seq, rev(x.seq)), c(rep(y1.segm, length(x.seq)),rep(y2.segm, length(x.seq))), col=t_col('firebrick1',75,'transpfire'), border=NA) }
    
    if(g==1 & ia==1){ plot.satt(0.0321, g, ia)} 
    else if (g==1 & ia==2){ plot.satt(0.0321, g, ia)}
    else if (g==1 & ia==3){ plot.satt(0.0087, g, ia)}
    else if (g==2 & ia==1){ plot.satt(0.0166, g, ia)}
    else if (g==2 & ia==2){ plot.satt(0.0166, g, ia)}
    else if (g==2 & ia==3){ plot.satt(0.0101, g, ia)}
} else if (d==2){
    ### DATA
    points(seq(from=1, to=16, by=2)+0.5, prev[1:8], pch=19, col='darkred')
    if(data$n.single.years>17){ lll = length(seq(from=17, to=data$n.single.years, by=2)+0.5)
    points(seq(from=17, to=data$n.single.years, by=2)+0.5, prev[9:11][1:lll], pch=4, col='darkred', cex=1.25)}

  cat('cint2', cint[,1], '\n')
  cat('cint2', cint[,2], '\n')
  sapply(1:8, function(i) segments((2*i-1)+0.5, cint[i,1], (2*i-1)+0.5, cint[i,2], col='darkred', lwd=1.5))

    ### Satterwhite
    if(g==1 & ia%in%seq.age){
      x.satt = which(seq(from=1999, length.out=length(nyears.p))%in%c('2008'))
      points(x.satt, 0.0032 , pch=4, col='firebrick1', lwd=1.5);
      segments(x.satt, 0.0016, x.satt, 0.0057, col='firebrick1', lwd=1.5 )

      if(ia==1){
        points(x.satt, 0.0062 , pch=4, col='firebrick3', lwd=1.5);
        segments(x.satt, 0.0038, x.satt, 0.0103, col='firebrick3', lwd=1.5 ) }

      x.satt.seq = seq(from=(x.satt-0.25), to=(x.satt+0.25), by=0.01)
      polygon(c(x.satt.seq, rev(x.satt.seq)), c(rep(0.0016, length(x.satt.seq)),rep(0.0057, length(x.satt.seq))),  col=t_col('firebrick1',75,'transpfire'), border=NA)

      if(ia==1){
        polygon(c(x.satt.seq, rev(x.satt.seq)), c(rep(0.0038, length(x.satt.seq)),rep(0.0103, length(x.satt.seq))),  col=t_col('firebrick3',75,'transpfire'), border=NA)

        legend('topright', bty='n', c('Estim.2008 (ages 15-39) from Satterwhite et al.', 'Estim.2008 (ages 15-24) from Satterwhite et al.'), col=c('firebrick1','firebrick3'), lwd=2) }
    }

    if(g==2 & ia%in%seq.age){
      x.satt = which(seq(from=1999, length.out=length(nyears.p))%in%c('2008'))
      points(x.satt, 0.0021 , pch=4, col='firebrick1', lwd=1.5);
      segments(x.satt, 0.0008, x.satt, 0.0043, col='firebrick1', lwd=1.5 )

      if(ia==1){
        points(x.satt, 0.0032 , pch=4, col='firebrick3', lwd=1.5);
        segments(x.satt, 0.0012, x.satt, 0.0084, col='firebrick3', lwd=1.5 ) }

      x.satt.seq = seq(from=(x.satt-0.25), to=(x.satt+0.25), by=0.01)
      polygon(c(x.satt.seq, rev(x.satt.seq)), c(rep(0.0008, length(x.satt.seq)),rep(0.0043, length(x.satt.seq))),  col=t_col('firebrick1',75,'transpfire'), border=NA)

      if(ia==1){
        polygon(c(x.satt.seq, rev(x.satt.seq)), c(rep(0.0012, length(x.satt.seq)),rep(0.0084, length(x.satt.seq))),  col=t_col('firebrick3',75,'transpfire'), border=NA)

        legend('topright', bty='n', c('Estim.2008 (ages 15-39) from Satterwhite et al.', 'Estim.2008 (ages 15-24) from Satterwhite et al.'), col=c('firebrick1','firebrick3'), lwd=2) }
    }
}   
    ## Calculation of prevalent cases ----
    if(g==1){g1=2} else if(g==2){g1=1}
    
     observed.pop  = data$N.by.year[,ia,g,d]*popul.factor
     projected.pop = proj.pop3[-1,c(2,ia+2),g1]
     projected.pop = projected.pop[1:pred.single.years,2]
     complete.pop = c(observed.pop, projected.pop)
     
     save(complete.pop, file=paste('prev_pop_k_',ia,'_g_',g,'_d_',d,'_',casereports,'_',screening.reporting, '.rda',sep='') )
     
     prevalent.cases[,ia,g,d] <<- mean.pct.prev*complete.pop
     mean.pct.prev[,ia,g,d]<<- mean.pct.prev
     
     #confint.0816[ia,]<<- c( cis[c(1,2),10]*complete.pop[10], cis[c(1,2),18]*complete.pop[18])
  }) ## close loop on prevalence 
  
  ## Save prevalent cases ----
   rownames(prevalent.cases)=seq(1999, length=(data$n.single.years+data$pred))

  #paste('prev_post_chn_k_',ia,'_g_',g,'_d_',d,'_',casereports,'_', screening.reporting,'.rda', sep='')
  
  
   write.csv(prevalent.cases[,,g,d], file= paste(current.folder,'/prev_cases_','g_',g,'_d_',d,'_',screening.reporting,'.csv', sep='')) #, sheetName="Prevalent cases")
   write.csv(mean.pct.prev[,,g,d], file= paste(current.folder,'/mean_pct_prev_','g_',g,'_d_',d,'_',screening.reporting,'.csv', sep='')) #, sheetName="Prevalence pct")
   #write.xlsx(confint.0816, file= paste(current.folder,'/confint_',g,'_',d,'.xlsx', sep=''), sheetName="confint")
   
}