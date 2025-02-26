data {
  
  // NATIONAL
   int<lower=0> N;
   int<lower=0> N1;
   int<lower=0> N2;
   int<lower=0> N3;
  // CT
   int<lower = 0> CT[2, N1]; 
   int<lower = 0> CTN[2, N1];
   
   int<lower = 0> CTm[2, N1]; 
   int<lower = 0> CTNm[2, N1];

   int<lower = 0> Te[2, N2];
   int<lower = 0> TN[2, N2]; 
   
   int<lower = 0> Tem[2, N2];
   int<lower = 0> TNm[2, N2];   
  // D
   int<lower = 0> D[2, N3];
   int<lower = 0> DN[2, N3]; 
   
   int<lower = 0> Dm[2, N3];
   int<lower = 0> DNm[2, N3]; 
  // years
   real year[N];
   real<lower = 0> age[2];
   real<lower = 0> sex[2];
   
  // STATE
  int<lower=0> yr;
   int<lower=0> age_st;
   int<lower=0> state;
   
   // WOMEN
   int<lower=0> f_ct_lnt;
   int<lower=0> f_ct[f_ct_lnt, 3];
   
   int<lower=0> f_ctd_lnt;
   int<lower=0> f_ctd[f_ctd_lnt, 3];

   int<lower=0> f_gc_lnt;
   int<lower=0> f_gc[f_gc_lnt, 3];
   
   int<lower=0> f_gcd_lnt;
   int<lower=0> f_gcd[f_gcd_lnt, 3];
   
   // MEN
   int<lower=0> m_ct_lnt;
   int<lower=0> m_ct[m_ct_lnt, 3];
   
   int<lower=0> m_ctd_lnt;
   int<lower=0> m_ctd[m_ctd_lnt, 3];

   int<lower=0> m_gc_lnt;
   int<lower=0> m_gc[m_gc_lnt, 3];
   
   int<lower=0> m_gcd_lnt;
   int<lower=0> m_gcd[m_gcd_lnt, 3];
   
  // CT
  // int<lower = 0> CD[yr, age, state];
   int Cnum_f[yr, age_st, state];
   int Cden_f[yr, age_st, state];
   
   int CDnum_f[yr, age_st, state];
   int CDden_f[yr, age_st, state];
   
   int Cnum_m[yr, age_st, state];
   int Cden_m[yr, age_st, state];
   
   int CDnum_m[yr, age_st, state];
   int CDden_m[yr, age_st, state];
   
  // GC
   int Gnum_f[yr, age_st, state];
   int Gden_f[yr, age_st, state];
   
   int GDnum_f[yr, age_st, state];
   int GDden_f[yr, age_st, state];
   
   int Gnum_m[yr, age_st, state];
   int Gden_m[yr, age_st, state];
   
   int GDnum_m[yr, age_st, state];
   int GDden_m[yr, age_st, state];
}

parameters {

// national
   matrix<lower = 0, upper = 1>[2, N] t_ct; //
   matrix<lower = 0, upper = 1>[2, N] ct_t; //
   matrix<lower = 0, upper = 1>[2, N] t_ctm; //
   matrix<lower = 0, upper = 1>[2, N] ct_tm; //
  real b0_age[2,2];
  real b_age;
  real b_sex;
  real b_year[2];
  real<lower = 0> clear;   //
  real<lower = 0> clearm;   //
  real<lower = 0, upper = 1> psymptf;   //
  real<lower = 0, upper = 1> psymptm;   //
  real<lower = 0, upper = 1> dursymptf;   //
  real<lower = 0, upper = 1> dursymptm;   //
  
// state  
     // women
   real<lower = 0> RR_f0[age_st,state];
   real<lower = 0> RR_f0_rel[age_st,state];
   real<lower = 0, upper = 1> tr_f0[age_st,state]; 
   real<lower = 0, upper = 1> tr_f0_rel[age_st,state]; 
   
   real<lower = 0, upper = 1> p_ct_f[yr,age_st, state] ; 
   real<lower = 0, upper = 1> p_gc_f[yr,age_st, state] ; 
   // men
   real<lower = 0> RR_m0[age_st,state];
   real<lower = 0> RR_m0_rel[age_st,state];
   real<lower = 0, upper = 1> tr_m0[age_st,state]; 
   real<lower = 0, upper = 1> tr_m0_rel[age_st,state]; 
   
   real<lower = 0, upper = 1> p_ct_m[yr,age_st, state] ; 
   real<lower = 0, upper = 1> p_gc_m[yr,age_st, state] ; 
}

transformed parameters {

  // NATIONAL
  //  bounds dont matter if defined by other variables ?
  // matrix<lower = 0, upper = 1>[2, N] logit_ct ;   //
  matrix<lower = 0, upper = 1>[2, N] d ;  
  matrix<lower = 0, upper = 1>[2, N] dm ; 
  matrix<lower = 0, upper = 1>[2, N] te ;  
  matrix<lower = 0, upper = 1>[2, N] tem ; 

  real<lower = 0, upper = 1> ct[2, 2, N];   //
  matrix<lower = 0, upper = 1>[2, N] incid;   //
  matrix<lower = 0, upper = 1>[2, N] incidm;   //
  matrix<lower = 0, upper = 3>[2, N] dur;   //
  matrix<lower = 0, upper = 3>[2, N] durm;   //
  matrix<lower = 0>[2, N] rr;  //
  matrix<lower = 0>[2, N] rrm;  //
  
  // STATE
  real<lower = 0, upper = 1> q_ct_f[yr, age_st, state] ; 
  real<lower = 0, upper = 1> d_ct_f[yr, age_st, state] ; 

  real<lower = 0, upper = 1> q_gc_f[yr, age_st, state] ; 
  real<lower = 0, upper = 1> d_gc_f[yr, age_st, state] ; 
  
  real<lower = 0, upper = 1> tr_f[yr,age_st, state]; //
  real<lower = 0> RR_f[yr,age_st, state] ; 
  // men

  real<lower = 0, upper = 1> q_ct_m[yr, age_st, state] ; 
  real<lower = 0, upper = 1> d_ct_m[yr, age_st, state] ; 
  
  real<lower = 0, upper = 1> q_gc_m[yr, age_st, state] ; 
  real<lower = 0, upper = 1> d_gc_m[yr, age_st, state] ; 
  
  real<lower = 0, upper = 1> tr_m[yr,age_st, state]; //
  real<lower = 0> RR_m[yr,age_st, state] ; 

  // NATIONAL
 //  monthly rate of testing
  real t_rate[2, N];
  real tm_rate[2, N];
  
 for (s in 1:2){
   for (y in 1:N){
     for (a in 1:2){
       ct[s,a,y]  = 1 / (1 + exp(-1*(b0_age[s,a] + b_age*age[a] +  b_sex*sex[s] + b_year[s]*year[y]))); 
   }
  }
 }

  for (y in 1:N){
    for (a in 1:2){
    te[a,y]  = t_ct[a,y]  * ct[1,a,y]  / ct_t[a,y];
    tem[a,y] = t_ctm[a,y] * ct[2,a,y] / ct_tm[a,y];
    }
   }
   
  for (y in 1:N){
   for (a in 1:2){
     rr[a,y] = (psymptf + (1-psymptf)*te[a,y]) / (te[a,y]);
     rrm[a,y] = (psymptm + (1-psymptm)*tem[a,y]) / (tem[a,y]);
       
      t_rate[a,y] =  -log( (1- te[a,y]  )/1 ); //annual rate
      tm_rate[a,y] = -log( (1- tem[a,y] )/1 );//annual rate
      
     dur[a,y] = 1/(psymptf * 1/dursymptf + (1-psymptf) * t_rate[a,y] + clear);
     durm[a,y] = 1/(psymptm * 1/dursymptm + (1-psymptm) * tm_rate[a,y] + clearm);
           
     incid[a,y] =  ct[1,a,y] / dur[a,y]; //
     incidm[a,y] = ct[2,a,y]/durm[a,y]; // 
     
     d[a,y]  = incid[a,y] * rr[a,y]*t_rate[a,y] / (rr[a,y]*t_rate[a,y] + clear);
     dm[a,y] = incidm[a,y]* rrm[a,y]*tm_rate[a,y]/ (rrm[a,y]*tm_rate[a,y] + clearm);
 
    }
  }
    // STATE
      // women
      for (a in 1:age_st){
         for (st in 1:state){
           RR_f[1,a, st] = RR_f0[a, st];
           RR_f[2,a, st] = RR_f0_rel[a, st]*RR_f0[a, st];
           RR_f[3,a, st] = RR_f0[a, st];
           RR_f[4,a, st] = RR_f0[a, st];
           
           RR_m[1,a, st] = RR_m0[a, st];
           RR_m[2,a, st] = RR_m0_rel[a, st]*RR_m0[a, st];
           RR_m[3,a, st] = RR_m0[a, st];
           RR_m[4,a, st] = RR_m0[a, st];
           
           tr_f[1,a, st] = tr_f0[a, st];
           tr_f[2,a, st] = tr_f0_rel[a,st]*tr_f0[a, st];
           tr_f[3,a, st] = tr_f0[a, st];
           tr_f[4,a, st] = tr_f0[a, st];
           
           tr_m[1,a, st] = tr_m0[a, st];
           tr_m[2,a, st] = tr_m0_rel[a,st]*tr_m0[a, st];
           tr_m[3,a, st] = tr_m0[a, st];
           tr_m[4,a, st] = tr_m0[a, st];
         }
      }

      for (a in 1:age_st){
         for (st in 1:state){
              for (y in 1:yr){  
                
       q_ct_f[y,a,st]  = p_ct_f[y,a,st]*RR_f[y,a,st]; 
       d_ct_f[y,a,st]  = p_ct_f[y,a,st]*tr_f[y,a,st]; 
       
       q_gc_f[y,a,st]  = p_gc_f[y,a,st]*RR_f[y,a,st]; 
       d_gc_f[y,a,st]  = p_gc_f[y,a,st]*tr_f[y,a,st];  
       
       q_ct_m[y,a,st]  = p_ct_m[y,a,st]*RR_m[y,a,st]; 
       d_ct_m[y,a,st]  = p_ct_m[y,a,st]*tr_m[y,a,st]; 
      
       q_gc_m[y,a,st]  = p_gc_m[y,a,st]*RR_m[y,a,st]; 
       d_gc_m[y,a,st]  = p_gc_m[y,a,st]*tr_m[y,a,st];  
        }
      }
    }  
}

model {

  // NATIONAL
  // Prior 
  target += lognormal_lpdf(clear | 0.00504, 0.22206);
  target += lognormal_lpdf(clearm | -0.0830, 0.07545);
  target += beta_lpdf(dursymptf | 10.46, 136.01);
  target += beta_lpdf(dursymptm | 10.44, 65.49);
  target += beta_lpdf(psymptf | 25.35, 73.71 );
  target += beta_lpdf(psymptm | 9.50, 48.36);


  // Across all years
for (i in 1:N){
  for (j in 1:2){
   // target += beta_lpdf(incid[j,i] | 1, 10);
   // target += beta_lpdf(incidm[j,i] | 1, 10);
   // target += weibull_lpdf(d_t[j,] | 1.4, 0.3);
   // target += weibull_lpdf(d_tm[j,] | 1.4, 0.3);
   target += beta_lpdf(ct_t[j,i] | 1.2, 2); // this should be narrow prior 
   target += beta_lpdf(t_ct[j,i] | 1.2, 2); //
   target += beta_lpdf(ct_tm[j,i] | 1.2, 2); // this should be narrow prior 
   target += beta_lpdf(t_ctm[j,i] | 1.2, 2); //
    }
  }


// NATIONAL
for (i in 2:N){
    for (j in 1:2){
  // Likelihood for NHANES data
   target += binomial_lpmf(D[j,i-1] | DN[j,i-1], d[j,i]) + binomial_lpmf(Dm[j,i-1] | DNm[j,i-1], dm[j,i]); 
    }
  }
  
for (i in 1:(N-4)){
    for (j in 1:2){
  // Likelihood for NHANES data
   target += binomial_lpmf(CT[j,i] | CTN[j,i], ct[1,j,i]) + binomial_lpmf(CTm[j,i] | CTNm[j,i], ct[2,j,i]); 
    }
  }
  
  // Likelihood for NSFG
  for (i2 in 14:(N-1)){
    for  (j in 1:2){
    target += binomial_lpmf(Te[j,i2-13] | TN[j,i2-13], te[j,i2]) + binomial_lpmf(Tem[j,i2-13] | TNm[j,i2-13], tem[j,i2]);
    }
  }
  
  // STATE
  // non pandemic years (2019, 2021, 2022)
  for (a in 1:5){ 
    for (st in 1:state){  
      RR_f0[a,st] ~gamma(40, 30);
      tr_f0[a,st] ~beta(5, 8);
      
      RR_m0[a,st] ~gamma(40, 30);
      tr_m0[a,st] ~beta(5, 8);
    }
  }
  
// pandemic year  
// DATA FOR THIS NOW MADE UP! NEED TO LOOK AT THE CLAIMS AND DIAGN!!!!!!!!!
  for (a in 1:5){ 
    for (st in 1:state){  
      RR_f0_rel[a,st] ~gamma(90, 50);
      tr_f0_rel[a,st] ~beta(90, 10);
      
      RR_m0_rel[a,st] ~gamma(90, 50);
      tr_m0_rel[a,st] ~beta(90, 10);
    }
  }

  for (y in 1:yr){
     for (a in 1:5){ 
        for (st in 1:state){ 
      p_ct_f[y,a,st] ~beta(1, 10);
      p_gc_f[y,a,st] ~beta(1, 10);
      
      p_ct_m[y,a,st] ~beta(1, 10);
      p_gc_m[y,a,st] ~beta(1, 10);
      
      }
    }
  }
  
  for (i in 1:f_ct_lnt){
      Cnum_f[f_ct[i,1],f_ct[i,2],f_ct[i,3]] ~ binomial(Cden_f[f_ct[i,1],f_ct[i,2],f_ct[i,3]], q_ct_f[f_ct[i,1],f_ct[i,2],f_ct[i,3]]); 
 }
 
    for (i in 1:f_ctd_lnt){
      CDnum_f[f_ctd[i,1],f_ctd[i,2],f_ctd[i,3]] ~ binomial(CDden_f[f_ctd[i,1],f_ctd[i,2],f_ctd[i,3]], d_ct_f[f_ctd[i,1],f_ctd[i,2],f_ctd[i,3]]); 
 }
 
   for (i in 1:f_gc_lnt){
      Gnum_f[f_gc[i,1],f_gc[i,2],f_gc[i,3]] ~ binomial(Gden_f[f_gc[i,1],f_gc[i,2],f_gc[i,3]], q_gc_f[f_gc[i,1],f_gc[i,2],f_gc[i,3]]); 
 }
 
    for (i in 1:f_gcd_lnt){
      GDnum_f[f_gcd[i,1],f_gcd[i,2],f_gcd[i,3]] ~ binomial(GDden_f[f_gcd[i,1],f_gcd[i,2],f_gcd[i,3]], d_gc_f[f_gcd[i,1],f_gcd[i,2],f_gcd[i,3]]); 
 }
 
   for (i in 1:m_ct_lnt){
      Cnum_m[m_ct[i,1],m_ct[i,2],m_ct[i,3]] ~ binomial(Cden_m[m_ct[i,1],m_ct[i,2],m_ct[i,3]], q_ct_m[m_ct[i,1],m_ct[i,2],m_ct[i,3]]); 
 }
 
    for (i in 1:m_ctd_lnt){
      CDnum_m[m_ctd[i,1],m_ctd[i,2],m_ctd[i,3]] ~ binomial(CDden_m[m_ctd[i,1],m_ctd[i,2],m_ctd[i,3]], d_ct_m[m_ctd[i,1],m_ctd[i,2],m_ctd[i,3]]); 
 }
 
   for (i in 1:m_gc_lnt){
      Gnum_m[m_gc[i,1],m_gc[i,2],m_gc[i,3]] ~ binomial(Gden_m[m_gc[i,1],m_gc[i,2],m_gc[i,3]], q_gc_m[m_gc[i,1],m_gc[i,2],m_gc[i,3]]); 
 }
 
    for (i in 1:m_gcd_lnt){
      GDnum_m[m_gcd[i,1],m_gcd[i,2],m_gcd[i,3]] ~ binomial(GDden_m[m_gcd[i,1],m_gcd[i,2],m_gcd[i,3]], d_gc_m[m_gcd[i,1],m_gcd[i,2],m_gcd[i,3]]); 
 }
  
}

 // generated quantities {
 //  
 //    matrix [2, N] t_rate_calc;
 //    matrix [2, N] tm_rate_calc; 
 //    matrix [2, N] d_t_calc;
 //    matrix [2, N] d_tm_calc; 
 //  
 //  for (y in 1:N){
 //   for (a in 1:2){
 //      t_rate_calc[a, y] =  -log( (1- ((1.0* Te[a, y] ) / (1.0* TN[a, y]) ) )) /12; // needed to convert int to real
 //      tm_rate_calc[a, y] = -log( (1- ((1.0*Tem[a, y] ) / (1.0*TNm[a, y]) ) )) /12;
 //      
 //    d_t_calc[a,y]  = d[a,y]  / te[a,y] ;
 //    d_tm_calc[a,y] = dm[a,y] / tem[a,y];
 //    }
 //  }
 // 
 // }
