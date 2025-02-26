data {
   int<lower=0> N;
  // CT
   int<lower = 0> CT[2, N]; 
   int<lower = 0> CTN[2, N];
   
   int<lower = 0> CTm[2, N]; 
   int<lower = 0> CTNm[2, N];

   int<lower = 0> CTnD[2, N];
   int<lower = 0> CTnDN[2, N]; 
   
   int<lower = 0> CTnDm[2, N];
   int<lower = 0> CTnDNm[2, N]; 
  // T
   int<lower = 0> TnD[2, N];
   int<lower = 0> TnDN[2, N]; 
   
   int<lower = 0> TnDm[2, N];
   int<lower = 0> TnDNm[2, N]; 
   
   int<lower = 0> Te[2, N];
   int<lower = 0> TN[2, N]; 
   
   int<lower = 0> Tem[2, N];
   int<lower = 0> TNm[2, N];   
  // D
   int<lower = 0> D[2, N];
   int<lower = 0> DN[2, N]; 
   
   int<lower = 0> Dm[2, N];
   int<lower = 0> DNm[2, N]; 
  // years
   real year[N];
   real<lower = 0> age[2];
   real<lower = 0> sex[2];
}

transformed data {

 //  monthly rate of testing
  real t_rate[2, N];
  real tm_rate[2, N];
  
 for (y in 1:N){
   for (a in 1:2){
      t_rate[a, y] =  -log( (1- ((1.0* Te[a, y] ) / (1.0* TN[a, y]) ) )) /12; // needed to convert int to real
      tm_rate[a, y] = -log( (1- ((1.0*Tem[a, y] ) / (1.0*TNm[a, y]) ) )) /12;
    }
  }
}
  
parameters {

   matrix<lower = 0, upper = 1>[2, N] t_ct; // 
   matrix<lower = 0, upper = 1>[2, N] ct_t; // 
   matrix<lower = 0, upper = 1>[2, N] t_ctm; // 
   matrix<lower = 0, upper = 1>[2, N] ct_tm; // 

//  real b0[2];
  real b0_age[2,2];
  real b_age;
  real b_sex;
  real b_year[2];
  matrix<lower = 0, upper = 3>[2, N] dur;   //
  matrix<lower = 0, upper = 3>[2, N] durm;   //
  
  real<lower = 0, upper = 1> clear;   //
  real<lower = 0, upper = 1> clearm;   //
}

transformed parameters {
  //  bounds dont matter if defined by other variables ?
 // matrix<lower = 0, upper = 1>[2, N] logit_ct ;   //
  matrix<lower = 0, upper = 1>[2, N] d ;  
  matrix<lower = 0, upper = 1>[2, N] dm ; 
  matrix<lower = 0, upper = 1>[2, N] te ;  
  matrix<lower = 0, upper = 1>[2, N] tem ; 
 // matrix<lower = 0, upper = 1>[2, N] ctm ;   //
  real<lower = 0, upper = 1> ct[2, 2, N];   //
  matrix<lower = 0, upper = 1>[2, N] incid;   //
  matrix<lower = 0, upper = 1>[2, N] incidm;   //
  // matrix<lower = 0>[2, N] d_t;  //
  // matrix<lower = 0>[2, N] d_tm;  //

 for (s in 1:2){
   for (y in 1:N){
     for (a in 1:2){
       ct[s,a,y]  = 1 / (1 + exp(-1*(b0_age[s,a] + b_age*age[a] +  b_sex*sex[s] + b_year[s]*year[y]))); 
   }
  }
 }

  for (y in 1:N){
    for (a in 1:2){
      
    incid[a,y] =  ct[1,a,y] / dur[a,y]; //
    incidm[a,y] = ct[2,a,y]/durm[a,y]; // 
     
    d[a,y]  = incid[a,y] * t_rate[a,y] / (t_rate[a,y] + clear);
    dm[a,y] = incidm[a,y]* tm_rate[a,y]/ (tm_rate[a,y] + clearm);

    te[a,y]  = t_ct[a,y]  * ct[1,a,y]  / ct_t[a,y];
    tem[a,y] = t_ctm[a,y] * ct[2,a,y] / ct_tm[a,y];
    
    // if ( (d[a,y]  / te[a,y]) > 1) {
    //   reject("d_t > 1");
    // }
    // d_t[a,y]  = d[a,y]  / te[a,y] ;
    // d_tm[a,y] = dm[a,y] / tem[a,y];
    }
   }
}

model {
  // Prior 
  target += beta_lpdf(clear | 18.9, 203.1);
  target += beta_lpdf(clearm | 161.9, 1943.6 );
  
for (i in 1:N){
  for (j in 1:2){
   target += beta_lpdf(incid[j,i] | 1, 10);
   target += beta_lpdf(incidm[j,i] | 1, 10);
   // target += weibull_lpdf(dur[j,i] | 3, 0.7);
   // target += weibull_lpdf(durm[j,i] | 3.0, 0.7);
   target += weibull_lpdf(dur[j,i] | 2.8, 0.3);
   target += weibull_lpdf(durm[j,i] | 3.0, 0.4);
   target += beta_lpdf(ct_t[j,i] | 1.2, 2); // this should be narrow prior 
   target += beta_lpdf(t_ct[j,i] | 1.2, 2); //
   target += beta_lpdf(ct_tm[j,i] | 1.2, 2); // this should be narrow prior 
   target += beta_lpdf(t_ctm[j,i] | 1.2, 2); //
   // target += weibull_lpdf(d_t[j,] | 1.4, 0.3);
   // target += weibull_lpdf(d_tm[j,] | 1.4, 0.3);
    
  // Likelihood:
   target += binomial_lpmf(CT[j,i] | CTN[j,i], ct[1,j,i]) + binomial_lpmf(D[j,i] | DN[j,i], d[j,i]) + binomial_lpmf(Te[j,i] | TN[j,i], te[j,i]) +
             binomial_lpmf(CTm[j,i] | CTNm[j,i], ct[2,j,i]) + binomial_lpmf(Dm[j,i] | DNm[j,], dm[j,i]) + binomial_lpmf(Tem[j,i] | TNm[j,i], tem[j,i]); 
         //    binomial_lpmf(CTnDm[j,] | CTnDNm[j,], ct_and_dm[j,]) + binomial_lpmf(TnDm[j,] | TnDNm[j,], t_and_dm[j,]) + 
   // target += beta_lpdf(CTp | ct_alpha, ct_beta); 
    }
  }
}

 generated quantities {
  
    matrix [2, N] t_rate_calc;
    matrix [2, N] tm_rate_calc; 
    matrix [2, N] d_t_calc;
    matrix [2, N] d_tm_calc; 
  
  for (y in 1:N){
   for (a in 1:2){
      t_rate_calc[a, y] =  -log( (1- ((1.0* Te[a, y] ) / (1.0* TN[a, y]) ) )) /12; // needed to convert int to real
      tm_rate_calc[a, y] = -log( (1- ((1.0*Tem[a, y] ) / (1.0*TNm[a, y]) ) )) /12;
      
    d_t_calc[a,y]  = d[a,y]  / te[a,y] ;
    d_tm_calc[a,y] = dm[a,y] / tem[a,y];
    }
  }

 }
