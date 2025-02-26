data {
   int<lower=0> N;
  // CT
   int<lower = 0> CT[2, N]; 
   int<lower = 0> CTN[2, N];
   
   int<lower = 0> CTm[2, N]; 
   int<lower = 0> CTNm[2, N];
   //real<lower = 0> CTp;
   //real<lower = 0> CTvar;
   
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
}

// transformed data {
// 
//  //  CT
//   real  alpha_CT = ((1 - CTp) / CTvar - 1 / CTp) * CTp ^ 2;
//  
//   // real  alpha_CTnD = ((1 - CTnD) / CTnDvar - 1 / CTnD) * CTnD ^ 2;
//   // real  beta_CTnD = alpha_CTnD * (1 / CTnD - 1);
//  //
// }
  
parameters {

  //real<lower=0, upper = 1> d;    // 
  // matrix<lower = 0, upper = 1>[2, N] t_ct; // 
  // matrix<lower = 0, upper = 1>[2, N] ct_t; // 
  // matrix<lower = 0, upper = 1>[2, N] d_t;  //
  // matrix<lower = 0, upper = 1>[2, N] ct_d;   //
  // matrix<lower = 0, upper = 1>[2, N] d_ct;   //
//  matrix<lower = 0, upper = 1>[2, N] dur2;   //
  
  // matrix<lower = 0, upper = 1>[2, N] t_ctm; // 
  // matrix<lower = 0, upper = 1>[2, N] ct_tm; // 
  // matrix<lower = 0, upper = 1>[2, N] d_tm;  //
  // matrix<lower = 0, upper = 1>[2, N] ct_dm;   //
  // matrix<lower = 0, upper = 1>[2, N] d_ctm;   //
//  matrix<lower = 0, upper = 1>[2, N] dur2m;   //
//  real b0[2];
  real b0_age[2,2];
  matrix[2,2] b_age;
  vector[2] b_year;
  // matrix<lower = 0, upper = 3>[2, N] dur;   //
  // matrix<lower = 0, upper = 3>[2, N] durm;   //
  // 
  // real<lower = 0, upper = 1> clear;   //
  // real<lower = 0, upper = 1> clearm;   //
}

transformed parameters {
  //  bounds dont matter if defined by other variables ?
 // matrix<lower = 0, upper = 1>[2, N] logit_ct ;   //
  // matrix<lower = 0, upper = 1>[2, N] d ;  
  // matrix<lower = 0, upper = 1>[2, N] t ;
 //  matrix<lower = 0, upper = 1>[2, N] ct_and_d ;  //
 //  matrix<lower = 0, upper = 1>[2, N] t_and_d ;    //

  // matrix<lower = 0, upper = 1>[2, N] dm ;  
  // matrix<lower = 0, upper = 1>[2, N] tm ;
  //  matrix<lower = 0, upper = 1>[2, N] ct_and_dm ;  //
  //  matrix<lower = 0, upper = 1>[2, N] t_and_dm ;    //
  
  matrix<lower = 0, upper = 1>[2, N] ctm ;   //
  matrix<lower = 0, upper = 1>[2, N] ct ;   //
  // matrix<lower = 0, upper = 1>[2, N] incid;   //
  // matrix<lower = 0, upper = 1>[2, N] incidm;   //

 for (y in 1:N){
   for (a in 1:2){
  ct[a,y]  = 1 / (1 + exp(-1*(b0_age[1,a] + b_age[1,a]*age[a] + b_year[1]*year[y] ))); 
  ctm[a,y] = 1 / (1 + exp(-1*(b0_age[2,a] + b_age[2,a]*age[a] + b_year[2]*year[y] ))); 
   }
 }

// placeholder correction as ct = 1 is common 
// if (ct[1,1] == 1)  
//   ct= [ [10^-9, 10^-9, 10^-9], [ 10^-9, 10^-9, 10^-9]];
//   
// if (ctm[1,1] == 1)  
//   ctm= [ [10^-9, 10^-9, 10^-9], [ 10^-9, 10^-9, 10^-9]];
// 
// print(dur)
  
//   for (y in 1:N){
//     for (a in 1:2){
//      incid[a,y] =  ct[a,y] / dur[a,y]; //+  incid[j,i]; //ct[j,i]/ dur[j,i] = 
//      incidm[a,y] = ctm[a,y]/durm[a,y]; // ctm[j,i]/ 
// //     d[j,i] = dur[j,i]+  incid[j,i];
// // //  d[j,i] = ct[j,i]  * d_ct[j,i]  / ct_d[j,i] ;  
// // //  t[j,i] = t_ct[j,i]  * ct[j,i]  / ct_t[j,i] ;
// //  // ct_and_d[j,i] = ct_d[j,i]  * d[j,i] ;  
// //  // t_and_d[j,i] = d_t[j,i]  * t[j,i] ;  
// //   
// //  // ctm[j,i] = d_ctm[j,i]  *incidm[j,i]  * durm[j,i]  + (1-d_ctm[j,i] )*incidm[j,i]  * dur2m[j,i] ;   
// // //  dm[j,i] = ctm[j,i]  * d_ctm[j,i]  / ct_dm[j,i] ;  
// // //  tm[j,i] = t_ctm[j,i]  * ctm[j,i]  / ct_tm[j,i] ;
// //  // ct_and_dm[j,i] = ct_dm[j,i]  * dm[j,i] ;  
// //  // t_and_dm[j,i] = d_tm[j,i]  * tm[j,i] ;  
//     }
//    }
 
}

model {
  // Prior 
  // target += beta_lpdf(clear | 18.9, 203.1);
  // target += beta_lpdf(clearm | 161.9, 1943.6 );
for (i in 1:N){
  for (j in 1:2){
   // target += beta_lpdf(incid[j,i] | 1, 10);
   // target += beta_lpdf(incidm[j,i] | 1, 10);
   // target += weibull_lpdf(dur[j,i] | 2.8, 0.3);
   // target += weibull_lpdf(durm[j,i] | 3.0, 0.4);
  // target += beta_lpdf(ct_t[j,] | 1.5, 3); // this should be narrow prior 
  // target += beta_lpdf(t_ct[j,] | 1.5, 3); // 
  // target += beta_lpdf(ct_d[j,] | 1.5, 3); 
  // target += beta_lpdf(d_t[j,] | 1.5, 3); 
  // target += beta_lpdf(d_ct[j,] | 1.5, 3); 
   
  // target += beta_lpdf(incidm[j,] | 1, 30); 
 //  target += beta_lpdf(durm[j,] | 4, 4); 

 //  target += beta_lpdf(ct_tm[j,] | 1.5, 3); // this should be narrow prior 
 //  target += beta_lpdf(t_ctm[j,] | 1.5, 3); // 
 //  target += beta_lpdf(ct_dm[j,] | 1.5, 3); 
 //  target += beta_lpdf(d_tm[j,] | 1.5, 3); 
  // target += beta_lpdf(d_ctm[j,] | 1.5, 3); 
    
  // Likelihood:
   target += binomial_lpmf(CT[j,i] | CTN[j,i], ct[j,i]) + // + binomial_lpmf(D[j,] | DN[j,], d[j,]) + 
           //  binomial_lpmf(CTnD[j,] | CTnDN[j,], ct_and_d[j,]) + binomial_lpmf(TnD[j,] | TnDN[j,], t_and_d[j,]) + 
          //   binomial_lpmf(Te[j,] | TN[j,], t[j,]) +
             binomial_lpmf(CTm[j,i] | CTNm[j,i], ctm[j,i]);  //+ binomial_lpmf(Dm[j,] | DNm[j,], dm[j,]) + 
         //    binomial_lpmf(CTnDm[j,] | CTnDNm[j,], ct_and_dm[j,]) + binomial_lpmf(TnDm[j,] | TnDNm[j,], t_and_dm[j,]) + 
         //    binomial_lpmf(Tem[j,] | TNm[j,], tm[j,]); 
   // target += beta_lpdf(CTp | ct_alpha, ct_beta); 
    }
  }
}

 generated quantities {
  //  matrix[2, N] ct_and_t;
   
    matrix [2, N] ct_calc  ;
    matrix [2, N] ctm_calc  ;   //
  
  for (y in 1:N){
   for (a in 1:2){
        ct_calc[a,y]  = 1 / (1 + exp(-1*(b0_age[1,a] + b_age[1,a]*age[a] + b_year[1]*year[y] ))); 
        ctm_calc [a,y]  = 1 / (1 + exp(-1*(b0_age[2,a] + b_age[2,a]*age[a] + b_year[2]*year[y] ))); 
    }
  }
 
  // 
  // for (i in 1:N){ 
  //   for(j in 1:2){
  //  ct_and_t[j,i] = ct_t[j,i] * t[j,i];  // generate value of interest based on est values
  //  ct_and_tm[j,i] = ct_tm[j,i] * tm[j,i];  
  //   }
  // }

 }
