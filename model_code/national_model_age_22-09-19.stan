data {
   int<lower=0> N;
  // CT
   int<lower = 0> CT[N]; 
   int<lower = 0> CTN[N];
   
   int<lower = 0> CTm[N]; 
   int<lower = 0> CTNm[N];
   //real<lower = 0> CTp;
   //real<lower = 0> CTvar;
   
   int<lower = 0> CTnD[N];
   int<lower = 0> CTnDN[N]; 
   
   int<lower = 0> CTnDm[N];
   int<lower = 0> CTnDNm[N]; 

  // T
   int<lower = 0> TnD[N];
   int<lower = 0> TnDN[N]; 
   
   int<lower = 0> TnDm[N];
   int<lower = 0> TnDNm[N]; 
   
   int<lower = 0> Te[N];
   int<lower = 0> TN[N]; 
   
   int<lower = 0> Tem[N];
   int<lower = 0> TNm[N];   
  // D
   int<lower = 0> D[N];
   int<lower = 0> DN[N]; 
   
   int<lower = 0> Dm[N];
   int<lower = 0> DNm[N]; 
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
  vector<lower = 0, upper = 1>[N] t_ct; // 
  vector<lower = 0, upper = 1>[N] ct_t; // 
  vector<lower = 0, upper = 1>[N] d_t;  //
  vector<lower = 0, upper = 1>[N] ct_d;   //
  vector<lower = 0, upper = 1>[N] d_ct;   //
  vector<lower = 0, upper = 1>[N] incid;   //
  vector<lower = 0, upper = 1>[N] dur;   //
  vector<lower = 0, upper = 1>[N] dur2;   //
  
  vector<lower = 0, upper = 1>[N] t_ctm; // 
  vector<lower = 0, upper = 1>[N] ct_tm; // 
  vector<lower = 0, upper = 1>[N] d_tm;  //
  vector<lower = 0, upper = 1>[N] ct_dm;   //
  vector<lower = 0, upper = 1>[N] d_ctm;   //
  vector<lower = 0, upper = 1>[N] incidm;   //
  vector<lower = 0, upper = 1>[N] durm;   //
  vector<lower = 0, upper = 1>[N] dur2m;   //
}

transformed parameters {
  vector<lower = 0, upper = 1>[N] ct ;   //
  vector<lower = 0, upper = 1>[N] d ;  
  vector<lower = 0, upper = 1>[N] t ;
  vector<lower = 0, upper = 1>[N] ct_and_d ;  //
  vector<lower = 0, upper = 1>[N] t_and_d ;    //
  
  vector<lower = 0, upper = 1>[N] ctm ;   //
  vector<lower = 0, upper = 1>[N] dm ;  
  vector<lower = 0, upper = 1>[N] tm ;
  vector<lower = 0, upper = 1>[N] ct_and_dm ;  //
  vector<lower = 0, upper = 1>[N] t_and_dm ;    //

 for (i in 1:N){
  ct[i] = d_ct[i]  *incid[i]  * dur[i]  + (1-d_ct[i] )*incid[i]  * dur2[i] ;   
  d[i] = ct[i]  * d_ct[i]  / ct_d[i] ;  
  t[i] = t_ct[i]  * ct[i]  / ct_t[i] ;
  ct_and_d[i] = ct_d[i]  * d[i] ;  
  t_and_d[i] = d_t[i]  * t[i] ;  
  
  ctm[i] = d_ctm[i]  *incidm[i]  * durm[i]  + (1-d_ctm[i] )*incidm[i]  * dur2m[i] ;   
  dm[i] = ctm[i]  * d_ctm[i]  / ct_dm[i] ;  
  tm[i] = t_ctm[i]  * ctm[i]  / ct_tm[i] ;
  ct_and_dm[i] = ct_dm[i]  * dm[i] ;  
  t_and_dm[i] = d_tm[i]  * tm[i] ;  
 }
 
}

model {
  // Prior 
   target += beta_lpdf(incid | 1, 30); 
   target += beta_lpdf(dur | 4, 4); 
   target += beta_lpdf(ct_t | 1.5, 3); // this should be narrow prior 
   target += beta_lpdf(t_ct | 1.5, 3); // 
   target += beta_lpdf(ct_d | 1.5, 3); 
   target += beta_lpdf(d_t | 1.5, 3); 
   target += beta_lpdf(d | 1.5, 3); 
   target += beta_lpdf(t | 1.5, 3); 
   target += beta_lpdf(d_ct | 1.5, 3); 
   
   target += beta_lpdf(incidm | 1, 30); 
   target += beta_lpdf(durm | 4, 4); 
   target += beta_lpdf(ct_tm | 1.5, 3); // this should be narrow prior 
   target += beta_lpdf(t_ctm | 1.5, 3); // 
   target += beta_lpdf(ct_dm | 1.5, 3); 
   target += beta_lpdf(d_tm | 1.5, 3); 
   target += beta_lpdf(dm | 1.5, 3); 
   target += beta_lpdf(tm | 1.5, 3); 
   target += beta_lpdf(d_ctm | 1.5, 3); 
    
  // Likelihood:
   target += binomial_lpmf(CT | CTN, ct)  + binomial_lpmf(D | DN, d) + binomial_lpmf(CTnD | CTnDN, ct_and_d) + binomial_lpmf(TnD | TnDN, t_and_d) + binomial_lpmf(Te | TN, t) +
             binomial_lpmf(CTm | CTNm, ctm)  + binomial_lpmf(Dm | DNm, dm) + binomial_lpmf(CTnDm | CTnDNm, ct_and_dm) + binomial_lpmf(TnDm | TnDNm, t_and_dm) + binomial_lpmf(Tem | TNm, tm); 
   // target += beta_lpdf(CTp | ct_alpha, ct_beta); 
}

 generated quantities {
   vector[N] ct_and_t;
   vector[N] ct_and_tm;

  for (i in 1:N){ 
   ct_and_t[i] = ct_t[i] * t[i];  // generate value of interest based on est values
   ct_and_tm[i] = ct_tm[i] * tm[i];  
  }

 }
