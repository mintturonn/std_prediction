data {
  // CT
   int<lower = 0> CT; 
   int<lower = 0> CTN; 
   
   int<lower = 0> CTnD;
   int<lower = 0> CTnDN; 

  // T
   int<lower = 0> TnD;
   int<lower = 0> TnDN; 
   
   int<lower = 0> T;
   int<lower = 0> TN; 
  // D
   int<lower = 0> D;
   int<lower = 0> DN;   
}

// transformed data {
//   
//  //  CT
//   real  alpha_CT = ((1 - CT) / CTvar - 1 / CT) * CT ^ 2;
//   real  beta_CT = alpha_CT * (1 / CT - 1);
//   
//   real  alpha_CTnD = ((1 - CTnD) / CTnDvar - 1 / CTnD) * CTnD ^ 2;
//   real  beta_CTnD = alpha_CTnD * (1 / CTnD - 1);
//  // 
// }
  
parameters {

  //real<lower=0, upper = 1> d;    // 
  real<lower=0, upper = 1> t_ct; // 
  real<lower=0, upper = 1> ct_t; // 
  real<lower=0, upper = 1> d_t;  //
  real<lower=0, upper=1> ct_d;   //
  real<lower=0, upper=1> d_ct;   //
  real<lower=0, upper=1> incid;   //
  real<lower=0, upper=1> dur;   //
}

transformed parameters {
  real<lower = 0, upper=1> ct = d_ct *incid * dur + (1-d_ct)*incid * dur*2;   //
  real<lower = 0, upper=1> d = ct * d_ct / ct_d;  
  real<lower = 0, upper = 1> t = t_ct * ct / ct_t;
  real<lower = 0, upper = 1> ct_and_d = ct_d * d;  //
  real<lower = 0, upper = 1> t_and_d = d_t * t;    //
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
    
  // Likelihood:
   target +=  binomial_lpmf(D | DN, d) + binomial_lpmf(CT | CTN, ct) + binomial_lpmf(CTnD | CTnDN, ct_and_d) + binomial_lpmf(TnD | TnDN, t_and_d) + binomial_lpmf(T | TN, t) ; //+  + beta_lpdf(alpha_CTnD | ct_and_d, beta_CTnD); //  binomial_lpmf(K | N, theta) + binomial_lpmf(k2 | N2, theta2); //
}

 generated quantities {
   real ct_and_t = ct_t * t;  // generate value of interest based on est values
   real ctN = ct * 100; 
   real ct_tN = ct_t * t*100; 
//   // new
//   real t_ct = ct_t * ct / t;
 }
