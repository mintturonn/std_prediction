data {
  
  // NATIONAL
   int<lower=0> N;
   int<lower=0> N1;
   int<lower=0> N2;
   int<lower = 0> age; // should be the same as state_st
   int<lower=2010, upper=2023> years[2]; //years[N];
  // CT
   int<lower = 0> CT[2, N1]; 
   int<lower = 0> CTN[2, N1];
   
   int<lower = 0> CTm[2, N1]; 
   int<lower = 0> CTNm[2, N1];

   int<lower = 0> Te[2, N2];
   int<lower = 0> TN[2, N2]; 
   
   int<lower = 0> Te_hedis;
   int<lower = 0> TN_hedis;
   
   int<lower = 0> Tem[2, N2];
   int<lower = 0> TNm[2, N2];   

     // STATE
 // int<lower=0> yr;
   int<lower=0> age_st;
   int<lower=0> state;
   
   //populattion
   int<lower=0> pop_f[age_st, state];
   int<lower=0> pop_m[age_st, state];
   
   // WOMEN
   int<lower=0> f_ct_lnt;
   int<lower=0> f_ct[f_ct_lnt, 2];
   
   int<lower=0> f_ctd_lnt;
   int<lower=0> f_ctd[f_ctd_lnt, 2];

   int<lower=0> f_gc_lnt;
   int<lower=0> f_gc[f_gc_lnt, 2];
   
   int<lower=0> f_gcd_lnt;
   int<lower=0> f_gcd[f_gcd_lnt, 2];
   
   // MEN
   int<lower=0> m_ct_lnt;
   int<lower=0> m_ct[m_ct_lnt, 2];
   
   int<lower=0> m_ctd_lnt;
   int<lower=0> m_ctd[m_ctd_lnt, 2];

   int<lower=0> m_gc_lnt;
   int<lower=0> m_gc[m_gc_lnt, 2];
   
   int<lower=0> m_gcd_lnt;
   int<lower=0> m_gcd[m_gcd_lnt, 2];
   
  // CT
  // int<lower = 0> CD[age, state];
   int Cnum_f[age_st, state];
   int Cden_f[age_st, state];
   
   int CDnum_f[age_st, state];
   int CDden_f[age_st, state];
   
   int Cnum_m[age_st, state];
   int Cden_m[age_st, state];
   
   int CDnum_m[age_st, state];
   int CDden_m[age_st, state];
   
  // GC
   int Gnum_f[age_st, state]; 
   int Gden_f[age_st, state];
   
   int GDnum_f[age_st, state];
   int GDden_f[age_st, state];
   
   int Gnum_m[age_st, state];
   int Gden_m[age_st, state];
   
   int GDnum_m[age_st, state];
   int GDden_m[age_st, state];
}

parameters {

  real<lower = 0, upper = 1> prsympt_ct; //pr symptoms CT
  real<lower = 0, upper = 1> prsympt_gc; //pr symptoms  GC

  real<lower = 0> test_sympt_ct; //rate of testing if symptomatic CT
  real<lower = 0> test_sympt_gc; //rate of testing if symptomatic GT

  real<lower = 0, upper = 1> test_asympt_ctgc0[state]; //rate of testing if Asymptomatic CT or GC
  real<lower = 0, upper = 1> test_asympt_RR[age_st-1];
  
  real<lower = 0> test_inf_RR0[age_st]; // RR of screening if infected vs not

  real<lower = 0> clear_ct; //rate of clearance CT
  real<lower = 0> clear_gc; //rate of clearance GC
  // 
  real<lower=0, upper=1> i_ct_f0[state]; // incidence est
  real<lower=0, upper=1> i_gc_f0[state];
  
  real<lower=0, upper=1> i_ct_f_RR[age_st-1]; // incidence est
  real<lower=0, upper=1> i_gc_f_RR[age_st-1];

}

transformed parameters {

  // NATIONAL
  real<lower = 0, upper = 1> te[age_st] ;   
  real<lower = 0> ct[age_st];
  real<lower = 0> gc[age_st];
  real<lower = 0> test_inf_RR[age_st]; // RR of screening if infected vs not
  
  // STATE
  real<lower = 0> ct_st[age_st, state];
  real<lower = 0> gc_st[age_st, state];
  
  real<lower = 0, upper = 1> test_asympt_ctgc[age_st, state] ;
  
  real<lower = 0, upper = 1> p_ct_f[age_st, state] ;
  real<lower = 0, upper = 1> p_gc_f[age_st, state] ;
  
  real<lower=0, upper=1> i_ct_f[age_st, state] ;
  real<lower=0, upper=1> i_gc_f[age_st, state] ;
  
  real<lower=0> dur_ct[age_st, state];
  real<lower=0> dur_gc[age_st, state];

  real<lower = 0, upper = 1> q_ct_f[age_st, state] ;
  real<lower = 0, upper = 1> q_gc_f[age_st, state] ;
  //   
   real<lower = 0, upper = 1> d_ct_f[age_st, state] ; 
   real<lower = 0, upper = 1> d_gc_f[age_st, state] ; 
  // 
  real<lower = 0> num_test_ct_f[age_st, state];
  real<lower = 0> num_test_gc_f[age_st, state];

   real<lower=0, upper=1> pr_det_ct_f[age_st, state] ; // probability of detection (CT)
   real<lower=0, upper=1> pr_det_gc_f[age_st, state] ; // probability of detection (GC)

   ///////////
 for (a in 1:age_st){
   test_inf_RR[a] = 1+test_inf_RR0[a];
 }
    
 for (st in 1:state){

     test_asympt_ctgc[1,st] = test_asympt_ctgc0[st];
     test_asympt_ctgc[2,st] = test_asympt_ctgc[1,st] * test_asympt_RR[1];
     test_asympt_ctgc[3,st] = test_asympt_ctgc[2,st] * test_asympt_RR[2];
     test_asympt_ctgc[4,st] = test_asympt_ctgc[3,st] * test_asympt_RR[3];
     test_asympt_ctgc[5,st] = test_asympt_ctgc[4,st] * test_asympt_RR[4];
     
     i_ct_f[1,st] = i_ct_f0[st];
     i_ct_f[2,st] = i_ct_f[1,st] * i_ct_f_RR[1];
     i_ct_f[3,st] = i_ct_f[2,st] * i_ct_f_RR[2];
     i_ct_f[4,st] = i_ct_f[3,st] * i_ct_f_RR[3];
     i_ct_f[5,st] = i_ct_f[4,st] * i_ct_f_RR[4];
     
     i_gc_f[1,st] = i_gc_f0[st];
     i_gc_f[2,st] = i_gc_f[1,st] * i_gc_f_RR[1];
     i_gc_f[3,st] = i_gc_f[2,st] * i_gc_f_RR[2];
     i_gc_f[4,st] = i_gc_f[3,st] * i_gc_f_RR[3];
     i_gc_f[5,st] = i_gc_f[4,st] * i_gc_f_RR[4];
    
     for (a in 1:age_st){

     pr_det_ct_f[a,st] =  (prsympt_ct*test_sympt_ct+(1-prsympt_ct)*test_inf_RR[a]*test_asympt_ctgc[a,st])/(prsympt_ct*test_sympt_ct+(1-prsympt_ct)*test_inf_RR[a]*test_asympt_ctgc[a,st]+clear_ct);
     pr_det_gc_f[a,st] =  (prsympt_gc*test_sympt_gc+(1-prsympt_gc)*test_inf_RR[a]*test_asympt_ctgc[a,st])/(prsympt_gc*test_sympt_gc+(1-prsympt_gc)*test_inf_RR[a]*test_asympt_ctgc[a,st]+clear_gc);

    }
 }


 for (st in 1:state){
       for (a in 1:age_st){   
 
   d_ct_f[a,st]  = i_ct_f[a,st]*pr_det_ct_f[a,st]; 
   d_gc_f[a,st]  = i_gc_f[a,st]*pr_det_gc_f[a,st]; 
  
   dur_ct[a,st] = (prsympt_ct/(test_sympt_ct+clear_ct))+(1-prsympt_ct)/(test_inf_RR[a]*test_asympt_ctgc[a,st]+clear_ct);
   dur_gc[a,st] = (prsympt_ct/(test_sympt_ct+clear_ct))+(1-prsympt_ct)/(test_inf_RR[a]*test_asympt_ctgc[a,st]+clear_ct);

   p_ct_f[a,st] = i_ct_f[a,st]*dur_ct[a,st];
   p_gc_f[a,st] = i_gc_f[a,st]*dur_gc[a,st];
   
   num_test_ct_f[a,st] = (pop_f[a,st]-pop_f[a,st]*p_ct_f[a,st]) *test_asympt_ctgc[a,st] + d_ct_f[a,st] ;
   num_test_gc_f[a,st] = (pop_f[a,st]-pop_f[a,st]*p_gc_f[a,st]) *test_asympt_ctgc[a,st] + d_gc_f[a,st];

   q_ct_f[a,st]  = pop_f[a,st]*d_ct_f[a,st]/num_test_ct_f[a,st];
   q_gc_f[a,st]  = pop_f[a,st]*d_gc_f[a,st]/num_test_gc_f[a,st];

  }
}
  
  // NATIONAL
 //  monthly rate of testing
  // prevalence at national level

 for (a in 1:age_st){
   for (st in 1:state){

      ct_st[a,st]  = p_ct_f[a,st]*pop_f[a,st];
      gc_st[a,st]  = p_gc_f[a,st]*pop_f[a,st];

   }
      ct[a]  = sum(ct_st[a,])/sum(pop_f[a,]);
      gc[a]  = sum(gc_st[a,])/sum(pop_f[a,]);
}

   for (a in 1:age_st){
    te[a]  =  sum(num_test_ct_f[a,]) /sum(pop_f[a,]) ;
    }

   //  print(RR_m);
}

model {

 for (st in 1:state){  
     i_ct_f0[st] ~beta(1, 10);  
     i_gc_f0[st] ~beta(1, 10); 
 }
 
  for (i in 1:4){   
     i_ct_f_RR[i] ~beta(2, 2);
     i_gc_f_RR[i] ~beta(2, 2);
  }
  
  for (a in 1:age_st){ 
  test_inf_RR0[a] ~gamma(2.712,1.989);
  }
   
 clear_ct         ~gamma(2598.217, 2831.49);
 clear_gc         ~gamma(266.468,  66.484);
 prsympt_ct       ~beta(430, 1260);  // 2x the original to make this prior more informative
 prsympt_gc       ~beta(136, 293);   // 2x the original to make this prior more informative
 test_sympt_ct    ~gamma(234,32);    // 2x the original to make this prior more informative
 test_sympt_gc    ~gamma(425,  37);  // 2x the original to make this prior more informative
 
 // clear_ct         ~gamma(1732.145, 1887.660);
 // clear_gc         ~gamma(133.234,  33.242);
 // prsympt_ct       ~beta(215.904, 630.299);
 // prsympt_gc       ~beta(67.886, 146.503);
 // test_sympt_ct    ~gamma(117.858,  15.899 );
 // test_sympt_gc    ~gamma(212.582 , 18.485 );

  for (st in 1:state){
    test_asympt_ctgc0[st] ~beta(6.398, 8.722 );
  }
  
  for (i in 1:4){
    test_asympt_RR[i] ~beta(2, 2);
  }

  // Likelihood for NHANES data
  for  (j in 1:2){
   target += 0.1*binomial_lpmf(CT[j,1] | CTN[j,1], ct[j]); // + binomial_lpmf(CTm[j,i] | CTNm[j,i], ct[2,j,i]);
    }
    
 for (a in 1:age_st) {
    if ( 5*gc[a]  > ct[a])
      // Penalty: large negative value if 5* GC prevalence in any age group is larger than CT prevalence
      target += -100;
  }
  
//  for (st in 1:state) {
//      if (pr_det_ct_f[1,st]  < 0.25)
//       // Penalty: large negative value if prob detected < 25% // based on symptomatic
//       target += -1000;
//  }
//  
// for (st in 1:state) {
//      if (pr_det_ct_f[1,st]  < 0.3)
//       // Penalty: large negative value if prob detected < 30% // based on symptomatic
//       target += -1000;
//  }



    for  (j in 1:2){
    //target += binomial_lpmf(Te[j,i2-11] | TN[j,i2-11], te[j,i2]); // + binomial_lpmf(Tem[j,i2-11] | TNm[j,i2-11], tem[j,i2]);
    target += 0.2* binomial_lpmf(Te[j,1] | TN[j,1], te[j]);
    }
    
 
    //target += binomial_lpmf(Te[j,i2-11] | TN[j,i2-11], te[j,i2]); // + binomial_lpmf(Tem[j,i2-11] | TNm[j,i2-11], tem[j,i2]);
    target += 0.2* binomial_lpmf(Te_hedis | TN_hedis, te[1]);


 //  
  for (i in 1:f_ct_lnt){
    target += 0.1* binomial_lpmf(Cnum_f[f_ct[i,1],f_ct[i,2]] | Cden_f[f_ct[i,1],f_ct[i,2]], q_ct_f[f_ct[i,1],f_ct[i,2]]);

 }
 
    for (i in 1:f_ctd_lnt){
  //   CDnum_f[f_ctd[i,1],f_ctd[i,2]] ~ binomial(CDden_f[f_ctd[i,1],f_ctd[i,2]], d_ct_f[f_ctd[i,1],f_ctd[i,2]]); 
    target += 0.1* binomial_lpmf( CDnum_f[f_ctd[i,1],f_ctd[i,2]]  | CDden_f[f_ctd[i,1],f_ctd[i,2]], d_ct_f[f_ctd[i,1],f_ctd[i,2]]);

 }
 
   for (i in 1:f_gc_lnt){
    //  Gnum_f[f_gc[i,1],f_gc[i,2]] ~  binomial(Gden_f[f_gc[i,1],f_gc[i,2]], q_gc_f[f_gc[i,1],f_gc[i,2]]);
   target += 0.1* binomial_lpmf( Gnum_f[f_gc[i,1],f_gc[i,2]]  | Gden_f[f_gc[i,1],f_gc[i,2]], q_gc_f[f_gc[i,1],f_gc[i,2]]);
 }
 // 
    for (i in 1:f_gcd_lnt){
    //  GDnum_f[f_gcd[i,1],f_gcd[i,2]] ~  binomial(GDden_f[f_gcd[i,1],f_gcd[i,2]], d_gc_f[f_gcd[i,1],f_gcd[i,2]]);
   target += 0.1* binomial_lpmf( GDnum_f[f_gcd[i,1],f_gcd[i,2]]  |GDden_f[f_gcd[i,1],f_gcd[i,2]], d_gc_f[f_gcd[i,1],f_gcd[i,2]]);
 }
 
 //   for (i in 1:m_ct_lnt){
 //      Cnum_m[m_ct[i,1],m_ct[i,2],m_ct[i,3]] ~ binomial(Cden_m[m_ct[i,1],m_ct[i,2],m_ct[i,3]], q_ct_m[m_ct[i,1],m_ct[i,2],m_ct[i,3]]); 
 // }
 // 
 //    for (i in 1:m_ctd_lnt){
 //      CDnum_m[m_ctd[i,1],m_ctd[i,2],m_ctd[i,3]] ~ binomial(CDden_m[m_ctd[i,1],m_ctd[i,2],m_ctd[i,3]], d_ct_m[m_ctd[i,1],m_ctd[i,2],m_ctd[i,3]]); 
 // }
 
 //   for (i in 1:m_gc_lnt){
 //      Gnum_m[m_gc[i,1],m_gc[i,2],m_gc[i,3]] ~ binomial(Gden_m[m_gc[i,1],m_gc[i,2],m_gc[i,3]], q_gc_m[m_gc[i,1],m_gc[i,2],m_gc[i,3]]); 
 // }
 // 
 //    for (i in 1:m_gcd_lnt){
 //      GDnum_m[m_gcd[i,1],m_gcd[i,2],m_gcd[i,3]] ~ binomial(GDden_m[m_gcd[i,1],m_gcd[i,2],m_gcd[i,3]], d_gc_m[m_gcd[i,1],m_gcd[i,2],m_gcd[i,3]]); 
 // }
  
}

  generated quantities {
 //  
  real<lower = 0> diag_ct[age_st];
  real<lower = 0> diag_gc[age_st];
  real<lower = 0> num_test_ct[state]; 
  real<lower = 0> num_test_gc[state]; 

 // 
  for (a in 1:age_st){
    diag_ct[a]  = sum(d_ct_f[a,]); 
    diag_gc[a]  = sum(d_gc_f[a,]);
  }

  for (st in 1:state){
    num_test_ct[st] =  sum(num_test_ct_f[,st]);
    num_test_gc[st] =  sum(num_test_gc_f[,st]);
  }
 
  }
