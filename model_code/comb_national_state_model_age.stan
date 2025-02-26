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
   
   int<lower = 0> Tem[2, N2];
   int<lower = 0> TNm[2, N2];   

     // STATE
  int<lower=0> yr;
   int<lower=0> age_st;
   int<lower=0> state;
   
   //populattion
   int<lower=0> pop_f[yr, age_st, state];
   int<lower=0> pop_m[yr, age_st, state];
   
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
  // real<lower=0, upper=1> national_tr[N];
 //  real<lower=0> national_RR[N];
  real<lower=0, upper=1> national_tr_2010_2014; 
 // real<lower=0, upper=1> national_tr_2015_2020; 
 // real<lower=0, upper=1> national_tr_2021_2023; 
// state  
   real<lower = 0> RR_f0[state];
   // real<lower = 0> RR_m0[state];
   
   real<lower = 0, upper = 1> tr_f0[state];
   // real<lower = 0, upper = 1> tr_m0[state];
   
   real<lower = 0> RR_age[age_st];
   real<lower = 0> tr_age[age_st];
   
   real<lower = 0, upper=1> p_age[age_st-1];
   
  real<lower = 0, upper = 1> p_ct_f0[state] ;
  real<lower = 0, upper = 1> p_gc_f0[state] ; 
   // real<lower = 0, upper = 1> p_ct_m[yr,age_st, state] ;
   
  
   // real<lower = 0, upper = 1> p_gc_m[yr,age_st, state] ; 
}

transformed parameters {

  // NATIONAL
  matrix<lower = 0, upper = 1>[age_st, N] te ;  
 // matrix<lower = 0, upper = 1>[age_st, N] tem ; 
  real<lower = 0, upper = 1> ct[2, age_st, N];   
  real<lower = 0, upper = 1> gc[2, age_st, N];   
  real<lower = 0> ct_st[2, age_st, state];
  real<lower = 0> gc_st[2, age_st, state];

  // STATE 
  real<lower = 0, upper = 1> p_ct_f[age_st, state] ;
  real<lower = 0, upper = 1> p_gc_f[age_st, state] ; 
   
  real<lower = 0, upper = 1> q_ct_f[yr, age_st, state] ; 
  real<lower = 0, upper = 1> d_ct_f[yr, age_st, state] ; 
  real<lower = 0, upper = 1> q_gc_f[yr, age_st, state] ; 
  real<lower = 0, upper = 1> d_gc_f[yr, age_st, state] ; 
  real<lower = 0, upper = 1> tr_f[yr, age_st, state] ; 
  // real<lower = 0, upper = 1> tr_m[yr, age_st, state] ; 
  real<lower = 0> RR_f[yr, age_st, state] ; 
  // real<lower = 0> RR_m[yr, age_st, state] ; 
  real<lower = 0> sum_tested[2,age_st, state];
  // real<lower = 0, upper = 1> q_ct_m[yr, age_st, state] ; 
  // real<lower = 0, upper = 1> d_ct_m[yr, age_st, state] ; 
  // real<lower = 0, upper = 1> q_gc_m[yr, age_st, state] ; 
  // real<lower = 0, upper = 1> d_gc_m[yr, age_st, state] ; 
   real national_tr[N];  // This will hold the transformed parameters for each year

  for (i in 1:N) {
    if (years[i] >= 2010 && years[i] <= 2014) {
      national_tr[i] = national_tr_2010_2014;
    } else if (years[i] >= 2015 && years[i] <= 2020) {
      national_tr[i] = national_tr_2010_2014;//national_tr_2015_2020;
    } else if (years[i] >= 2021 && years[i] <= 2023) {
      national_tr[i] = national_tr_2010_2014;//national_tr_2021_2023;
    } else {
      national_tr[i] = -999;  
    }
  }

    // STATE
    for (y in 1:N){
      for (st in 1:state){
          for (a in 1:age_st){
           RR_f[y,a,st] = (1+RR_f0[st])*RR_age[a];  // national_RR[y]*
          // RR_m[y,a,st] =  national_tr[y]*RR_m0[st]*RR_age[a];
           
           
          // tr_m[y,a,st] = /national_tr[y]*tr_m0[st]*tr_age[a];
        }
         tr_f[y,1,st] = national_tr[y]*tr_f0[st]*tr_age[1];
         tr_f[y,2,st] = national_tr[y]*tr_f0[st]*tr_age[1]*tr_age[2];
         tr_f[y,3,st] = national_tr[y]*tr_f0[st]*tr_age[1]*tr_age[3];
         tr_f[y,4,st] = national_tr[y]*tr_f0[st]*tr_age[1]*tr_age[4];
         tr_f[y,5,st] = national_tr[y]*tr_f0[st]*tr_age[1]*tr_age[5];
      }
    }

     for (st in 1:state){ 
           p_ct_f[1,st] = p_ct_f0[st];  // national_RR[y]*
           p_gc_f[1,st] = p_gc_f0[st];       
           
           p_ct_f[2,st] = p_age[1]* p_ct_f[1,st];  // national_RR[y]*
           p_gc_f[2,st] = p_age[1]* p_gc_f[1,st]; 
           
           p_ct_f[3,st] = p_age[2]* p_ct_f[2,st];  // national_RR[y]*
           p_gc_f[3,st] = p_age[2]* p_gc_f[2,st]; 
           
           p_ct_f[4,st] = p_age[3]* p_ct_f[3,st];  // national_RR[y]*
           p_gc_f[4,st] = p_age[3]* p_gc_f[3,st];

           p_ct_f[5,st] = p_age[4]* p_ct_f[4,st];  // national_RR[y]*
           p_gc_f[5,st] = p_age[4]* p_gc_f[4,st];
        }

         for (st in 1:state){
           for (y in 1:yr){
               for (a in 1:age_st){   
     
       q_ct_f[y,a,st]  = p_ct_f[a,st]*RR_f[y,a,st]; 
       d_ct_f[y,a,st]  = p_ct_f[a,st]*tr_f[y,a,st]; 
       
       q_gc_f[y,a,st]  = p_gc_f[a,st]*RR_f[y,a,st];
       d_gc_f[y,a,st]  = p_gc_f[a,st]*tr_f[y,a,st];
       
       // q_ct_m[y,a,st]  = p_ct_m[y,a,st]*RR_m[y,a,st]; 
       // d_ct_m[y,a,st]  = p_ct_m[y,a,st]*tr_m[y,a,st]; 
       // 
       // q_gc_m[y,a,st]  = p_gc_m[y,a,st]*RR_m[y,a,st]; 
       // d_gc_m[y,a,st]  = p_gc_m[y,a,st]*tr_m[y,a,st];  
        }
      }
    }
  
  
  // NATIONAL
 //  monthly rate of testing
  // prevalence at national level
// for (s in 1:2){
  

   for (y in 1:N){
     for (a in 1:age_st){
       for (st in 1:state){  
           
            ct_st[1,a,st]  = p_ct_f[a,st]*pop_f[y,a,st]; 
            ct_st[2,a,st]  = 0.5; //p_ct_m[y,a,st]*pop_m[y,a,st]; 
         
           gc_st[1,a,st]  = p_gc_f[a,st]*pop_f[y,a,st];
           gc_st[2,a,st]  = 0.5; //p_gc_m[y,a,st]*pop_m[y,a,st];
       }
        ct[1,a,y]  = sum(ct_st[1,a,])/sum(pop_f[y,a,]); 
        ct[2,a,y]  = 0.5; //sum(ct_st[2,a,])/sum(pop_m[y,a,]); 
             
        gc[1,a,y]  = sum(gc_st[1,a,])/sum(pop_f[y,a,]);
        gc[2,a,y]  = 0.5; //sum(gc_st[2,a,])/sum(pop_m[y,a,]);
     }
    }
// } 

  for (y in 1:N){
   for (a in 1:age_st){ 
     for (st in 1:state){  
    sum_tested[1,a,st] = pop_f[y,a,st]* (ct[1,a,y]*tr_f[y,a,st]  + (1-ct[1,a,y])*tr_f[y,a,st]/RR_f[y,age_st,st] ) ;
    sum_tested[2,a,st] = 0.5;//pop_m[y,a,st]* (ct[2,a,y]*tr_m[y,a,st]  + (1-ct[2,a,y])* tr_m[y,a,st]/RR_m[y,age_st,st]) ;
    }
    te[a,y]  =  sum(sum_tested[1,a,]) /sum(pop_f[y,a,]) ;
    //tem[a,y] =  sum(sum_tested[2,a,]) /sum(pop_m[y,a,]) ;
    }
   }

   //  print(ct[2,1,1]);
   // // print( pop_f[1,1,1]);
   //  print(tr_m);
   //  print(RR_m);
}

model {

  // NATIONAL
 //  national_tr[1] ~ beta(2, 2); 
 // // national_RR[1] ~ gamma(65, 30); 
 // 
 //  for (i in 2:N) {
 //    national_tr[i] ~ normal(national_tr[i-1], 0.0001); // Random walk
 //  //  national_RR[i] ~ normal(national_RR[i-1], 0.0001); // Random walk
 //  }
  national_tr_2010_2014 ~ beta(2, 6);
 // national_tr_2015_2020 ~ beta(2, 6);
 // national_tr_2021_2023 ~ beta(2, 6);


//for (i in 1:(N-7)){
    for (j in 1:2){
  // Likelihood for NHANES data
   target += binomial_lpmf(CT[j,1] | CTN[j,1], ct[1,j,1]); // + binomial_lpmf(CTm[j,i] | CTNm[j,i], ct[2,j,i]); 
    }
 // }
  
  // Likelihood for NSFG
//  for (i2 in 12:(N-5)){
    for  (j in 1:2){
    //target += binomial_lpmf(Te[j,i2-11] | TN[j,i2-11], te[j,i2]); // + binomial_lpmf(Tem[j,i2-11] | TNm[j,i2-11], tem[j,i2]);
    target += binomial_lpmf(Te[j,1] | TN[j,1], te[j,1]);
    }
 // }
  
  // STATE
    for (st in 1:state){  
      RR_f0[st] ~gamma(30, 30);
      tr_f0[st] ~gamma(30, 30);
      
      // RR_m0[st] ~gamma(30, 30);
      // tr_m0[st] ~gamma(30, 30);
    }
   
   for (a in 1:age_st){   
     RR_age[a] ~gamma(5, 5);
    }
  
 tr_age[1] ~gamma(5, 2); 
 tr_age[2] ~beta(2, 2); 
 tr_age[3] ~beta(2, 2); 
 tr_age[4] ~beta(2, 2); 
 tr_age[5] ~beta(2, 2); 

 
   //  for (a in 1:age_st){ 
   //     for (st in 1:state){ 
          
   //         p_ct_f0[st] ~beta(1, 10);
   //         p_gc_f0[st] ~beta(1, 10);
        //    p_ct_m[i,a,st] ~beta(1, 10);
        //    p_gc_m[i,a,st] ~beta(1, 10);
   //   }
  //   }
     
  // for (i in 2:N){ 
  //       for (st in 1:state){ 
  //   p_ct_f[i,1,st] ~ normal(p_ct_f[i-1,1,st], 0.001);
  //   p_gc_f[i,1,st] ~ normal(p_gc_f[i-1,1,st], 0.001);
  //       }
  //     }
    
   p_age[1] ~beta(20,1);
   p_age[2] ~beta(20,1);
   p_age[3] ~beta(20,1);
   p_age[4] ~beta(20,1);

  for (i in 1:f_ct_lnt){
      Cnum_f[f_ct[i,1],f_ct[i,2],f_ct[i,3]] ~ binomial(Cden_f[f_ct[i,1],f_ct[i,2],f_ct[i,3]], q_ct_f[f_ct[i,1],f_ct[i,2],f_ct[i,3]]); 
 }
 
    for (i in 1:f_ctd_lnt){
      CDnum_f[f_ctd[i,1],f_ctd[i,2],f_ctd[i,3]] ~ binomial(CDden_f[f_ctd[i,1],f_ctd[i,2],f_ctd[i,3]], d_ct_f[f_ctd[i,1],f_ctd[i,2],f_ctd[i,3]]); 
 }
 
   for (i in 1:f_gc_lnt){
      Gnum_f[f_gc[i,1],f_gc[i,2],f_gc[i,3]] ~ binomial(Gden_f[f_gc[i,1],f_gc[i,2],f_gc[i,3]], q_gc_f[f_gc[i,1],f_gc[i,2],f_gc[i,3]]);
 }
 // 
    for (i in 1:f_gcd_lnt){
      GDnum_f[f_gcd[i,1],f_gcd[i,2],f_gcd[i,3]] ~ binomial(GDden_f[f_gcd[i,1],f_gcd[i,2],f_gcd[i,3]], d_gc_f[f_gcd[i,1],f_gcd[i,2],f_gcd[i,3]]);
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
     // real<lower = 0> p_gc_f[N,age_st, state];
     // real<lower = 0> q_gc_f[N,age_st, state];
 //    matrix [2, N] tm_rate_calc; 
 //    matrix [2, N] d_t_calc;
    matrix [age_st, state] test_cov; 
    
    for (a in 1:5){
      for (st in 1:state){
        test_cov[a,st] = sum_tested[1,a,st]/pop_f[1,a,st];
      }
    }
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
  //  for (y in 1:N){
  //   for (a in 1:age_st){
  //      for (st in 1:state){
  //      p_gc_f[y,a,st] = (GDnum_f[y,a,st] /GDden_f[y,a,st])/tr_f[y,a,st] ;
  //      q_gc_f[y,a,st]  = p_gc_f[y,a,st]*RR_f[y,a,st];
  //   }
  //  }
  // }

 
  }
