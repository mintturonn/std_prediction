data {
   int<lower=0> yr;
   int<lower=0> age;
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
   int Cnum_f[yr, age, state];
   int Cden_f[yr, age, state];
   
   int CDnum_f[yr, age, state];
   int CDden_f[yr, age, state];
   
   int Cnum_m[yr, age, state];
   int Cden_m[yr, age, state];
   
   int CDnum_m[yr, age, state];
   int CDden_m[yr, age, state];
   
  // GC
   int Gnum_f[yr, age, state];
   int Gden_f[yr, age, state];
   
   int GDnum_f[yr, age, state];
   int GDden_f[yr, age, state];
   
   int Gnum_m[yr, age, state];
   int Gden_m[yr, age, state];
   
   int GDnum_m[yr, age, state];
   int GDden_m[yr, age, state];
}

parameters {
   // women
   real<lower = 0> RR_f0[age,state];
   real<lower = 0> RR_f0_rel[age,state];
   real<lower = 0, upper = 1> tr_f0[age,state]; 
   real<lower = 0, upper = 1> tr_f0_rel[age,state]; 
   
   real<lower = 0, upper = 1> p_ct_f[yr,age, state] ; 
   real<lower = 0, upper = 1> p_gc_f[yr,age, state] ; 
   // men
   real<lower = 0> RR_m0[age,state];
   real<lower = 0> RR_m0_rel[age,state];
   real<lower = 0, upper = 1> tr_m0[age,state]; 
   real<lower = 0, upper = 1> tr_m0_rel[age,state]; 
   
   real<lower = 0, upper = 1> p_ct_m[yr,age, state] ; 
   real<lower = 0, upper = 1> p_gc_m[yr,age, state] ; 
}

transformed parameters {
  // women

  real<lower = 0, upper = 1> q_ct_f[yr, age, state] ; 
  real<lower = 0, upper = 1> d_ct_f[yr, age, state] ; 

  real<lower = 0, upper = 1> q_gc_f[yr, age, state] ; 
  real<lower = 0, upper = 1> d_gc_f[yr, age, state] ; 
  
  real<lower = 0, upper = 1> tr_f[yr,age, state]; //
  real<lower = 0> RR_f[yr,age, state] ; 
  // men

  real<lower = 0, upper = 1> q_ct_m[yr, age, state] ; 
  real<lower = 0, upper = 1> d_ct_m[yr, age, state] ; 
  
  real<lower = 0, upper = 1> q_gc_m[yr, age, state] ; 
  real<lower = 0, upper = 1> d_gc_m[yr, age, state] ; 
  
  real<lower = 0, upper = 1> tr_m[yr,age, state]; //
  real<lower = 0> RR_m[yr,age, state] ; 


      for (a in 1:age){
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

      for (a in 1:age){
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

//  generated quantities {
//   
//  // real<lower = 0, upper = 1> calc_propi_tested[age, state]; //
//  // 
//  //  for (a in 1:age){
//  //    for (st in 1:state){ 
//  //        calc_propi_tested[a, st] =  tr[a,st]/(tr[a,st]+clear); 
//  //    }
//  //  }
// 
// }
