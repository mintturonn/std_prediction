data {
   int<lower=0> yr;
   int<lower=0> age;
   int<lower=0> state;
   
   // WOMEN
   int<lower=0> f_ct_lnt;
   int<lower=0> f_ct[f_ct_lnt, yr];
   
   int<lower=0> f_ctd_lnt;
   int<lower=0> f_ctd[f_ctd_lnt, yr];

   int<lower=0> f_gc_lnt;
   int<lower=0> f_gc[f_gc_lnt, yr];
   
   int<lower=0> f_gcd_lnt;
   int<lower=0> f_gcd[f_gcd_lnt, yr];
   
   // MEN
   int<lower=0> m_ct_lnt;
   int<lower=0> m_ct[m_ct_lnt, yr];
   
   int<lower=0> m_ctd_lnt;
   int<lower=0> m_ctd[m_ctd_lnt, yr];

   int<lower=0> m_gc_lnt;
   int<lower=0> m_gc[m_gc_lnt, yr];
   
   int<lower=0> m_gcd_lnt;
   int<lower=0> m_gcd[m_gcd_lnt, yr];
   
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
   real<lower = 0, upper = 1> tr_f[age, state]; //
   real<lower = 0> RR_f[age, state] ; 
   real<lower = 0, upper = 1> p_ct_f[yr,age, state] ; 
   real<lower = 0, upper = 1> p_gc_f[yr,age, state] ; 
   // men
   real<lower = 0, upper = 1> tr_m[age, state]; //
   real<lower = 0> RR_m[age, state] ; 
   real<lower = 0, upper = 1> p_ct_m[yr,age, state] ; 
   real<lower = 0, upper = 1> p_gc_m[yr,age, state] ; 
}

transformed parameters {
  // women
 // real<lower = 0> RR_f[age, state] ; 
  real<lower = 0, upper = 1> q_ct_f[yr, age, state] ; 
  real<lower = 0, upper = 1> d_ct_f[yr, age, state] ; 

  real<lower = 0, upper = 1> q_gc_f[yr, age, state] ; 
  real<lower = 0, upper = 1> d_gc_f[yr, age, state] ; 
  // men
 // real<lower = 0> RR_m[age, state] ; 
  real<lower = 0, upper = 1> q_ct_m[yr, age, state] ; 
  real<lower = 0, upper = 1> d_ct_m[yr, age, state] ; 
  
  real<lower = 0, upper = 1> q_gc_m[yr, age, state] ; 
  real<lower = 0, upper = 1> d_gc_m[yr, age, state] ; 


      for (a in 1:age){
         for (st in 1:state){
              for (y in 1:yr){  
       q_ct_f[y,a,st]  = p_ct_f[y,a,st]*RR_f[a,st]; 
       d_ct_f[y,a,st]  = p_ct_f[y,a,st]*tr_f[a,st]; 
       
       q_gc_f[y,a,st]  = p_gc_f[y,a,st]*RR_f[a,st]; 
       d_gc_f[y,a,st]  = p_gc_f[y,a,st]*tr_f[a,st];  
       
       q_ct_m[y,a,st]  = p_ct_m[y,a,st]*RR_m[a,st]; 
       d_ct_m[y,a,st]  = p_ct_m[y,a,st]*tr_m[a,st]; 
      
       q_gc_m[y,a,st]  = p_gc_m[y,a,st]*RR_m[a,st]; 
       d_gc_m[y,a,st]  = p_gc_m[y,a,st]*tr_m[a,st];  
        }
      }
    }

}

   
model {

  for (a in 1:yr){ 
    for (st in 1:state){  
      RR_f[a,st] ~gamma(40, 30);
      tr_f[a,st] ~beta(5, 8);
      
      RR_m[a,st] ~gamma(40, 30);
      tr_m[a,st] ~beta(5, 8);
    }
  }
  
  for (a in 4:5){ 
    for (st in 1:state){  
      RR_f[a,st] ~gamma(40, 30);
      tr_f[a,st] ~beta(5, 8);
      
      RR_m[a,st] ~gamma(40, 30);
      tr_m[a,st] ~beta(5, 8);
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
