data {
   int<lower=0> year;
   int<lower=0> age;
   int<lower=0> state;
  // CT
   int<lower = 0> Pop[year, age, state]; 
   int<lower = 0> CD[year, age, state];
   int<lower = 0> Cnum[year, age, state];
   int<lower = 0> Cden[year, age, state];
}

parameters {

   real<lower = 0, upper = 1> tr[age, state]; //
   real<lower = 0> clear;   //
   real<lower = 0, upper = 1> i[age, state] ; 
   real<lower = 0, upper = 1> p[age, state] ; 
}

transformed parameters {

  real<lower = 0> q[year, age, state] ; 
  real<lower = 0> d[year, age, state] ; 
   
 for (st in 1:state){
    for (y in 1:year){
      for (a in 1:age){
       
       q[y,a,st]  = p[a,st]*(1 - exp(-tr[a,st])); 
       d[y,a,st]  = i[a,st]*tr[a,st]/(tr[a,st]+clear); 
      }
    }
  }

}

   
model {
  // Prior 
  clear ~lognormal(0.00504, 0.22206);


  for (a in 1:3){ 
    for (st in 1:state){  
      i[a,st] ~beta(1, 30);
      p[a,st] ~beta(1, 100);
      tr[a,st] ~beta(50, 80);
    }
  }
  
  for (a in 4:5){ 
    for (st in 1:state){  
      i[a,st] ~beta(1, 200);
      p[a,st] ~beta(1, 500);
      tr[a,st] ~beta(10, 80);
    }
  }

 for (st in 1:state){
    for (y in 1:year){
      for (a in 1:age){
    CD[y,a,st] ~ binomial(Pop[y,a,st], d[y,a,st]);
    Cnum[y,a,st] ~ binomial(Cden[y,a,st], q[y,a,st]); 
      }
    }
  }
  
}

 generated quantities {
  
 real<lower = 0, upper = 1> calc_propi_tested[age, state]; //

  for (a in 1:age){
    for (st in 1:state){ 
        calc_propi_tested[a, st] =  tr[a,st]/(tr[a,st]+clear); 
    }
  }

}
