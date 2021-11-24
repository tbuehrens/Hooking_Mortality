data{
  int n;//number of observations (fish)
  int rec[n]; //bernoulli variable indicating if an individual recovered
  int TC[n]; //dummy variable indicating if an individual is a T or C
}
parameters{
  real<lower=0,upper=1> p[2];
}
transformed parameters{
  //logit(p[i]) =
}
model{
  //priors
  p ~ beta(1,1);
  //likelihoods
  for(i in 1:n){
    if(TC[i]==1){
      rec[i] ~ bernoulli(p[1]);
    }else{
      rec[i] ~ bernoulli(p[1]*p[2]);
    }
  }
}
