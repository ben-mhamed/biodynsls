// Biomass dynamic model for short-lived species

#include<TMB.hpp>
//#include<math.hpp>
template<class Type>
Type objective_function<Type>::operator()()
{
  // Data:
  DATA_VECTOR(C);
  DATA_VECTOR(LOG_CPUE);

  // Parameters
  PARAMETER(LOG_r); //
  PARAMETER(LOG_K);
  PARAMETER(LOG_q);
  PARAMETER(LOG_b);
  PARAMETER(LOG_sigma_proc);
  PARAMETER(LOG_sigma_obs);
  PARAMETER_VECTOR(LOG_B);
  // Procedures: (transformed parameters)
  Type r = exp(LOG_r);
  Type K = exp(LOG_K);
  Type q = exp(LOG_q);
  Type b = exp(LOG_b);
  Type sigma_proc = exp(LOG_sigma_proc);
  Type sigma_obs = exp(LOG_sigma_obs);
  Type B = exp(LOG_B);
  // Reports on transformed parameters
  ADREPORT(sigma_proc);
  ADREPORT(sigma_obs);

  int n = C.size(); // get the time series length
  Type nll = 0.0; // initialize the negative log likelihood

  // Process model
  LOG_B[0] = LOG_b+LOG_K;
  for(int i=1;i<n;i++){
     Type m = r*B[i-1]*(1-(B[i-1]/K) - C[i-1]); // Annual version
     Type LOG_m = log(m);
     nll-= dnorm(LOG_B[i],LOG_m,sigma_proc,true);
  }

  // Observation model
  for(int i=0;i<n;i++){
    Type Ipred = LOG_q + LOG_B[i]
    nll-= dnorm(LOG_CPUE[i],Ipred,sigma_obs,true);
  }

  return nll;
}

