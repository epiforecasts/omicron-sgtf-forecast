functions {
#include functions/convolve.stan
}

data {
  int t;
  int t_gp;
  int times[t_gp];
  int gps;
  int t_start[gps];
  int t_end[gps];
  int Y[t];
  int N[t];
  int likelihood;
  int output_loglik;
  int relat;
  int overdisp;
  int debug;
}

parameters {
  real i_mean;
  real<lower = 0> i_sd;
  vector<offset = i_mean, multiplier = i_sd>[gps] intercept;
  real<lower = 0> t_grad_sd;
  vector<offset = 0, multiplier = t_grad_sd>[wks] intercept;
  vector<lower = 0>[overdisp ? 1 : 0] sqrt_phi;
}

transformed parameters {
  vector[t_gp] frac[gps];
  vector[t_gp] conv_frac[gps];
  // combine terms for fraction voc per region and day
  for (i in 1:gps) {
    vector[t_gp] local_grad = cumulative_sum(t_grad[i]);
    logit(frac[i]) = intercept[i] + grad * times + local_grad;
  }
  // calculate pmf
  
  // convolve fractions by omicron testing delay
  for (i in 1:gps) {
    conv_frac[i] = convolve(frac[i], pmf);
  }
  // rescale overdispersion
  if (overdisp) {
    phi = 1 ./ sqrt(sqrt_phi);
  }
}

model {
  // intercept per region
  i_mean ~ normal(0, 10);
  i_sd ~ normal(0, 1);
  intercept ~ normal(i_mean, i_sd);

  // global gradient
  grad ~ std_normal();
  
  //region specific random effect on gradient
  t_grad_sd ~ normal(0, 1);
  for (i in 1:gps) {
    to_vector(t_grad[i]) ~  normal(0, t_grad_sd);
  }

  // delay to report
  delay_mean ~ std_normal();
  delay_sd ~ std_normal();
  // observation model priors
  if (overdisp) {
    sqrt_phi ~ std_normal();
  }
  // observation model 
  if (likelihood) {
    for (i in 1:gps) {
      if (overdisp) {
        Y[t_start[i]:t_end[i]] ~ beta_binomial(N[i], conv_frac[i] * phi[1], 
                        (1 - conv_frac[i]) * phi[1]);
      }else{
        Y[t_start[i]:t_end[i]] ~ binomial(N[i], conv_frac[i]);
      }
    }
  }
}

generated quantities {
}