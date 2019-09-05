data {
  int<lower=0> N;                 // total number of observations
  int<lower=0> M;                 // number of days
  vector<lower=0>[N] b;           // travel times
  vector<lower=0,upper=24>[N] t;  // observation times
  int<lower=1,upper=7> d[N];      // day of the week (1=monday, ..., 7=sunday)
}
transformed data {
  real center = mean(b);
  vector[N] b_center = b - center;
  int<lower=0,upper=1> is_weekday[N];
  for (n in 1:N) {
    is_weekday[n] = d[n] < 6;
  }
}
parameters {
  real<lower=-center> mu;

  vector<lower=0,upper=1>[2] pi;

  vector<lower=6,upper=20>[2] mu_tau;
  real<lower=0,upper=2> sigma_tau;
  matrix<lower=6,upper=20>[2,M] tau;

  vector<lower=0.1,upper=3>[2] omega;

  vector<lower=50,upper=10000>[2] mu_rho;
  real <lower=0,upper=1000> sigma_rho;
  matrix<lower=0,upper=10000>[2,M] rho;

  real<lower=0> err;
}
transformed parameters {
  vector[4] lp;
  for (p1 in 0:1) {
    for (p2 in 0:1) {
      lp[p1 + 2 * p2 + 1] =
        bernoulli_lpmf(p1 | pi[1]) + bernoulli_lpmf(p2 | pi[2]);

      for (n in 1:N) {
        real eta;
        eta = mu;
        if (p1) {
          eta += is_weekday[n] * rho[1,d[n]] *
              exp(-pow(t[n] - tau[1,d[n]], 2) * 0.5 * pow(omega[1], -2));
        }
        if (p2) {
          eta += is_weekday[n] * rho[2,d[n]] *
              exp(-pow(t[n] - tau[2,d[n]], 2) * 0.5 * pow(omega[2], -2));
        }
        lp[p1 + 2 * p2 + 1] += normal_lpdf(b_center[n] | eta, err);
      }
    }
  }
}
model {
  mu ~ normal(0, 100);
  pi ~ beta(0.001, 0.001);

  mu_tau[1] ~ uniform(6, 10);
  mu_tau[2] ~ uniform(14, 20);
  sigma_tau ~ gamma(0.1, 0.1);

  mu_rho ~ normal(100, 100);
  sigma_rho ~ gamma(0.1, 0.1);

  for (j in 1:2) {
    for (m in 1:M) {
      tau[j,m] ~ normal(mu_tau[j], sigma_tau);
      rho[j,m] ~ normal(mu_rho[j], sigma_rho);
    }
  }

  target += log_sum_exp(lp);
}
