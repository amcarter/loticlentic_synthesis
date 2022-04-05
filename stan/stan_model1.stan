// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size
 vector[N] res_time; // Predictor1
 vector[N] kO2; // Predictor2
 vector[N] DOpre;// intercept
 vector[N] DO; // Outcome
}

parameters {
 real alpha; // Intercept
 real beta1; // Slope1 (regression coefficients)
 real beta2; // Slope2 (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
 DO ~ normal(alpha + DOpre + res_time * beta1 + kO2 * beta2, sigma);
}

generated quantities {
} // The posterior predictive distribution
