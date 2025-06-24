// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size MUST SET THIS EACH TIME FOR EACH VARIABLE
 int < lower = 1 > K; // Number of temperature obs
 matrix[K, N] x1; // Predictor - [SAMPLE SIZE]
 vector[N] y; // Outcome
 vector[N] x2; // new temperature
}

parameters {
 real beta0; // Intercept
 real beta1; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
 real  open; // open of window
 real < lower = open > close; // close of the window
}



model {

// Binary search function

int bin_search(real x, int min_val, int max_val){
  // This assumes that min_val >= 0 is the minimum integer in range, 
  //  max_val > min_val,
  // and that x has already been rounded. 
  //  It should find the integer equivalent to x.
  int range = (max_val - min_val+1)/2; // We add 1 to make sure that truncation doesn't exclude a number
  int mid_pt = min_val + range;
  int out;
  while(range > 0) {
    if(x == mid_pt){
      out = mid_pt;
      range = 0;
    } else {
      // figure out if range == 1
      range =  (range+1)/2; 
      mid_pt = x > mid_pt ? mid_pt + range: mid_pt - range; 
      }
  }
  return out;
}

int open2 = bin_search(open, 1, 100)

// specify window
 
 for(i in 1:N){
 x2 = mean(x1[round(open2):(round(close2)+1),i]);
 } 
  
}
 
 // specify prior
 beta0 ~ normal(0, 100);
 beta1 ~ normal(0, 100);
 
 //specify data likelihood
 
 y ~ normal(beta0 + x2 * beta1 , sigma);
}

generated quantities {
} // The posterior predictive distribution
