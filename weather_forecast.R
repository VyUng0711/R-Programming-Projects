library(rstan)
#install.packages('lubridate')
library(lubridate)
mauna_loa<- read.csv("/Users/VyUng/Downloads/weekly_in_situ_co2_mlo.csv")
correct_date <- mdy(mauna_loa$Date)
convert_year <- function(x,y){
  m <- year(x) %% 100
  year(x) <- ifelse(m > y %% 100, 1900+m, 2000+m)
  x
}
mauna_loa$Date<-convert_year(correct_date,y=1950)
plot(mauna_loa$Date[0:500],mauna_loa$CO2[0:500],xlab="Date (Year)",ylab="CO2 (ppm)")

start <- as.Date("1958-03-29")
num_date <- as.integer(mauna_loa$Date-start)
normalized_date <- (num_date-min(num_date))/(max(num_date)-min(num_date))
level <- mauna_loa$CO2
normalized_level <- (level-min(level))/(max(level)-min(level))
plot(normalized_date,normalized_level)
n <- length(normalized_date)
x_future <- seq(from=21791+7, to=36946, by=7)
x_future
n_future <- length(x_future)
all_x <- c(num_date,x_future) 
x_future_normalized <- x_future/(max(num_date)-min(num_date))
x_future_normalized
data1 <- list(
  n = n,
  y = normalized_level[1:n],
  x = normalized_date[1:n],
  x_future = x_future_normalized,
  n_future = n_future)
model1 <- "
data{
  int<lower=0> n;
  int<lower=0> n_future;
  real x[n];
  real y[n];
  real x_future[n_future];
}
parameters {
  real c0;
  real c1;
  real c2;
  real c3;
  real c4;
  real c5;
}
transformed parameters {
  real<lower=0, upper=0.1> d0;
  real<lower=0> d1;
  real<lower=0> d2;
  real<lower=0> d3;
  real<lower=0> d4;
  real<lower=0> d5;

  d0 = 0.1/(1+exp(-c0));
  d1 = exp(c1);
  d2 = exp(c2);
  d3 = exp(c3);
  d4 = exp(c4);
  d5 = exp(c5);
}

model {
  c0 ~ normal(0,1);
  c1 ~ normal(0,1);
  c2 ~ normal(0,1);
  c3 ~ normal(0,1);
  c4 ~ normal(0,1);
  c5 ~ normal(-1,1);

  for(t in 1:n) {
    y[t] ~ normal((c0+c1*x[t]+c2*x[t]*x[t]+c3*sin(2*3.1415*x[t]/(365.25/21791))+c4), c5);
  }
}

generated quantities {
  real y_future[n_future];
  for(t in 1:n_future) {
    y_future[t]=normal_rng((c0+c1*x_future[t]+c2*x_future[t]*x_future[t]+c3*sin(2*3.1415*x_future[t]/(365.25/21791))+c4), c5*c5);
  }
}
"
fit <- stan(
  model_code = model,
  data = data,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 1,
  refresh = 1000,
  control = list(adapt_delta=0.999)
)
print(fit, par=c('c0', 'c1', 'c2', 'c3', 'c4','c5'), probs=c(.05, .5, 0.95))
samples <- extract(fit)
x_future
samples$y_future
results <- apply(samples$y_future, 2, quantile, probs=c(0.025, 0.975))
results
plot(
  normalized_date[1:n], normalized_level[1:n] ,
  col='black', type='l',
  xlim=c(0,1.5),
  ylim=c(0,2),
  main='Data, future data, and predicted 95% interval')
lines(x_future, c(results[1,]), lty='dashed', col='blue')   # 95% interval in blue
lines(x_future, c(results[2,]), lty='dashed', col='blue')
abline(v=n, col='orange')
