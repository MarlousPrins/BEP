dt_cases

# Sampling
n <- 2884
dt_cases <- dt_cases[1:2884,]
X <- dt_cases$cumulative_symptom_onset

glm_forecast <- glm(as.numeric(dt_cases$int_time1) ~ dt_cases$cumulative_symptom_onset 
           + dt_cases$C2, family = Gamma(link="inverse"))

#### Sampling BETAS ####
# from output glm 
mu <- c(1.074e-02, -2.592e-06)
sigma <- vcov(glm_forecast)[2:3, 2:3]

# Multivariate distribution of betas
betas <- mvrnorm(n,mu, sigma)

# Samples restrictions with: int-event-time > 0, B1 >0, B2<0
b1 <- betas[,1]
b2 <- betas[,2]
ratio <- -b1/b2  # gives you the amount of forecast cases (possible)
linpred <- X * b1 + X**2 * b2

# Betas with restriction
res_betas <- betas[linpred>0 & ratio > 0 & b1 > 0, ] 

# Random sample of set b1 and b2 
random <- sample(x = 1:n, size = 1)
samp_betas <- res_betas [random,]

samp_b1_1 <- samp_betas[1]
samp_b2_1 <- samp_betas[2]

#### Forecast ####
# 200 additional  
CaseID <- as.numeric(c(2885:3035))
x <- data.table(CaseID)

# Quadratic model with set of sample betas and future cases
eta <- function(x,b1= samp_b1_1, b2 = samp_b2_1){
  b1*x + b2*x^2
}
result_eta <- data.frame(eta(x))

# Delete rows with negative eta
x <- x[!result_eta$C<=0,]
result_eta <- result_eta[!result_eta$C<=0,]

# Gamma distribution with shape parameter = alpha and rate parameter = alpha * eta
alpha <- 0.9317589 # as from gamma.dispersion in GLM script: The MLE of the dispersion parameter of the gamma distribution.
gamma <- as.numeric(rgamma (length(result_eta), shape = alpha, rate = alpha * result_eta))

# Add inter-event times
x[,intereventtime := gamma]

# Determine week of onset for cases
firstdate <- as.Date( "2019-08-19")
forecastdates <- cumsum(x$intereventtime) + firstdate

x[,date := forecastdates]

# Cases per week
weekly <- cut2(x$date, cuts = seq(as.Date("2019-08-19"), as.Date("2019-09-02"), "week"))

levels(weekly) <- seq(as.Date("2019-08-19"), as.Date("2019-09-02"), "week")

f_cases <- as.data.table(table(weekly))
f_cases <- f_cases[1:3,2]
f_cases <- t(f_cases)
