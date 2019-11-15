library("ggplot2")
library("data.table")
library("dplyr")
library('MASS')
library ('mixtools')
library('mvtnorm')
library('Hmisc')

# FORECAST WITH: INTER_EVENT TIMES 1 

#### OUTPUT GLM ####

n <- 3088
X <- dt_cases$cumulative_symptom_onset

#### BETAS (int_time1) ####
# from output glm 
mu <- c(1.1243e-02, -2.966e-06)
sigma <- vcov(glm1)[2:3, 2:3]

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
samp_betas <- betas[random,]

samp_b1_1 <- samp_betas[1]
samp_b2_1 <- samp_betas[2]
#### Forecast #### 
# 1000 additional cases
CaseID <- as.numeric (c(3088:4087))
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

# Determine day of onset for forecasted cases
firstdate <- as.Date( "2019-09-23")
forecastdates <- cumsum(x$intereventtime) + firstdate

x[,date := forecastdates]

# Cases per week
weekly <- cut2(x$date, cuts = seq(as.Date("2019-09-23"), as.Date("2021-02-12"), "week"))

levels(weekly) <- seq(as.Date("2019-09-23"), as.Date("2021-02-12"), "week")

f_cases <- as.data.table(table(weekly))
f_cases <- f_cases[1:3,2]
f_cases <- t(f_cases)

