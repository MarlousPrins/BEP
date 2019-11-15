library("ggplot2")
library("data.table")
library("dplyr")
library('MASS')
library ('mixtools')
library('mvtnorm')
library('Hmisc')

# FORECAST {output nowcast, thus GLM's} # 

#### WITH INT_EVENT 1 ####
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
alpha <- gamma.dispersion(glm1) # as from gamma.dispersion in GLM script: The MLE of the dispersion parameter of the gamma distribution.
gamma <- as.numeric(rgamma (length(result_eta), shape = alpha, rate = alpha * result_eta))

# Add inter-event times #COLORBREWER
x[,intereventtime := gamma]

# Determine day of onset for forecasted cases
firstdate <- as.Date( "2019-09-16")
forecastdates <- cumsum(x$intereventtime) + firstdate

x[,date := forecastdates]

# Cases per week
weekly <- cut2(x$date, cuts = seq(as.Date("2019-09-16"), as.Date("2021-02-12"), "week"))

levels(weekly) <- seq(as.Date("2019-09-16"), as.Date("2021-02-12"), "week")

forecastplot <- plot(table(weekly), type = "h", main= "Ebola forecast", ylab = "Number of cases", xlab = "Forecast weeks")

#### WITH INT_EVENT 2 ####
# 1000 additional cases
CaseID <- as.numeric (c(3088:4087))
x2 <- data.table(CaseID)

# Quadratic model with set of sample betas and future cases
eta <- function(x2,b1= samp_b1_2, b2 = samp_b2_2){
  b1*x2 + b2*x2^2
}
result_eta <- data.frame(eta(x2))

# Delete rows with negative eta
x2 <- x2[!result_eta$C<=0,]
result_eta <- result_eta[!result_eta$C<=0,]

# Gamma distribution with shape parameter = alpha and rate parameter = alpha * eta
alpha <- gamma.dispersion(glm2) # as from gamma.dispersion in GLM script: The MLE of the dispersion parameter of the gamma distribution.
gamma <- as.numeric(rgamma (length(result_eta), shape = alpha, rate = alpha * result_eta))

# Add inter-event times #COLORBREWER
x2[,intereventtime := gamma]


# Determine day of onset for forecasted cases
firstdate <- as.Date( "2019-09-16")
forecastdates <- cumsum(x2$intereventtime) + firstdate

x2[,date := forecastdates]

# Cases per week
weekly2 <- cut2(x2$date, cuts = seq(as.Date("2019-09-16"), as.Date("2021-07-13"), "week"))

levels(weekly2) <- seq(as.Date("2019-09-16"), as.Date("2021-07-13"), "week")

forecastplot2 <- plot(table(weekly2), type = "h", main= "Ebola forecast 2", ylab = "Number of cases", xlab = "Forecast weeks")

#### WITH INT_EVENT 3 ####
# 1000 additional cases
CaseID <- as.numeric (c(3088:5087))
x3 <- data.table(CaseID)

# Quadratic model with set of sample betas and future cases
eta <- function(x3,b1= samp_b1_3, b2 = samp_b2_3){
  b1*x3 + b2*x3^2
}
result_eta <- data.frame(eta(x3))

# Delete rows with negative eta
x3 <- x3[!result_eta$C<=0,]
result_eta <- result_eta[!result_eta$C<=0,]

# Gamma distribution with shape parameter = alpha and rate parameter = alpha * eta
alpha <- gamma.dispersion(glm3) # as from gamma.dispersion in GLM script: The MLE of the dispersion parameter of the gamma distribution.
gamma <- as.numeric(rgamma (length(result_eta), shape = alpha, rate = alpha * result_eta))

# Add inter-event times #COLORBREWER
x3[,intereventtime := gamma]

# Determine day of onset for forecasted cases
firstdate <- as.Date( "2019-09-16")
forecastdates <- cumsum(x3$intereventtime) + firstdate

x3[,date := forecastdates]

# Cases per week
weekly3 <- cut2(x3$date, cuts = seq(as.Date("2019-09-16"), as.Date("2021-09-22"), "week"))

levels(weekly3) <- seq(as.Date("2019-09-16"), as.Date("2021-09-22"), "week")

forecastplot3 <- plot(table(weekly3), type = "h", main= "Ebola forecast 3", ylab = "Number of cases", xlab = "Forecast weeks")

