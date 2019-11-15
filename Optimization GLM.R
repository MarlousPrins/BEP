library("ggplot2")
library("data.table")
library("dplyr")
library('MASS')
library ('mixtools')
library('mvtnorm')

dt_cases

#### DATA PREP ####
# inter-event time of nowcast 1
colnames(dt_cases)[colnames(dt_cases)=="inter_event_time"] <- "int_time1"

# add inter-event-times of nowcast 2 and nowcast 3

dt_cases<- dt_cases[, int_time2:= new_int2]

# Cumulative symptom onset
dt_cases <- mutate (dt_cases,
                 cumulative_symptom_onset = as.numeric(1:3088))

# Squared cumulative symptom onset
dt_cases <- mutate (dt_cases,
                 C2 = dt_cases$cumulative_symptom_onset^2)

print(dt_cases)
#### GLM ####

#glm with int_time1
n <- 3088

dt_cases[n, "int_time1"] <- NA

glm1 <- glm(as.numeric(dt_cases$int_time1) ~ dt_cases$cumulative_symptom_onset 
           + dt_cases$C2, family = Gamma(link="inverse"))
summary(glm1)

#glm with int_time2

dt_cases[n, "int_time2"] <- NA

glm2 <- glm(as.numeric(dt_cases$int_time2) ~ dt_cases$cumulative_symptom_onset 
            + dt_cases$C2, family = Gamma(link="inverse"))
summary(glm2)


# Plot for three glm's : 'predict'command gives predictor (1/mu)
plot(dt_cases$cumulative_symptom_onset, 1/as.numeric(dt_cases$int_time1), 
     main = "GLM",ylab= "Rate of infection", 
     xlab = "Cumulative sum of past symptom onset (#)", cex=0.5, ylim=c(0,150))

lines(dt_cases$cumulative_symptom_onset[-n], predict(glm1), col='yellow')
lines(dt_cases$cumulative_symptom_onset[-n], predict(glm2), col='red')

#### OUTPUT GLM ####

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
samp_betas <- res_betas [random,]

samp_b1_1 <- samp_betas[1]
samp_b2_1 <- samp_betas[2]

#### BETAS (int_time2)####

mu <- c(1.115e-02, -2.816e-06)
sigma <- vcov(glm2)[2:3, 2:3]

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

samp_b1_2 <- samp_betas[1]
samp_b2_2 <- samp_betas[2]

