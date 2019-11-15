library("ggplot2")
library("data.table")
library("dplyr")
library('MASS')
library('Hmisc')

#### Load dt cases ####

dt_cases <- as.data.table(read.csv(file = "C:/Users/maart/Documents/Marlousje/BEP/linelist.csv"))

# Format dates from Excel file: dates correspond with first day of the week
dt_cases[, onset := as.Date(onset, format = "%d/%m/%Y")]
dt_cases[, report := as.Date(report, format = "%d/%m/%Y")]

# Plot

weeklyobservedcases <- cut2(dt_cases$onset, cuts = seq (as.Date("2018-07-16"), as.Date("2019-09-16"), "week"))
levels (weeklyobservedcases) <- seq (as.Date("2018-07-16"), as.Date("2019-09-16"), "week")
observedplot <- plot(table(weeklyobservedcases), type = "h", col = "blue",
                     main =" Observed ebola cases", ylab = "Number of cases", xlab= 'Weeks')

#### Determine r_delay ####
# One week before the first onset date
ebola.nowcast <- dt_cases

t_start <- as.Date("2018-08-19")

# Maximum delay for every case = date of report - date of onset
max_delay <- as.numeric(dt_cases$report - dt_cases$onset)

# Minimum delay for every case
min_delay <- as.numeric(dt_cases$report - (dt_cases$onset +7))

# if min_delay is less than zero, convert to zero
min_delay[min_delay <0] <- 0

# Initialise empty vector for randomly drawn delay
r_delay <- numeric(nrow(dt_cases))

# Draw delay for each case, with min_dely and max_delay as boudaries
for(i in 1:length(min_delay))
  r_delay[i] <- runif(n = 1, min = min_delay[i], max = max_delay[i])

# add r_delay to dt_cases
dt_cases[, delay := r_delay]

# order by date of symptom onset
dt_cases <- dt_cases[order(onset),]

#### Adding onset day + inter-event-time ####
# Draw random value between 0 and 7 days
jit <- runif(n=nrow(dt_cases), min=0, max=7)
jit_date <- dt_cases$onset + jit

# Add to onset date
dt_cases[, onsetday := c(jit_date)]

# Order cases by onset date
dt_cases <- dt_cases[order(onsetday),]

# Add inter event time to dt_cases (last value is set to 0)
dt_cases[, inter_event_time := as.numeric(c(onsetday %>% diff, 0))]

# Save data set
saveRDS(dt_cases, file = 'dt_cases.rds')

#  vector of inter-event-times before nowcasting
old_int <- dt_cases[,inter_event_time]

#### Nowcasting #### 
# Variable t_0 (first onset date)
t_0 <- as.Date("2018-07-16")

#### Week 1: 2019-09-16 ####
# N1 = Number of cases between 2018-07-16 and 2019-09-09

t_before0916 <- as.Date("2019-09-09")
t_0916 <- as.Date("2019-09-16")

N1 <- ebola.nowcast[onset <= t_before0916 & onset >= t_0, ] %>% nrow

# Amount of cases with delay >= 7 days and < 7 days

n1 <- ebola.nowcast[onset <= t_before0916 & onset >= t_0 & delay >= 7,] %>% nrow
n2 <- ebola.nowcast[onset <= t_before0916 & onset >= t_0 & delay < 7,] %>% nrow

# Proportion

p_morethan1week <- n1/N1
p_lessthan1week <- n2/N1

# Amount of nowcasted cases 

Observed_0916 <- ebola.nowcast[onset == t_0916, ] %>% nrow
Nowcast_0916 <- (Observed_0916/p_lessthan1week)*p_morethan1week

# Round new nb of cases
N_nowc <- Nowcast_0916 %>% round

# create matrix 17 cases
ID_nowc <- max(dt_cases$ID)+(1:N_nowc)
onset_nowc <- as.Date("2019-09-16")
mat_nowcast <- data.frame(ID = ID_nowc, onset = onset_nowc, report = onset_nowc + 14, 
                          delay = NA, onsetday = as.Date(NA), inter_event_time = NA)
dt_cases <- as.data.table(rbind.data.frame(dt_cases, mat_nowcast))

#### Week 2: 2019-09-09 ####
# N2 = Number of cases between 2018-07-16 and 2019-09-02

t_before0909 <- as.Date("2019-09-02")
t_0909 <- as.Date("2019-09-09")

N2 <- ebola.nowcast[onset <= t_before0909 & onset >= t_0, ] %>% nrow

# Amount of cases with delay >= 7 days and < 7 days

n_morethan2weeks <- ebola.nowcast[onset <= t_before0909 & onset >= t_0 & delay >= 14,] %>% nrow
n_lessthan2weeks <- ebola.nowcast[onset <= t_before0909 & onset >= t_0 & delay < 14,] %>% nrow

# Proportion

p_morethan2weeks <- n_morethan2weeks/N2
p_lessthan2weeks <- n_lessthan2weeks/N2

# Amount of nowcasted cases 

Observed_0909 <- ebola.nowcast[onset == t_0909, ] %>% nrow
Nowcast_0909 <- (Observed_0909/p_lessthan2weeks)*p_morethan2weeks

# Round new nb of cases
N_nowc <- Nowcast_0909 %>% round

# create matrix nowcasted cases
ID_nowc <- max(dt_cases$ID)+(1:N_nowc)
onset_nowc <- as.Date("2019-09-09")
mat_nowcast <- data.frame(ID = ID_nowc, onset = onset_nowc, report = onset_nowc + 14, 
                          delay = NA, onsetday = as.Date(NA), inter_event_time = NA)
dt_cases <- as.data.table(rbind.data.frame(dt_cases, mat_nowcast))

#### Week 3: 2019-09-02 ####
# N3 = Number of cases between 2018-07-16 and 2019-08-26

t_before0902 <- as.Date("2019-08-26")
t_0902 <- as.Date("2019-09-02")

N3 <- ebola.nowcast[onset <= t_before0902 & onset >= t_0, ] %>% nrow

# Amount of cases with delay >= 7 days and < 7 days

n_morethan3weeks <- ebola.nowcast[onset <= t_before0902 & onset >= t_0 & delay >= 21,] %>% nrow
n_lessthan3weeks <- ebola.nowcast[onset <= t_before0902 & onset >= t_0 & delay < 21,] %>% nrow

# Proportion

p_morethan3weeks <- n_morethan3weeks/N3
p_lessthan3weeks <- n_lessthan3weeks/N3

# Amount of nowcasted cases 

Observed_0902 <- ebola.nowcast[onset == t_0902, ] %>% nrow
Nowcast_0902 <- (Observed_0902/p_lessthan3weeks)*p_morethan3weeks

# Round new nb of cases
N_nowc <- Nowcast_0902 %>% round

# create matrix nowcasted cases
ID_nowc <- max(dt_cases$ID)+(1:N_nowc)
onset_nowc <- as.Date("2019-09-02")
mat_nowcast <- data.frame(ID = ID_nowc, onset = onset_nowc, report = onset_nowc + 14, 
                          delay = NA, onsetday = as.Date(NA), inter_event_time = NA)
dt_cases <- as.data.table(rbind.data.frame(dt_cases, mat_nowcast))

#### Week 4: 2019-08-26 ####
# N4 = Number of cases between 2018-07-16 and 2019-08-19

t_before0826 <- as.Date("2019-08-19")
t_0826 <- as.Date("2019-08-26")

N4 <- ebola.nowcast[onset <= t_before0826 & onset >= t_0, ] %>% nrow

# Amount of cases with delay >= 7 days and < 7 days

n_morethan4weeks <- ebola.nowcast[onset <= t_before0826 & onset >= t_0 & delay >= 28,] %>% nrow
n_lessthan4weeks <- ebola.nowcast[onset <= t_before0826 & onset >= t_0 & delay < 28,] %>% nrow

# Proportion

p_morethan4weeks <- n_morethan4weeks/N4
p_lessthan4weeks <- n_lessthan4weeks/N4

# Amount of nowcasted cases 

Observed_0826 <- ebola.nowcast[onset == t_0826, ] %>% nrow
Nowcast_0826 <- (Observed_0826/p_lessthan4weeks)*p_morethan4weeks

# Round new nb of cases
N_nowc <- Nowcast_0826 %>% round

# create matrix nowcasted cases
ID_nowc <- max(dt_cases$ID)+(1:N_nowc)
onset_nowc <- as.Date("2019-08-26")
mat_nowcast <- data.frame(ID = ID_nowc, onset = onset_nowc, report = onset_nowc + 14, 
                          delay = NA, onsetday = as.Date(NA), inter_event_time = NA)
dt_cases <- as.data.table(rbind.data.frame(dt_cases, mat_nowcast))

#### Week 5: 2019-08-19 ####
# N5 = Number of cases between 2018-07-16 and 2019-08-12

t_0 <- as.Date("2018-07-16")
t_before0819 <- as.Date("2019-08-12")
t_0819 <- as.Date("2019-08-19")

N5 <- ebola.nowcast[onset <= t_before0819 & onset >= t_0, ] %>% nrow

# Amount of cases with delay >= 7 days and < 7 days

n_morethan5weeks <- ebola.nowcast[onset <= t_before0819 & onset >= t_0 & delay >= 35,] %>% nrow
n_lessthan5weeks <- ebola.nowcast[onset <= t_before0819 & onset >= t_0 & delay < 35,] %>% nrow

# Proportion

p_morethan5weeks <- n_morethan5weeks/N5
p_lessthan5weeks <- n_lessthan5weeks/N5

# Amount of nowcasted cases 

Observed_0819 <- ebola.nowcast[onset == t_0819, ] %>% nrow
Nowcast_0819 <- (Observed_0819/p_lessthan5weeks)*p_morethan5weeks

# Round new nb of cases
N_nowc <- Nowcast_0819 %>% round

# Create matrix nowcasted cases
ID_nowc <- max(dt_cases$ID)+(1:N_nowc)
onset_nowc <- as.Date("2019-08-19")
mat_nowcast <- data.frame(ID = ID_nowc, onset = onset_nowc, report = onset_nowc + 14, 
                          delay = NA, onsetday = as.Date(NA), inter_event_time = NA)
dt_cases <- as.data.table(rbind.data.frame(dt_cases, mat_nowcast))

#### Inter-event-times latest cases + nowcast plot ####

# compute daily onset for nowcasted values
# amount of total nowcasted cases
newcases <- as.numeric(sum(is.na(dt_cases$delay)))

# jitter for nowcasted cases between 0 and 7 days
jit <- runif(n=newcases, min=0, max=7)

# Add to onset date
dt_cases[is.na(delay), onsetday := onset + jit]

# Order cases by onset date
dt_cases <- dt_cases[order(onsetday),]

# Add inter event time to dt_cases (last value is set to 0)
dt_cases[, inter_event_time := as.numeric(c(onsetday %>% diff, 0))]

# plot nowcasted values

dt_cases_nowcast <- dt_cases[is.na(delay),]
dt_cases_nowcast <- dt_cases_nowcast[order(onset),] 

weeklynowcastcases <- cut2(dt_cases_nowcast$onset, 
                           cuts = seq (as.Date("2019-08-19"), as.Date("2019-09-16"), "week"))

levels (weeklynowcastcases) <- seq (as.Date("2019-08-19"), as.Date("2019-09-16"), "week")

nowcastplot <- plot(table(weeklynowcastcases), type = "h", col = "orange",
                     main ="Nowcast ebola cases", ylab = "Number of cases", xlab= 'Weeks')

nowcastcases <- data.table(table(weeklynowcastcases))

# Remove nowcasted values (inter-event times are kept the same)
dt_cases<- dt_cases[!is.na(delay),]

new_int <- dt_cases[,inter_event_time]

# old v.s. new inter-event-times
plot (old_int, type = 'l', xlim = c(2900,3088), ylim=c(0,2))
lines(new_int, col ='purple')

#### 95% confidence interval #### 
# negative binomial distribution per week
# week 1
week1 <- as.data.table(qnbinom(c(0.025, 0.5, 0.975), prob = p_lessthan1week , size =Observed_0916))
week1<- t(week1)

# week 2
week2 <- as.data.table(qnbinom(c(0.025, 0.5, 0.975), prob = p_lessthan2weeks , size =Observed_0909))
week2<- t(week2)

# week 3
week3 <-as.data.table(qnbinom(c(0.025, 0.5, 0.975), prob = p_lessthan3weeks , size =Observed_0902))
week3<- t(week3)
# week 4 
week4 <-as.data.table(qnbinom(c(0.025, 0.5, 0.975), prob = p_lessthan4weeks , size =Observed_0826))
week4 <- t(week4)
# week 5 
week5 <- as.data.table(qnbinom(c(0.025, 0.5, 0.975), prob = p_lessthan5weeks , size =Observed_0819))
week5 <- t(week5)

# bind all weeks 
allweeks <- rbind(week5, week4, week3, week2, week1)
allweeks <- as.data.table(allweeks)

# add onset week
weeks <- (seq(as.Date("2019-08-19"), as.Date("2019-09-16"), "week", format = "%d/%m/%Y"))
allweeks <- allweeks[, onset := weeks]

#plot nowcasted values with 95% interval
plot_now <- ggplot(allweeks, aes(allweeks$onset, allweeks$V2)) + 
  geom_bar(position=position_dodge(), stat = 'identity', fill='#016450') + 
  geom_errorbar(aes(ymin = allweeks$V1, ymax = allweeks$V3), 
                width=.8, position=position_dodge(.5))
plot_now + labs (x= 'Week of symptom onset', y='Numbers of cases', title = 'Nowcasted cases')

