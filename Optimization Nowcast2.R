library("ggplot2")
library("data.table")
library("dplyr")
library('MASS')
library('Hmisc')

dt_cases2 <- readRDS(file='dt_cases.rds')

# NOWCAST (2) WITH AMOUNT OF CASES RANDOM OF NEGATIVE BIN DIS (WITHIN 95% INTERVAL)

#### Nowcasted number per week ####
#week1
now1 <- rnbinom(n=1, prob = p_lessthan1week , size =Observed_0916)
ID_nowc <- max(dt_cases$ID)+(1:now1)
onset_nowc <- as.Date("2019-09-16")
mat_nowcast <- data.frame(ID = ID_nowc, onset = onset_nowc, report = onset_nowc + 14, 
                          delay = NA, onsetday = as.Date(NA), inter_event_time = NA)
dt_cases2 <- as.data.table(rbind.data.frame(dt_cases2, mat_nowcast))

#week2
now2 <- rnbinom(n=1, prob = p_lessthan2weeks , size =Observed_0909)
ID_nowc <- max(dt_cases$ID)+(1:now2)
onset_nowc <- as.Date("2019-09-09")
mat_nowcast <- data.frame(ID = ID_nowc, onset = onset_nowc, report = onset_nowc + 14, 
                          delay = NA, onsetday = as.Date(NA), inter_event_time = NA)
dt_cases2 <- as.data.table(rbind.data.frame(dt_cases2, mat_nowcast))

#week3
now3 <- rnbinom(n=1, prob = p_lessthan3weeks , size =Observed_0902)
ID_nowc <- max(dt_cases$ID)+(1:now3)
onset_nowc <- as.Date("2019-09-02")
mat_nowcast <- data.frame(ID = ID_nowc, onset = onset_nowc, report = onset_nowc + 14, 
                          delay = NA, onsetday = as.Date(NA), inter_event_time = NA)
dt_cases2 <- as.data.table(rbind.data.frame(dt_cases2, mat_nowcast))

#week4
now4 <- rnbinom(n=1, prob = p_lessthan4weeks , size =Observed_0826)
ID_nowc <- max(dt_cases$ID)+(1:now4)
onset_nowc <- as.Date("2019-08-26")
mat_nowcast <- data.frame(ID = ID_nowc, onset = onset_nowc, report = onset_nowc + 14, 
                          delay = NA, onsetday = as.Date(NA), inter_event_time = NA)
dt_cases2 <- as.data.table(rbind.data.frame(dt_cases2, mat_nowcast))

#week5
now5 <- rnbinom(n=1, prob = p_lessthan5weeks , size =Observed_0819)
ID_nowc <- max(dt_cases$ID)+(1:now5)
onset_nowc <- as.Date("2019-08-19")
mat_nowcast <- data.frame(ID = ID_nowc, onset = onset_nowc, report = onset_nowc + 14, 
                          delay = NA, onsetday = as.Date(NA), inter_event_time = NA)
dt_cases2 <- as.data.table(rbind.data.frame(dt_cases2, mat_nowcast))

#### Inter-eventtimes ####
# amount of total nowcasted cases
newcases <- as.numeric(sum(is.na(dt_cases2$delay)))

# jitter for nowcasted cases between 0 and 7 days
jit <- runif(n=newcases, min=0, max=7)

# Add to onset date
dt_cases2[is.na(delay), onsetday := onset + jit]

# Order cases by onset date
dt_cases2 <- dt_cases2[order(onsetday),]

# Add inter event time to dt_cases (last value is set to 0)
dt_cases2[, inter_event_time := as.numeric(c(onsetday %>% diff, 0))]

# Remove nowcasted values, keep changes inter-event times
dt_cases2<- dt_cases2[!is.na(delay),]
new_int2 <- dt_cases2[,inter_event_time]

# # plot old_int (inter-event-times before nowcasting)
# with new_int (Nowcasted inter-event-times: with mean of binom and proportions over all time)
# with new_int2 (Nowcasted inter-event-times: with random of binom and proportions over all time)

plot (old_int, type = 'l', xlim = c(3000,3088), 
                 ylim=c(0,2.5), main = "Inter-event-times", xlab = 'Case ID', ylab = "Inter-event-time(days)")
lines(new_int, col ='#fdbb84')
lines(new_int2, col = '#e34a33')

legend('topleft', legend=c("Before nowcasting", "After nowcasting (1)", "After nowcasting (2)"),
       col=c('black','#fdbb84', "#e34a33"),lty = 1, cex=0.8)

# plot new nowcasted values

nowcast2 <- rbind(now5,now4,now3,now2,now1)
nowcast2 <- as.data.table(nowcast2)

nowcast2 <- nowcast2[, onset := weeks]
