library(quantmod)
Ticker<-"INTC"
getSymbols(Ticker, src = "yahoo", 
           from = '2016-12-01', 
           to = '2022-01-01', 
           periodicity="monthly")

monthly<-as.numeric(INTC$INTC.Adjusted)
monthly_R<-diff(monthly)/monthly[1:(length(monthly)-1)]
monthly_r<-log(monthly[2:length(monthly)]/monthly[1:(length(monthly)-1)])

mm<-mean(monthly_R)
mc<-(prod(1+monthly_R))^(1/60)-1
ms<-sd(monthly_R)
msk<-e1071::skewness(monthly_R, type=2)
mku<-e1071::kurtosis(monthly_R, type=2)
plot(density(monthly_R))

(mlm<-mean(monthly_r))
(mls<-sd(monthly_r))
(mlsk<-e1071::skewness(monthly_r, type=2))
(mlku<-e1071::kurtosis(monthly_r, type=2))
plot(density(monthly_r))


getSymbols(Ticker, src = "yahoo", 
           from = '2016-12-30', 
           to = '2022-01-01', 
           periodicity="daily")

daily<-as.numeric(INTC$INTC.Adjusted)
daily_R<-diff(daily)/daily[1:(length(daily)-1)]
daily_r<-log(daily[2:length(daily)]/daily[1:(length(daily)-1)])

(dm<-mean(daily_R))
(dc<-(prod(1+daily_R))^(1/length(daily_R))-1)
(ds<-sd(daily_R))
(dsk<-e1071::skewness(daily_R, type=2))
(dku<-e1071::kurtosis(daily_R, type=2))
plot(density(daily_R))

(dlm<-mean(daily_r))
(dls<-sd(daily_r))
(dlsk<-e1071::skewness(daily_r, type=2))
(dlku<-e1071::kurtosis(daily_r, type=2))
plot(density(daily_r))

View(data.frame(proste_mies=c(mm, mc, ms, msk, mku),
                proste_dzie=c(dm, dc, ds, dsk, dku),
                log_mies=c(mlm, NA, mls, mlsk, mlku),
                log_dzie=c(dlm, NA, dls, dlsk, dlku)))

