#Projekt

#quantmod::getSymbols("AAPL", from='2019-01-01', verbose = TRUE)
#?getSymbols

for (i in 1:2){

tickers<-c("AAPL", "AMZN", "TSLA", "^GSPC")
tickers2<-gsub("\\^", "", tickers)

if (i==1) 
{quantmod::getSymbols(tickers, src = "yahoo", from = "2013-12-01", to = "2022-01-01", 
                     periodicity="monthly")
} else if (i==2)
{quantmod::getSymbols(tickers, src = "yahoo", from = "2013-12-31", to = "2022-01-01", 
                      periodicity="daily")
}


s1<-as.vector(eval(parse(text=paste(tickers2[1],"$", tickers2[1],".Adjusted", sep=""))))
s2<-as.vector(eval(parse(text=paste(tickers2[2],"$", tickers2[2],".Adjusted", sep=""))))
s3<-as.vector(eval(parse(text=paste(tickers2[3],"$", tickers2[3],".Adjusted", sep=""))))
s4<-as.vector(eval(parse(text=paste(tickers2[4],"$", tickers2[4],".Adjusted", sep=""))))
data<-time(eval(parse(text=paste(tickers2[4],"$", tickers2[4],".Adjusted", sep=""))))

if(i==1){
  sm<-data.frame(data, s1, s2, s3, s4)}
else if(i==2){
  sd<-data.frame(data, s1, s2, s3, s4)}
#View(sm)

Rcalc<-function(x){diff(x)/x[-length(x)]}

R1<-Rcalc(s1)
R2<-Rcalc(s2)
R3<-Rcalc(s3)
R4<-Rcalc(s4)

rcalc<-function(x){log(diff(x)/x[-length(x)]+1)}

r1<-rcalc(s1)
r2<-rcalc(s2)
r3<-rcalc(s3)
r4<-rcalc(s4)

# mean(r1)
# exp(mean(r1)+(sd(r1))^2/2)-1
# mean(R1)
# exp(mean(r1))-1
# (cumprod(1+R1)[length(R1)])^(1/length(R1))-1


if(i==1){
  rm<-data.frame(data=data[-1], r1, r2, r3, r4)
  Rm<-data.frame(data=data[-1], R1, R2, R3, R4)
}
else if(i==2){
  rd<-data.frame(data=data[-1], r1, r2, r3, r4)
  Rd<-data.frame(data=data[-1], R1, R2, R3, R4)
}

#table(lubridate::year(rd$data))

#View(rm)
}

# colMeans(Rm[,-1])
colMeans(Rd[,-1])
# colMeans(rm[,-1])
# colMeans(rd[,-1])

# as.numeric(format(Sys.Date(), format = "%u"))
# lubridate::wday(Sys.Date(), week_start=1)
# lubridate::wday(Sys.Date())
# as.numeric(format(Rd$data, format = "%u"))

colMeans(Rd[lubridate::wday(Rd$data)==2,-1])
colMeans(Rd[lubridate::wday(Rd$data)>2,-1])

apply(Rd[lubridate::wday(Rd$data)==2,-1], 2, sd)
apply(Rd[lubridate::wday(Rd$data)>2,-1], 2, sd)

t1_1<-data.frame(ticker=tickers,
                 srednia_caly = colMeans(Rd[,-1]),
                 odch_caly = apply(Rd[,-1], 2, sd),
                 srednia_pn = colMeans(Rd[lubridate::wday(Rd$data)==2,-1]),
                 odch_pn = apply(Rd[lubridate::wday(Rd$data)==2,-1], 2, sd),
                 srednia_inne = colMeans(Rd[lubridate::wday(Rd$data)>2,-1]),
                 odch_inne = apply(Rd[lubridate::wday(Rd$data)>2,-1], 2, sd))

t1_2<-data.frame(ticker=tickers, 
                 skosnosc=apply(Rd[,-1],2, function(x){e1071::skewness(x, type=2)}),
                 kurtoza=apply(Rd[,-1],2, function(x){e1071::kurtosis(x, type=2)}))

t1_3<-cor(Rd[,-1])

t2_1<-data.frame(ticker=tickers,
                 srednia_caly = colMeans(Rm[,-1]),
                 odch_caly = apply(Rm[,-1], 2, sd),
                 srednia_1 = colMeans(Rm[lubridate::year(Rm$data)<2018,-1]),
                 odch_1 = apply(Rm[lubridate::year(Rm$data)<2018,-1], 2, sd),
                 srednia_2 = colMeans(Rm[lubridate::year(Rm$data)>2017,-1]),
                 odch_2 = apply(Rm[lubridate::year(Rm$data)>2017,-1], 2, sd))

t2_2<-cor(Rm[lubridate::year(Rm$data)<2018,-1])

t2_3<-cor(Rm[lubridate::year(Rm$data)>2017,-1])

t2_4<-cor(Rm[,-1])

t3_1<-data.frame(ticker=tickers,
                 nu_d = colMeans(rd[,-1]),
                 sigma_d = apply(rd[,-1], 2, sd),
                 nu_m = colMeans(rm[,-1]),
                 sigma_m = apply(rm[,-1], 2, sd))

t3_2<-data.frame(ticker=tickers, 
                 Rmu_rd=exp(252*(t3_1$nu_d+t3_1$sigma_d^2/2))-1,
                 Rsigma_rd=exp(252*(t3_1$nu_d+t3_1$sigma_d^2/2))*sqrt(exp(252*t3_1$sigma_d^2)-1),
                 Rmu_rm=exp(12*(t3_1$nu_m+t3_1$sigma_m^2/2))-1,
                 Rsigma_rm=exp(12*(t3_1$nu_m+t3_1$sigma_m^2/2))*sqrt(exp(12*t3_1$sigma_m^2)-1)
)

library(IntroCompFinR)
#?globalMin.portfolio()

mu<-t3_2[1:3,4]
sigma<-t3_2[1:3,5]
corm<-t2_4[1:3, 1:3]
t3_3<-Sigma<-sigma%*%t(sigma)*corm
gmvp<-globalMin.portfolio(mu, Sigma, shorts=FALSE)
#globalMin.portfolio(mu, Sigma, shorts=TRUE)

#sum(gmvp$weights*mu)
#sqrt(t(gmvp$weights) %*% Sigma %*% gmvp$weights)

t3_4<-data.frame(ticker=tickers[1:3], gmvp$weights)

t3_5<-c(gmvp$er, gmvp$sd, gmvp$sd^2)

#ers<-
  
st<-ceiling(gmvp$er*100)/100
en<-floor(max(mu)*100)/100
se<-seq(st, en, by=.01)

eps<-Vectorize(function(x, shorts=FALSE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$sd})
epw1<-Vectorize(function(x, shorts=FALSE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$weights[1]})
epw2<-Vectorize(function(x, shorts=FALSE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$weights[2]})
epw3<-Vectorize(function(x, shorts=FALSE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$weights[3]})

t3_6<-data.frame(w1=epw1(se), w2=epw2(se), w3=epw3(se), mu=se, sigma=eps(se))
#View(t3_6)

plot(t3_6$sigma, t3_6$mu)

# max(mu)
# 
# eps(rep(40:80/100,2), rep(c(TRUE, FALSE),41))
# 
# plot(sigma, mu, ylim=c(min(mu), max(mu)+(max(mu)-min(mu))/5))
# text(sigma, mu+(max(mu)-min(mu))/10, labels=tickers[1:3])
# tickers

# library("googlesheets4")
# a<-read_sheet("1uTRluUKvTt3weNZX5vBXq9Sf8_MUh3CoDg-sVjYdDkg", sheet="Sheet1")
# View(a)

