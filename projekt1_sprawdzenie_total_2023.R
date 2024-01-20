con <- file("test1.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
dates_delete<-readRDS("dates_delete.Rds")
dates_delete2<-c("2021-06-03",
                 "2021-11-01",
                 "2021-11-11",
                 "2022-01-06",
                 "2022-04-15",
                 "2022-04-18",
                 "2022-05-03",
                 "2022-06-16",
                 "2022-08-15",
                 "2022-11-01",
                 "2022-11-11",
                 "2022-12-26"
)

wig<-read.csv2("wig.csv")
wigm<-read.csv2("wigm.csv")[1:97,]

library("googlesheets4")
#a<-read_sheet("1iywT16NSDbYN2fvvhfatczXXpRyDfs2CZLhR5UZN-NE", sheet="Lista")
#View(a)

#tickersc<-as.character(na.omit(unique(c(a$Ticker1, a$Ticker2, a$Ticker3, "^GSPC"))))
#tickersc<-c('AAPL')
# "ADDYY" %in% tickersc

# getresults<-quantmod::getSymbols(tickersc, src = "yahoo", from = "2013-12-31", to = "2022-01-01", 
#                       periodicity="daily")

# yfres<-yfR::yf_get(tickersc, first_date = "2013-12-31", last_date = "2022-01-01", 
#                    freq_data="daily", do_cache=FALSE)
# yfresm<-yfR::yf_get(tickersc, first_date = "2013-12-01", last_date = "2022-01-01", 
#                     freq_data="monthly", do_cache=FALSE)
#?yf_get

#projekt 1 - sprawdzenie

# TESThttps://docs.google.com/spreadsheets/d/1e75J350DM8pheVacoe1F0k-R_LJmYkOj9MuY9-kEUVU/edit
#https://docs.google.com/spreadsheets/d/1yD4hpgHMC-2U-FXNPsuznWPuIE8kVyLzfz5uFaeZUuA/edit?usp=sharing
#https://docs.google.com/spreadsheets/d/1MDqm2SCPXmWK1amjJPYyRH-MVZCWsyfFkwGHgBY0sc4/edit?usp=sharing

library("googlesheets4")
options(scipen=999)

# gid<-"1l80SeMymlKSSkJXS_QHRa3EKbvVlWnNi4XRxbNOJ3Nk" #copy mrk
gid<-"1iywT16NSDbYN2fvvhfatczXXpRyDfs2CZLhR5UZN-NE" #real
# gid<-"1hBwKsC4CHNUbEuovwyorgT_uR2r1Pjm7kzUqfqTuBvo"
 sp<- sheet_properties(gid)
 spn<-sp$name[c(-1,-2,-3)]
# gs<-c('s122475', 's181152', 's180899', 's148884')
 gs<-c("s196379", "s182408")
 # gs<-c('s156850')
 # gs<-c('s182433')
 # gs<-c('s182447')
 # gs<-c('s182408')
#gs<-c('s196310')
 #gs<-c('s148884')
  
dfres<-data.frame(spn, res=rep(NA, length(spn)))
#dfres$res[which(spn==gs)]<-999

spn2<-spn[spn %in% gs]
#spn2<-spn[5:6]
#spn2<-spn

for(gs in spn2){
 k = 0  
 a<-read_sheet(gid, sheet=gs)
 
#View(sp)
write_res_google=TRUE
tickers<-unname(unlist(a[1,2:4]))
tickers2<-paste0(tickers,'.WA')

quantmod::getSymbols(tickers2, src = "yahoo", from = "2014-12-30", to = "2023-01-01", 
                     periodicity="daily")

sp1<-as.vector(eval(parse(text=paste("`",tickers2[1],"`","$","`", tickers2[1],".Adjusted","`[!zoo::index(`",tickers2[1],"`) %in% c(dates_delete, dates_delete2)]", sep=""))))
sp2<-as.vector(eval(parse(text=paste("`",tickers2[2],"`","$","`", tickers2[2],".Adjusted","`[!zoo::index(`",tickers2[2],"`) %in% c(dates_delete, dates_delete2)]", sep=""))))
sp3<-as.vector(eval(parse(text=paste("`",tickers2[3],"`","$","`", tickers2[3],".Adjusted","`[!zoo::index(`",tickers2[3],"`) %in% c(dates_delete, dates_delete2)]", sep=""))))
spi<-wig$Close
d<-wig$Date
d<-as.Date(d, "%d.%m.%Y")
d<-d[2:length(d)]
library(lubridate)
pon<-wday(d)==2
r1<-diff(sp1)/sp1[1:(length(sp1)-1)]
r2<-diff(sp2)/sp2[1:(length(sp2)-1)]
r3<-diff(sp3)/sp3[1:(length(sp3)-1)]
ri<-diff(spi)/spi[1:(length(spi)-1)]

#View(data.frame(sp1, sp2, sp3, spi))
#View(data.frame(r1, r2, r3, ri))

m<-wigm$Date
m<-as.Date(m, '%d.%m.%Y')
m<-m[2:length(m)]
h1<-year(m)<2019

quantmod::getSymbols(tickers2, src = "yahoo", from = "2014-12-01", to = "2022-12-31", 
                     periodicity="monthly")

sp1m<-as.vector(eval(parse(text=paste("`",tickers2[1],"`","$","`", tickers2[1],".Adjusted","`", sep=""))))
sp2m<-as.vector(eval(parse(text=paste(tickers2[2],"$", tickers2[2],".Adjusted", sep=""))))
sp3m<-as.vector(eval(parse(text=paste(tickers2[3],"$", tickers2[3],".Adjusted", sep=""))))
spim<-wigm$Close


r1m<-diff(sp1m)/sp1m[1:(length(sp1m)-1)]
r2m<-diff(sp2m)/sp2m[1:(length(sp2m)-1)]
r3m<-diff(sp3m)/sp3m[1:(length(sp3m)-1)]
rim<-diff(spim)/spim[1:(length(spim)-1)]


#mean(r1)-unname(unlist(a[5,2]))
#Tabela 1_1
t1_1<-matrix( nrow=4,   
data= c(mean(r1), mean(r2), mean(r3), mean(ri),
sd(r1), sd(r2), sd(r3), sd(ri), 
mean(r1[pon]), mean(r2[pon]), mean(r3[pon]), mean(ri[pon]),
sd(r1[pon]), sd(r2[pon]), sd(r3[pon]),sd(ri[pon]),
mean(r1[!pon]),mean(r2[!pon]),mean(r3[!pon]),mean(ri[!pon]),
sd(r1[!pon]),sd(r2[!pon]),sd(r3[!pon]),sd(ri[!pon])))


#Tabela 1_2
t1_2<-matrix(nrow=4,   
data=c(    
e1071::skewness(r1, type=2),
e1071::skewness(r2, type=2),
e1071::skewness(r3, type=2),
e1071::skewness(ri, type=2),
e1071::kurtosis(r1, type=2),
e1071::kurtosis(r2, type=2),
e1071::kurtosis(r3, type=2),
e1071::kurtosis(ri, type=2)))

#Tabela 1_2c
t1_2c<-matrix(nrow=4,   
             data=c(    
               e1071::skewness(r1, type=3),
               e1071::skewness(r2, type=3),
               e1071::skewness(r3, type=3),
               e1071::skewness(ri, type=3),
               e1071::kurtosis(r1, type=3),
               e1071::kurtosis(r2, type=3),
               e1071::kurtosis(r3, type=3),
               e1071::kurtosis(ri, type=3)))

#Tabela 1_2a
t1_2a<-matrix(nrow=4,   
              data=c(    
                e1071::skewness(r1, type=1),
                e1071::skewness(r2, type=1),
                e1071::skewness(r3, type=1),
                e1071::skewness(ri, type=1),
                e1071::kurtosis(r1, type=1),
                e1071::kurtosis(r2, type=1),
                e1071::kurtosis(r3, type=1),
                e1071::kurtosis(ri, type=1)))


t1_3<-cor(x=data.frame(r1, r2,r3,ri))


#Tabela 2_1
t2_1<-matrix( nrow=4,   
              data= c(mean(r1m), mean(r2m), mean(r3m), mean(rim),
                      sd(r1m), sd(r2m), sd(r3m), sd(rim), 
                      mean(r1m[h1]), mean(r2m[h1]), mean(r3m[h1]), mean(rim[h1]),
                      sd(r1m[h1]), sd(r2m[h1]), sd(r3m[h1]),sd(rim[h1]),
                      mean(r1m[!h1]),mean(r2m[!h1]),mean(r3m[!h1]),mean(rim[!h1]),
                      sd(r1m[!h1]),sd(r2m[!h1]),sd(r3m[!h1]),sd(rim[!h1])))

t2_2<-cor(data.frame(r1m[h1], r2m[h1], r3m[h1], rim[h1]))

t2_3<-cor(data.frame(r1m[!h1], r2m[!h1], r3m[!h1], rim[!h1]))

t2_4<-cor(data.frame(r1m, r2m, r3m, rim))

t3_1<-matrix(nrow=4, data=c(
mean(log(1+r1)),mean(log(1+r2)),mean(log(1+r3)),mean(log(1+ri)),
sd(log(1+r1)),sd(log(1+r2)),sd(log(1+r3)),sd(log(1+ri)),
mean(log(1+r1m)),mean(log(1+r2m)),mean(log(1+r3m)),mean(log(1+rim)),
sd(log(1+r1m)),sd(log(1+r2m)),sd(log(1+r3m)),sd(log(1+rim))
))

t3_2<-matrix(nrow=4, data=c(
exp(252*(t3_1[,1]+t3_1[,2]^2/2))-1,
exp(252*(t3_1[,1]+t3_1[,2]^2/2))*sqrt(exp(252*t3_1[,2]^2)-1),
exp(12*(t3_1[,3]+t3_1[,4]^2/2))-1,
exp(12*(t3_1[,3]+t3_1[,4]^2/2))*sqrt(exp(12*t3_1[,4]^2)-1)))


library(IntroCompFinR)
#?globalMin.portfolio()

mu<-t3_2[1:4,3]
sigma<-t3_2[1:4,4]
corm<-t2_4[1:4, 1:4]
t3_3<-sigma%*%t(sigma)*corm

mu<-mu[1:3]
Sigma<-t3_3[1:3,1:3]
gmvp<-globalMin.portfolio(mu, Sigma, shorts=FALSE)
#globalMin.portfolio(mu, Sigma, shorts=TRUE)

#sum(gmvp$weights*mu)
#sqrt(t(gmvp$weights) %*% Sigma %*% gmvp$weights)

t3_4<-gmvp$weights

t3_5<-c(gmvp$er, gmvp$sd, gmvp$sd^2)

depp<-function(temp){as.numeric(gsub(" p.p", "e-2", gsub(" p.p.", "e-2", gsub(",", ".", temp))))
}

mod1m<-lm(r1m~rim)
mod2m<-lm(r2m~rim)
mod3m<-lm(r3m~rim)

mod1<-lm(r1~ri)
mod2<-lm(r2~ri)
mod3<-lm(r3~ri)

t4_1<-matrix(nrow=3, data=c(
  mod1m$coefficients[2], mod2m$coefficients[2], mod3m$coefficients[2], 
  mod1m$coefficients[1], mod2m$coefficients[1], mod3m$coefficients[1], 
  mod1$coefficients[2], mod2$coefficients[2], mod3$coefficients[2], 
  mod1$coefficients[1], mod2$coefficients[1], mod3$coefficients[1] 
))

t4_2<-matrix(nrow=4, data=rep(1,16))
t4_2[2,1]<-t4_2[1,2]<-(sd(rim))^2*mod1m$coefficients[2]*mod2m$coefficients[2]/sd(r1m)/sd(r2m)
t4_2[3,1]<-t4_2[1,3]<-(sd(rim))^2*mod1m$coefficients[2]*mod3m$coefficients[2]/sd(r1m)/sd(r3m)
t4_2[3,2]<-t4_2[2,3]<-(sd(rim))^2*mod2m$coefficients[2]*mod3m$coefficients[2]/sd(r2m)/sd(r3m)
t4_2[4,1]<-t4_2[1,4]<-(sd(rim))^2*mod1m$coefficients[2]*1/sd(r1m)/sd(rim)
t4_2[4,2]<-t4_2[2,4]<-(sd(rim))^2*mod2m$coefficients[2]*1/sd(r2m)/sd(rim)
t4_2[4,3]<-t4_2[3,4]<-(sd(rim))^2*mod3m$coefficients[2]*1/sd(r3m)/sd(rim)


ers<-depp(unname(unlist(a[87:90,5])))
ers[abs(ers-max(mu))<0.0001]<-max(mu)-0.000000001

shorts=FALSE
eps<-Vectorize(function(x, shorts=FALSE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$sd})
epw1<-Vectorize(function(x, shorts=FALSE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$weights[1]})
epw2<-Vectorize(function(x, shorts=FALSE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$weights[2]})
epw3<-Vectorize(function(x, shorts=FALSE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$weights[3]})

if(max(ers)<=max(mu) & min(ers)>=min(mu)){
  check13=TRUE
  
  t3_6<-matrix(nrow=length(ers), data=c(
    epw1(ers),
    epw2(ers),
    epw3(ers),
    ers,
    eps(ers)
  ))
}else{
check13=FALSE

t3_6<-matrix(nrow=4, data=c(
  rep(0,4),
  rep(0,4),
  rep(0,4),
  rep(0,4),
  rep(0,4)
))
}




  
tabnames<-c("tabela 1_1", "tabela 1_2", "tabela 1_3", "tabela 2_1", "tabela 2_2",
            "tabela 2_3", "tabela 2_4", "tabela 3_1", "tabela 3_2", "tabela 3_3", 
            "tabela 3_4", "tabela 3_5", 
            "tabela 3_6",
            "tabela 4_1", "tabela 4_2")
tabele<-list(t1_1, t1_2, t1_3, t2_1, t2_2, t2_3, t2_4, t3_1, t3_2, t3_3, t3_4, t3_5, 
             t3_6,
             t4_1, t4_2)
tabsource<-list(a[5:8,2:7], a[12:15,2:3], a[19:22,2:5], a[26:29,2:7], a[33:36,2:5]
                , a[40:43,2:5], a[47:50,2:5], a[54:57,2:5], a[61:64,2:5]
                , a[68:71,2:5], a[75:77,2], a[81:83,2], 
                a[87:90, 2:6],
                a[95:97, 2:5], a[102:105,2:5])
tabfbr<-c("I8", "I15", "I22", "I29", "I36", "I43", "I50", "I57", "I64", "I71",
          "I77", "I83", 
          "I90",
          "I96", "I104")
tabprec<-c(4,3, 3,4,3,3,3,4,4,3,2,3,
           2,4,4)
tabprec2<-tabprec-1





#lm(r1~ri)
#i=15
#write_res_google=FALSE
for (i in 1:length(tabele)){
  if(min(dim(as.matrix(tabele[[i]])))>0 && (check13 || i!=13)){
  check<-tabele[[i]]-depp(unname(unlist(tabsource[[i]])))
  print(check)
  empty<-length(depp(unname(unlist(tabsource[[i]]))))==0 | length(tabele[[i]])!=length(depp(unname(unlist(tabsource[[i]]))))
  ok<-sum(abs(round(check, tabprec[i])))==0
  okz<-sum(abs(check))<0.7*10^-tabprec2[i]
  if (empty) {
    print (paste(tabnames[i], "EMPTY!!"))
    if(write_res_google){
      range_write(gid, sheet=gs, range=tabfbr[i], data=data.frame(Sprawdzone="__:(__"))}
  } else if(ok){
    print (paste(tabnames[i], "ok"))
    k=k+1
    if(write_res_google){
      range_write(gid, sheet=gs, range=tabfbr[i], data=data.frame(Sprawdzone="ok"))}
  } else if (okz){
    print (paste(tabnames[i], "zaokrąglenia?"))
    k = k+0.99
    if(write_res_google){
      range_write(gid, sheet=gs, range=tabfbr[i], data=data.frame(Sprawdzone="zaokrąglenia?"))}
  } else {
    print (paste(tabnames[i], "?????"))
    if(write_res_google){
    range_write(gid, sheet=gs, range=tabfbr[i], data=data.frame(Do_sprawdzenia="????"))}
  }
}
}
if(check13==FALSE) {    if(write_res_google){
  range_write(gid, sheet=gs, range=tabfbr[13], data=data.frame(Do_sprawdzenia="!???"))}
}


# library("googlesheets4")
# a<-read_sheet("1uTRluUKvTt3weNZX5vBXq9Sf8_MUh3CoDg-sVjYdDkg", sheet="Wzór")
# View(a)
#t1_1
#t1_3

}

#t4_2

# dfres$res[which(spn==gs)]<-k
# 
# dfres
#range_write('1JfY9GMafjubEVNtvGC7gqCFK3SGaPileQy_eU42HRXY', sheet="INPUT", range="A1", data=dfres)

sink()
sink(type="message")

#dfres
