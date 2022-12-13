
#projekt 1 - sprawdzenie

# TESThttps://docs.google.com/spreadsheets/d/1e75J350DM8pheVacoe1F0k-R_LJmYkOj9MuY9-kEUVU/edit
#https://docs.google.com/spreadsheets/d/1yD4hpgHMC-2U-FXNPsuznWPuIE8kVyLzfz5uFaeZUuA/edit?usp=sharing
#https://docs.google.com/spreadsheets/d/1MDqm2SCPXmWK1amjJPYyRH-MVZCWsyfFkwGHgBY0sc4/edit?usp=sharing
library("googlesheets4")
options(scipen=999)
#gid<-"1yD4hpgHMC-2U-FXNPsuznWPuIE8kVyLzfz5uFaeZUuA"
gid<-"1MDqm2SCPXmWK1amjJPYyRH-MVZCWsyfFkwGHgBY0sc4"
gs<-"Dane1"
a<-read_sheet(gid, sheet=gs)
#View(a)
tickers<-unname(unlist(a[1,2:4]))
sp1<-unname(unlist(yfres[yfres$ticker==tickers[1],c("price_adjusted")]))
d<-yfres[yfres$ticker==tickers[1],c("ref_date")]$ref_date
d<-d[2:length(d)]
library(lubridate)
pon<-wday(d)==2
r1<-diff(sp1)/sp1[1:(length(sp1)-1)]
sp2<-unname(unlist(yfres[yfres$ticker==tickers[2],c("price_adjusted")]))
r2<-diff(sp2)/sp2[1:(length(sp2)-1)]
sp3<-unname(unlist(yfres[yfres$ticker==tickers[3],c("price_adjusted")]))
r3<-diff(sp3)/sp3[1:(length(sp3)-1)]
spi<-unname(unlist(yfres[yfres$ticker=="^GSPC",c("price_adjusted")]))
ri<-diff(spi)/spi[1:(length(spi)-1)]

#View(data.frame(sp1, sp2, sp3, spi))
#View(data.frame(r1, r2, r3, ri))

m<-yfresm[yfresm$ticker==tickers[1],c("ref_date")]$ref_date
m<-m[2:length(m)]
h1<-year(m)<2018
sp1m<-unname(unlist(yfresm[yfresm$ticker==tickers[1],c("price_adjusted")]))
r1m<-diff(sp1m)/sp1m[1:(length(sp1m)-1)]
sp2m<-unname(unlist(yfresm[yfresm$ticker==tickers[2],c("price_adjusted")]))
r2m<-diff(sp2m)/sp2m[1:(length(sp2m)-1)]
sp3m<-unname(unlist(yfresm[yfresm$ticker==tickers[3],c("price_adjusted")]))
r3m<-diff(sp3m)/sp3m[1:(length(sp3m)-1)]
spim<-unname(unlist(yfresm[yfresm$ticker=="^GSPC",c("price_adjusted")]))
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

tabnames<-c("tabela 1_1", "tabela 1_2", "tabela 1_3", "tabela 2_1", "tabela 2_2",
            "tabela 2_3", "tabela 2_4", "tabela 3_1")
tabele<-list(t1_1, t1_2, t1_3, t2_1, t2_2, t2_3, t2_4, t3_1)
tabsource<-list(a[5:8,2:7], a[12:15,2:3], a[19:22,2:5], a[26:29,2:7], a[33:36,2:5]
                , a[40:43,2:5], a[47:50,2:5], a[54:57,2:5])
tabfbr<-c("I8", "I15", "I22", "I29", "I33", "I40", "I47", "I54")
tabprec<-c(6,4, 3,5,3,3,3,6)
for (i in length(tabele)){  
  check<-round(tabele[[i]]-unname(unlist(tabsource[[i]])),tabprec[i])
  print(check)
  ok<-sum(check)==0
  if(ok){
    print (paste(tabnames[i], "ok"))
    range_write(gid, range=tabfbr[i], data=data.frame(Sprawdzone="ok"))
  } else {
    print (paste(tabnames[i], "?????"))
    range_write(gid, range=tabfbr[i], data=data.frame(Do_sprawdzenia="????"))
  }
}

exp(252*(v+s^2/2))-1

# library("googlesheets4")
# a<-read_sheet("1uTRluUKvTt3weNZX5vBXq9Sf8_MUh3CoDg-sVjYdDkg", sheet="Wzór")
# View(a)
t1_1
