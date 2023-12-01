tickersc<-c('PGE.WA', 'CDR.WA', 'PKO.WA')

# dates_delete<-unique(yfres[yfres$volume==0,]$ref_date)
# saveRDS(dates_delete, "dates_delete.Rds")
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

# c(dates_delete, dates_delete2)[order(c(dates_delete, dates_delete2))]

wig<-read.csv2("wig.csv")

yfres<-yfR::yf_get(tickersc, first_date = "2014-12-30", last_date = "2023-01-01", 
                   freq_data="daily", do_cache=FALSE)

# why is 2018-01-30 data missing?
# yfres[yfres$ref_date=="2018-01-30",]
# "2018-01-30" %in% dates_delete
# why is WIG missing?

yfres<-yfres[!(yfres$ref_date %in% c(dates_delete, dates_delete2)),]
yfresm<-yfR::yf_get(tickersc, first_date = "2014-11-29", last_date = "2023-01-01", 
                    freq_data="monthly", do_cache=FALSE)

wigm<-read.csv2("wigm.csv")[1:97,]

r1<-diff(yfres$price_adjusted)[1:2000]/yfres$price_adjusted[1:2000]
r2<-diff(yfres$price_adjusted)[2002:4001]/yfres$price_adjusted[2002:4001]
r3<-diff(yfres$price_adjusted)[4003:6002]/yfres$price_adjusted[4003:6002]
ri<-diff(wig$Close)[1:2000]/wig$Close[1:2000]

rm1<-diff(yfresm$price_adjusted)[1:96]/yfresm$price_adjusted[1:96]
rm2<-diff(yfresm$price_adjusted)[1:96+97]/yfresm$price_adjusted[1:96+97]
rm3<-diff(yfresm$price_adjusted)[1:96+97*2]/yfresm$price_adjusted[1:96+97*2]
rmi<-diff(wigm$Close)[1:96]/wigm$Close[1:96]

mean(ri)

quantmod::getSymbols(tickersc, src = "yahoo", from = "2014-12-30", to = "2023-01-01", 
                     periodicity="daily")
PGE.WA <- PGE.WA[!zoo::index(PGE.WA) %in% c(dates_delete, dates_delete2)]
CDR.WA <- CDR.WA[!zoo::index(CDR.WA) %in% c(dates_delete, dates_delete2)]
PKO.WA <- PKO.WA[!zoo::index(PKO.WA) %in% c(dates_delete, dates_delete2)]

sp1<-as.vector(eval(parse(text=paste("`",tickersc[1],"`","$","`", tickersc[1],".Adjusted","`", sep=""))))
sp2<-as.vector(eval(parse(text=paste(tickersc[2],"$", tickersc[2],".Adjusted", sep=""))))
sp3<-as.vector(eval(parse(text=paste(tickersc[3],"$", tickersc[3],".Adjusted", sep=""))))
#vspi<-as.vector(eval(parse(text=paste("GSPC$GSPC.Adjusted", sep=""))))

d<-zoo::index(PKO.WA)
d<-d[2:length(d)]
library(lubridate)
pon<-wday(d)==2
r1_<-diff(sp1)/sp1[1:(length(sp1)-1)]
#sp2<-unname(unlist(yfres[yfres$ticker==tickers[2],c("price_adjusted")]))
r2_<-diff(sp2)/sp2[1:(length(sp2)-1)]
#sp3<-unname(unlist(yfres[yfres$ticker==tickers[3],c("price_adjusted")]))
r3_<-diff(sp3)/sp3[1:(length(sp3)-1)]
#spi<-unname(unlist(yfres[yfres$ticker=="^GSPC",c("price_adjusted")]))
#ri<-diff(spi)/spi[1:(length(spi)-1)]

# PZU.WAd<-PZU.WA
# LPP.WAd<-LPP.WA
# PKO.WAd<-PKO.WA

mean(r1)
mean(r2)
mean(r3)
mean(ri)

quantmod::getSymbols(tickersc, src = "yahoo", from = "2014-12-01", to = "2022-12-31", 
                     periodicity="monthly")

sp1m<-as.vector(eval(parse(text=paste("`",tickersc[1],"`","$","`", tickersc[1],".Adjusted","`", sep=""))))
sp2m<-as.vector(eval(parse(text=paste(tickersc[2],"$", tickersc[2],".Adjusted", sep=""))))
sp3m<-as.vector(eval(parse(text=paste(tickersc[3],"$", tickersc[3],".Adjusted", sep=""))))
#spim<-as.vector(eval(parse(text=paste("GSPC$GSPC.Adjusted", sep=""))))

#sp1m<-unname(unlist(yfresm[yfresm$ticker==tickers[1],c("price_adjusted")]))
r1m<-diff(sp1m)/sp1m[1:(length(sp1m)-1)]
#sp2m<-unname(unlist(yfresm[yfresm$ticker==tickers[2],c("price_adjusted")]))
r2m<-diff(sp2m)/sp2m[1:(length(sp2m)-1)]
#sp3m<-unname(unlist(yfresm[yfresm$ticker==tickers[3],c("price_adjusted")]))
r3m<-diff(sp3m)/sp3m[1:(length(sp3m)-1)]

quantile(r3-r3_)
quantile(r1m-rm1)
quantile(r2m-rm2)
quantile(r3m-rm3)

plot(r2m-rm2)
# PZU.WAm<-PZU.WA
# LPP.WAm<-LPP.WA
# PKO.WAm<-PKO.WA
saveRDS(PZU.WAm, "PZU.WAm.rds")
saveRDS(PKO.WAm, "PKO.WAm.rds")
saveRDS(LPP.WAm, "LPP.WAm.rds")


zoo::index(PKO.WAm)

PKOmlink<-read.csv("https://query1.finance.yahoo.com/v7/finance/download/PKO.WA?period1=1417392000&period2=1672444800&interval=1mo&events=history&includeAdjustedClose=true")

quantile(PKO.WAm$PKO.WA.Adjusted - PKOmlink$Adj.Close)

