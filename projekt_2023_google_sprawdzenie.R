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

library("googlesheets4")
options(scipen=999)

gid<-'1fOGTyZOsn5FfFDuNcg82onoRZ_9CvGW1Xu-_y6Gv6KY'

gs<-'Lista'

a<-read_sheet(gid, sheet=gs)

students <-c(13, 18)

for (student in students) {print(student)

tickersc<-as.character(a[student+1,5:7])
tickersc<-paste0(substring(tickersc,1,3),".WA")

yfres<-yfR::yf_get(tickersc, first_date = "2014-12-30", last_date = "2023-01-01", 
                   freq_data="daily", do_cache=FALSE)
yfres<-yfres[!(yfres$ref_date %in% c(dates_delete, dates_delete2)),]
yfresm<-yfR::yf_get(tickersc, first_date = "2014-12-01", last_date = "2023-01-01", 
                    freq_data="monthly", do_cache=FALSE)

library(dplyr)
counts<-unname(unlist((yfres %>% group_by(ticker) %>% summarize(count = n()))[,2]))

print(sort(tickersc)[counts!=2001])
}
#where <- cell_limits(c(student+1, 8), c(student+1, 8))
#range_write(gid, sheet=gs, range=where, data="??")

#yfres %>% group_by(ref_date) %>% summarize(count = n()) %>% filter(count!=3)


r1<-diff(yfres$price_adjusted)[1:2000]/yfres$price_adjusted[1:2000]
r2<-diff(yfres$price_adjusted)[2002:4001]/yfres$price_adjusted[2002:4001]
r3<-diff(yfres$price_adjusted)[4003:6002]/yfres$price_adjusted[4003:6002]
ri<-diff(wig$Close)[1:2000]/wig$Close[1:2000]

mean(r1)
mean(r2)
mean(r3)
mean(ri)
sd(r1)
sd(r2)
sd(r3)
sd(ri)
  
c(dates_delete, dates_delete2)[order(c(dates_delete, dates_delete2))]

