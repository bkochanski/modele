tickersc<-c('PKO.WA', 'PZU.WA', 'LPP.WA')

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


wig<-read.csv2("wig.csv")

yfres<-yfR::yf_get(tickersc, first_date = "2014-12-30", last_date = "2023-01-01", 
                   freq_data="daily", do_cache=FALSE)

# why is 2018-01-30 data missing?
# yfres[yfres$ref_date=="2018-01-30",]
# "2018-01-30" %in% dates_delete
# why is WIG missing?

yfres<-yfres[!(yfres$ref_date %in% dates_delete),]
yfresm<-yfR::yf_get(tickersc, first_date = "2014-12-01", last_date = "2023-01-01", 
                    freq_data="monthly", do_cache=FALSE)

wigm<-read.csv2("wigm.csv")

