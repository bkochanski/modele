library("googlesheets4")
a<-read_sheet("1uTRluUKvTt3weNZX5vBXq9Sf8_MUh3CoDg-sVjYdDkg", sheet="Lista")
#View(a)

tickersc<-as.character(na.omit(unique(c(a$Ticker1, a$Ticker2, a$Ticker3, "^GSPC"))))
tickersc<-c('AAPL')
# "ADDYY" %in% tickersc

# getresults<-quantmod::getSymbols(tickersc, src = "yahoo", from = "2013-12-31", to = "2022-01-01", 
#                       periodicity="daily")

yfres<-yfR::yf_get(tickersc, first_date = "2013-12-31", last_date = "2022-01-01", 
                                 freq_data="daily", do_cache=FALSE)
yfresm<-yfR::yf_get(tickersc, first_date = "2013-12-01", last_date = "2022-01-01", 
                   freq_data="monthly", do_cache=FALSE)
#?yf_get

#table(yfres$ticker)
tickersc[!tickersc %in% unique(yfres$ticker)]

mean(table(yfres$ticker)==2016)*100

# for (i in 1:length(tickersc)){
#   print(tickersc[i])
#   try(print(dim(eval(as.name(tickersc[i])))))
# }


