#apple daily
afres<-yfR::yf_get(c('AAPL'), first_date = "2013-12-31", last_date = "2023-01-01", 
                   freq_data="daily", do_cache=FALSE)
afresm<-yfR::yf_get(c('AAPL'), first_date = "2013-12-01", last_date = "2023-01-01", 
                    freq_data="monthly", do_cache=FALSE)
write.csv2(afres, paste0('afres',today(),'.csv'))
write.csv2(afresm, paste0('afresm',today(),'.csv'))
?yf_get
