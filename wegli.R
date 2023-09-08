tickers<-c('BHW.WA', 'ATT.WA', 'CPS.WA', 'ENG.WA', 'GTN.WA', 'ING.WA',
           'LPP.WA', 'LWB.WA', 'MBK.WA', 'MIL.WA', 'OPL.WA', 'PGE.WA',
           'PKN.WA', 'PKO.WA', 'PZU.WA', 'SPL.WA', 'PKN.WA', 'KGH.WA', 
           'PEO.WA', 'TPE.WA')

quantmod::getSymbols(tickers, src = "yahoo", from = "2013-11-30", to = "2022-11-30", 
                     periodicity="monthly")

mylist<-list()

for (i in 1:(length(tickers))){
  mylist[[i]]<-as.vector(eval(parse(text=paste(tickers[i],"$", tickers[i],".Adjusted", sep=""))))
  print(i)
  }


