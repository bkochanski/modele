# library("googlesheets4")
# a<-read_sheet("1CW_Z0Vd8BQX9Brz30Ca_HrRWwsyS835My8NcwF7Hs0s", sheet="Sheet2")
# save(a, file="a.rda")
load("a.rda")

plus<-0
includeBills<-0
end<-7+includeBills
endr<-24+includeBills

Rho<-as.matrix(a[20:endr+plus,3:end])
sigma<-as.numeric(a[18+plus,3:end])
mu<-as.numeric(a[17+plus,3:end])

Sigma<-sigma %*% t(sigma) * Rho

library(IntroCompFinR)
#?efficient.portfolio

eps1<-Vectorize(function(x, shorts=FALSE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$sd})

eps1part<-Vectorize(function(x, a1, a2, shorts=FALSE)
{efficient.portfolio(mu[c(a1, a2)], Sigma[c(a1, a2),c(a1, a2)], target.return=x, shorts=shorts)$sd})

eps2<-Vectorize(function(x, shorts=TRUE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$sd})

#View(data.frame(noshorts=eps1(1:13/100), shorts=eps2(1:13/100)))

nsim<-1000
w<-matrix(rep(0, nsim*6), ncol=6)

w[,1]<-(1-(runif(nsim))^(1/5))*includeBills
w[,2]<-(1-(runif(nsim))^(1/4))*(1-w[,1])
w[,3]<-(1-(runif(nsim))^(1/3))*(1-w[,1]-w[,2])
w[,4]<-(1-(runif(nsim))^(1/2))*(1-w[,1]-w[,2]-w[,3])
w[,5]<-(1-(runif(nsim)))*(1-w[,1]-w[,2]-w[,3]-w[,4])
w[,6]<-(1-w[,1]-w[,2]-w[,3]-w[,4]-w[,5])

w<-w[,(1+(1-includeBills)):6]

#mean(rowSums(w))

# wl<-w*3-1
# wl[,6]<-(1-wl[,1]-wl[,2]-wl[,3]-wl[,4]-wl[,5])


port<-data.frame(sigma=rep(0,nsim), er = rep(0, nsim))

for (i in 1:nsim){
  port$sigma[i]<-sqrt(t(w[i,]) %*% Sigma %*% w[i,])
  port$er[i]<-sum(w[i,]*mu)
  # port$sigmal[i]<-sqrt(t(wl[i,]) %*% Sigma %*% wl[i,])
  # port$erl[i]<-sum(wl[i,]*mu)
}


ef<-efficient.frontier(mu, Sigma, nport=1000, shorts=FALSE)
efs<-efficient.frontier(mu, Sigma, nport=1000)
ers<-seq(min(mu), max(mu), (max(mu)-min(mu))/1000)
ef2<-eps1(ers)

r12<-ers[ers<=mu[1]&ers>=mu[2]|ers>=mu[1]&ers<=mu[2]]
f12<-eps1part(r12,1,2)
d12<-data.frame(f12, r=r12)

r13<-ers[ers<=mu[1]&ers>=mu[3]|ers>=mu[1]&ers<=mu[3]]
f13<-eps1part(r13,1,3)
d13<-data.frame(f13, r=r13)

r14<-ers[ers<=mu[1]&ers>=mu[4]|ers>=mu[1]&ers<=mu[4]]
f14<-eps1part(r14,1,4)
d14<-data.frame(f14, r=r14)

r15<-ers[ers<=mu[1]&ers>=mu[5]|ers>=mu[1]&ers<=mu[5]]
f15<-eps1part(r15,1,5)
d15<-data.frame(f15, r=r15)

r23<-ers[ers<=mu[2]&ers>=mu[3]|ers>=mu[2]&ers<=mu[3]]
f23<-eps1part(r23,2,3)
d23<-data.frame(f23, r=r23)

r24<-ers[ers<=mu[2]&ers>=mu[4]|ers>=mu[2]&ers<=mu[4]]
f24<-eps1part(r24,2,4)
d24<-data.frame(f24, r=r24)

r25<-ers[ers<=mu[2]&ers>=mu[5]|ers>=mu[2]&ers<=mu[5]]
f25<-eps1part(r25,2,5)
d25<-data.frame(f25, r=r25)

r34<-ers[ers<=mu[3]&ers>=mu[4]|ers>=mu[3]&ers<=mu[4]]
f34<-eps1part(r34,3,4)
d34<-data.frame(f34, r=r34)

r35<-ers[ers<=mu[3]&ers>=mu[5]|ers>=mu[3]&ers<=mu[5]]
f35<-eps1part(r35,3,5)
d35<-data.frame(f35, r=r35)

r45<-ers[ers<=mu[4]&ers>=mu[5]|ers>=mu[4]&ers<=mu[5]]
f45<-eps1part(r45,4,5)
d45<-data.frame(f45, r=r45)

df<-Reduce(function(x, y) merge(x, y, all=TRUE), list(d12, d13, d14, d15, d23, d24, d25, d34, d35, d45))
#View(df)
df2<-data.frame(r=df[,1], s=apply(df[,2:11],1,function(x){max(x, na.rm=TRUE)}))
#View(df2)

  
# tmp<-c(1,2)
# ef12<-efficient.frontier(mu[tmp], Sigma[tmp,tmp], nport=1000, shorts=FALSE)
# tmp<-c(1,3)
# ef13<-efficient.frontier(mu[tmp], Sigma[tmp,tmp], nport=1000, shorts=FALSE)
# tmp<-c(1,4)
# ef14<-efficient.frontier(mu[tmp], Sigma[tmp,tmp], nport=1000, shorts=FALSE)
# tmp<-c(1,5)
# ef15<-efficient.frontier(mu[tmp], Sigma[tmp,tmp], nport=1000, shorts=FALSE)
# tmp<-c(2,3)
# ef23<-efficient.frontier(mu[tmp], Sigma[tmp,tmp], nport=1000, shorts=FALSE)
# tmp<-c(2,4)
# ef24<-efficient.frontier(mu[tmp], Sigma[tmp,tmp], nport=1000, shorts=FALSE)
# tmp<-c(2,5)
# ef25<-efficient.frontier(mu[tmp], Sigma[tmp,tmp], nport=1000, shorts=FALSE)
# tmp<-c(3,4)
# ef34<-efficient.frontier(mu[tmp], Sigma[tmp,tmp], nport=1000, shorts=FALSE)
# tmp<-c(3,5)
# ef35<-efficient.frontier(mu[tmp], Sigma[tmp,tmp], nport=1000, shorts=FALSE)
# tmp<-c(4,5)
# ef45<-efficient.frontier(mu[tmp], Sigma[tmp,tmp], nport=1000, shorts=FALSE)



library(ggplot2)
library(ggrepel)

ggplot() + geom_point(aes(x=sigma, y=er), data=port, colour="grey") +
#  geom_line(aes(x=eps1(1:13/100), y=1:13/100), colour='blue') +
#  geom_line(aes(x=eps2(1:13/100), y=1:13/100), colour='red') +
  # geom_path(aes(x=ef$sd, y=ef$er), colour='blue') +
  geom_path(aes(x=ef2, y=ers), colour='blue') +
  geom_path(aes(x=efs$sd, y=efs$er), colour='red') +
  geom_point(aes(x=sigma, y=mu), colour='dark green', size=2) +
  geom_path(aes(x=df2$s, y=df2$r), colour='dark green', size=0.1) +
  # geom_path(aes(x=ef13$sd, y=ef13$er), colour='light grey', size=0.01) +
  # geom_path(aes(x=ef14$sd, y=ef14$er), colour='green', size=0.01) +
  # geom_path(aes(x=ef15$sd, y=ef15$er), colour='green', size=0.01) +
  # geom_path(aes(x=ef23$sd, y=ef23$er), colour='green', size=0.01) +
  # geom_path(aes(x=ef24$sd, y=ef24$er), colour='green', size=0.01) +
  # geom_path(aes(x=ef25$sd, y=ef25$er), colour='green', size=0.01) +
  # geom_path(aes(x=ef34$sd, y=ef34$er), colour='green', size=0.01) +
  # geom_path(aes(x=ef35$sd, y=ef35$er), colour='green', size=0.01) +
  # geom_path(aes(x=ef45$sd, y=ef45$er), colour='green', size=0.01) +
  ylim(c(0,.15)) + 
  # xlim(c(0.02, .35)) +
  xlab("odchylenie standardowe") +
  ylab("oczekiwany zwrot") +
  geom_text_repel(aes(x=sigma, y=mu, label=c("Akcje większych spółek",	
                                             "Akcje mniejszych spółek",
                                             "Obligacje korporacyjne",
                                             "Długoterminowe obligacje rządowe",
                                             "Średnioterminowe obligacje rządowe")))

# min(mu)
# 
# names(mu)=c(colnames(Sigma))
# globalMin.portfolio(mu, Sigma, shorts=FALSE)

#+
#  geom_point(aes(x=sigmal, y=erl), col='blue')

#plot(wl[,1], wl[,2])

#plot(port, pch=16)

# library(PortfolioAnalytics)
# ?optimize.portfolio

#efs$weights

efs$sd[round(efs$sd,3)==0.2]
efs$weights[405,]

colnames(Sigma)

ef$sd[round(ef$sd,3)==0.2]
ef$weights[646,]
