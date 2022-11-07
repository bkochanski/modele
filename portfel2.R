Rho<-matrix(c(1, 0.0858696284380679, 0.00640949741225064, 0.0878821538413691,
                 0.0858696284380679, 1, 0.0518411915326102, 0.124957887592895,
                 0.00640949741225064, 0.0518411915326102, 1, 0.0129138381075416,
                 0.0878821538413691, 0.124957887592895, 0.0129138381075416, 1), 
               nrow=4)
sigma<-c(0.0290248376824625, 0.0167670365768345, 0.0568742568485702, 0.0152322321196229)
mu<-c(0.00251488528344351, .00113736483080366, 0.00393165700360302, 0.000785521545466652)

Sigma<-sigma %*% t(sigma) * Rho

library(IntroCompFinR)
#?efficient.portfolio

eps1<-Vectorize(function(x, shorts=FALSE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$sd})


eps2<-Vectorize(function(x, shorts=TRUE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$sd})

#View(data.frame(noshorts=eps1(1:13/100), shorts=eps2(1:13/100)))

nsim<-1000
w<-matrix(rep(0, nsim*4), ncol=4)

w[,1]<-(1-(runif(nsim))^(1/3))*(1)
w[,2]<-(1-(runif(nsim))^(1/2))*(1-w[,1])
w[,3]<-(1-(runif(nsim)))*(1-w[,1]-w[,2])
w[,4]<-(1-w[,1]-w[,2]-w[,3])

port<-data.frame(sigma=rep(0,nsim), er = rep(0, nsim))

for (i in 1:nsim){
  port$sigma[i]<-sqrt(t(w[i,]) %*% Sigma %*% w[i,])
  port$er[i]<-sum(w[i,]*mu)
  # port$sigmal[i]<-sqrt(t(wl[i,]) %*% Sigma %*% wl[i,])
  # port$erl[i]<-sum(wl[i,]*mu)
}


ef<-efficient.frontier(mu, Sigma, nport=1000, shorts=FALSE)
efs<-efficient.frontier(mu, Sigma, nport=1000)
ers<-seq(min(mu), max(mu), (max(mu)-min(mu))/1000)[1:999]
ef2<-eps1(ers)


ggplot() + geom_point(aes(x=sigma, y=er), data=port, colour="grey") +
  geom_path(aes(x=ef2, y=ers), colour='blue') +
  geom_path(aes(x=efs$sd, y=efs$er), colour='red') +
  geom_point(aes(x=sigma, y=mu), colour='green', size=2) +
  xlab("odchylenie standardowe") +
  ylab("oczekiwany zwrot") 
