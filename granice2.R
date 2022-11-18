mc<-matrix(c(
1.00, 0.87, 0.07, 0.20,
0.87, 1.00, 0.07, 0.38,
0.07, 0.07, 1.00, 0.19,
0.20, 0.38, 0.19, 1.00), nrow=4)

s<-c(0.06, 0.09, 0.06, 0.08)

mu<-c(0.11, 0.12, 0.06, 0.13)

Sigma<-s%*%t(s)*mc

library(IntroCompFinR)

ef<-efficient.frontier(mu, Sigma, nport=1000, shorts=FALSE)
efs<-efficient.frontier(mu, Sigma, nport=1000)

erseq<-seq(from=min(mu),to=max(mu), length.out=100)
erseq<-erseq[1:(length(erseq))]

erseq2<-seq(from=0,to=.15, length.out=500)

noshorts<-Vectorize(function(x, shorts=FALSE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$sd})
shorts<-Vectorize(function(x, shorts=TRUE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$sd})

df<-data.frame(er=erseq, sigman=noshorts(erseq))
dfs<-data.frame(er=erseq2, sigmas=shorts(erseq2))
df2<-data.frame(mu=mu, s=s)


u<-0:100/100
sz2<-Vectorize(
  function(v, a1, a2){
    w<-c(0, 0, 0, 0)
    w[a1]<-v
    w[a2]<-1-v
    sqrt(t(w)%*%Sigma%*%w)})
rz2<-Vectorize(
  function(v, a1, a2){
    w<-c(0, 0, 0, 0)
    w[a1]<-v
    w[a2]<-1-v
    sum(w*mu)})

#sz2(u, 1, 4)
#rz2(u, 1, 4)

ef12<-approxfun(rz2(u, 1, 2), sz2(u, 1, 2))
ef13<-approxfun(rz2(u, 1, 3), sz2(u, 1, 3))
ef14<-approxfun(rz2(u, 1, 4), sz2(u, 1, 4))
ef23<-approxfun(rz2(u, 2, 3), sz2(u, 2, 3))
ef24<-approxfun(rz2(u, 2, 4), sz2(u, 2, 4))
ef34<-approxfun(rz2(u, 3, 4), sz2(u, 3, 4))

#Generowanie losowych portfeli
nsim<-1000
w<-matrix(rep(0, nsim*4), ncol=4)
w[,1]<-(1-(runif(nsim))^(1/3))
w[,2]<-(1-(runif(nsim))^(1/2))*(1-w[,1])
w[,3]<-(1-(runif(nsim)))*(1-w[,1]-w[,2])
w[,4]<-(1-w[,1]-w[,2]-w[,3])

port<-data.frame(sigma=rep(0,nsim), er = rep(0, nsim))

for (i in 1:nsim){
  port$sigma[i]<-sqrt(t(w[i,]) %*% Sigma %*% w[i,])
  port$er[i]<-sum(w[i,]*mu)
}

g<-function(x){max(c(ef12(x),ef13(x), ef14(x), ef23(x), ef24(x), ef34(x)), na.rm=TRUE)}
g<-Vectorize(g)
#g(df$er)

ggplot() + 
  geom_point(aes(x=sigma, y=er), data=port, colour='grey') +
  geom_point(aes(x=s, y=mu), colour='blue', data=df2) +
  geom_path(aes(x=sigmas, y=er), data=dfs, colour='red') +
  geom_path(aes(x=sigman, y=er), data=df, colour='dark green') +
  geom_path(aes(x=g(df$er), y=df$er), colour='dark green') +
  xlim(0, .15) + ylim(0,.15) + xlab("odchylenie standardowe") +
  ylab("oczekiwana stopa zwrotu")




efficient.portfolio(mu, Sigma, .1)
efficient.portfolio(mu, Sigma, .1, shorts=FALSE)

efficient.portfolio(mu, Sigma, .07)
efficient.portfolio(mu, Sigma, .07, shorts=FALSE)

efficient.portfolio(mu, Sigma, .11)
efficient.portfolio(mu, Sigma, .11, shorts=FALSE)




# ggplot() + 
# #  geom_path(aes(x=sigmas, y=er), data=dfs, colour='red') +
#   geom_path(aes(x=sigmas, y=er), data=dfs[dfs$er>dfs$er[wh],], colour='blue') +
#   xlim(0, .15) + ylim(0,.20) + xlab("odchylenie standardowe") +
#   ylab("oczekiwana stopa zwrotu")
# 
# wh<-which(dfs$sigmas==min(dfs$sigmas))
# dfs$er[wh]

