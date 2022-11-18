fin2<-NULL
while(is.null(fin2)){
  
# mc<-matrix(c(1, .62, .25, .39,
#              .62, 1, .90, .23,
#              .25, .90, 1, .1,
#              .39, .23, .1, 1), nrow=4)
# mu<-c(.05, .11, .13, .8)
# s<-c(.11, .14, .14, .06)

# mc<-matrix(c(
# 1.00000000, 0.17871898, 0.12099789, 0.02713642,
# 0.17871898, 1.00000000, 0.05166505, 0.67170042,
# 0.12099789, 0.05166505, 1.00000000, 0.71483341,
# 0.02713642, 0.67170042, 0.71483341, 1.00000000), nrow=4)
# s<-c(0.07756130, 0.05713326, 0.05649660, 0.05157271)
# mu<-c(0.10761965, 0.06678649, 0.07265202, 0.06337917)
# 
# mc<-round(mc,2)
# s<-round(s, 3)
# mu<-round(mu, 3)

k<-4
fin<-NULL
while(is.null(fin)){
mt<-matrix(rnorm(k^2), nrow=k)
mc<-cov2cor(mt%*%t(mt))
# mc
# sum(mc<0)
#if (!sum(mc<0)==0) {fin<-1}
if (!(mean(mc)<.2)& sum(mc<0)==0) {fin<-1}
}
mu<-runif(k, .05, .15)
s<-runif(k, .05, .15)

mu<-round(mu, 2)
s<-round(s, 2)
mc<-round(mc, 2)
Sigma<-s%*%t(s)*mc

library(IntroCompFinR)

ef<-efficient.frontier(mu, Sigma, nport=1000, shorts=FALSE)
efs<-efficient.frontier(mu, Sigma, nport=1000)

erseq<-seq(from=min(mu),to=max(mu), length.out=100)
erseq<-erseq[2:(length(erseq)-1)]

noshorts<-Vectorize(function(x, shorts=FALSE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$sd})
shorts<-Vectorize(function(x, shorts=TRUE)
{efficient.portfolio(mu, Sigma, target.return=x, shorts=shorts)$sd})

df<-data.frame(er=erseq, sigman=noshorts(erseq), sigmas=shorts(erseq))
df2<-data.frame(mu=mu, s=s)

#View(df)
#min(df$sigman-df$sigmas)

if ((min(df$sigman-df$sigmas)>.002) & (max(df$sigman-df$sigmas)<.02)) {fin2<-1}
}

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

sz2(u, 1, 4)
rz2(u, 1, 4)

ef12<-approxfun(rz2(u, 1, 2), sz2(u, 1, 2))
ef13<-approxfun(rz2(u, 1, 3), sz2(u, 1, 3))
ef14<-approxfun(rz2(u, 1, 4), sz2(u, 1, 4))
ef23<-approxfun(rz2(u, 2, 3), sz2(u, 2, 3))
ef24<-approxfun(rz2(u, 2, 4), sz2(u, 2, 4))
ef34<-approxfun(rz2(u, 3, 4), sz2(u, 3, 4))

g<-function(x){max(c(ef12(x),ef13(x), ef14(x), ef23(x), ef24(x), ef34(x)), na.rm=TRUE)}
g<-Vectorize(g)
g(df$er)

ggplot() + geom_point(aes(x=s, y=mu), colour='blue', data=df2) +
  geom_path(aes(x=sigmas, y=er), data=df, colour='red') +
  geom_path(aes(x=sigman, y=er), data=df, colour='green') +
  geom_path(aes(x=g(df$er), y=df$er), colour='dark green') +
  xlim(0, .15) + ylim(0,.15)


sum(mc<.1)
sum(mc<0)  
mc
s
mu

df2



