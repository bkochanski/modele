S0=1
nu = .01
sigma=.05
u<-rnorm(120, nu, sigma)
eu<-exp(u)
#ceu<-S0*cumprod(eu)
ceu<-S0*cumprod(exp(u))
ur<-log(ceu/c(1,ceu[-length(ceu)]))
mean(u)
mean(ur)
#eu[1]*eu[2]*eu[3]/ceu[3]
plot(ceu, ylim=c(0,15), pch='.')
lines(ceu)
(1:5)[-5]

r<-replicate(10000,(S0*cumprod(exp(rnorm(120, nu, sigma))))[120]-1)
hist(r, breaks=100)
mean(r)
sd(r)
mean(log(1+r))
bnu<-nu*120
sd(log(1+r))
bsigma<-sigma*sqrt(120)

exp(bnu+bsigma^2/2)-1

mean(r)
(m1<-exp(120*(nu+sigma^2/2))-1)
sd(r)
(s1<-exp(120*(nu+sigma^2/2))*sqrt(exp(120*sigma^2)-1))

mean(log(1+r))
sd(log(1+r))
