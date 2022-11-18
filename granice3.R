Sigma<-matrix(c(.09, .045, .01, 
                .045, .25, .06,
                .01, .06, .04), nrow=3)
s<-sqrt(diag(Sigma))
mu<-c(.08, .10, .16)
rf<-.08

library(IntroCompFinR)

ef<-efficient.frontier(mu, Sigma, nport=1000, shorts=FALSE)
efs<-efficient.frontier(mu, Sigma, nport=1000)

erseq<-seq(from=min(mu),to=max(mu), length.out=100)
erseq<-erseq[1:(length(erseq))]

erseq2<-seq(from=0,to=.30, length.out=500)

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
    w<-c(0, 0, 0)
    w[a1]<-v
    w[a2]<-1-v
    sqrt(t(w)%*%Sigma%*%w)})
rz2<-Vectorize(
  function(v, a1, a2){
    w<-c(0, 0, 0)
    w[a1]<-v
    w[a2]<-1-v
    sum(w*mu)})

#sz2(u, 1, 4)
#rz2(u, 1, 4)

ef12<-approxfun(rz2(u, 1, 2), sz2(u, 1, 2))
ef13<-approxfun(rz2(u, 1, 3), sz2(u, 1, 3))
ef23<-approxfun(rz2(u, 2, 3), sz2(u, 2, 3))

#Generowanie losowych portfeli
nsim<-1000
w<-matrix(rep(0, nsim*3), ncol=3)
w[,1]<-(1-(runif(nsim))^(1/2))
w[,2]<-(1-(runif(nsim)))*(1-w[,1])
w[,3]<-(1-w[,1]-w[,2])

port<-data.frame(sigma=rep(0,nsim), er = rep(0, nsim))

for (i in 1:nsim){
  port$sigma[i]<-sqrt(t(w[i,]) %*% Sigma %*% w[i,])
  port$er[i]<-sum(w[i,]*mu)
}

g<-function(x){max(c(ef12(x),ef13(x), ef23(x)), na.rm=TRUE)}
g<-Vectorize(g)
#g(df$er)

dfs$sharpe<- (dfs$er-rf)/dfs$sigma
sh<-max(dfs$sharpe)
dfss<-dfs[which(dfs$sharpe==sh),]

dfss2<-dfs[which(dfs$sharpe==sh):length(dfs$sharpe),]
dfe<-dfs[which(dfs$sigmas==min(dfs$sigmas)):length(dfs$sigmas),]

labels=c('Granica efektywna \nw sytuacji istnienia \naktywa wolnego od ryzyka \ni braku możliwości \nzaciągania długu \npo stopie wolnej \nod ryzyka',
         'Część granicy\nminimalnej wariancji \naktywów ryzykownych \nniebędąca \ngranicą efektywną',
         "Granica \nefektywna \naktywów \nryzykownych",
         'Granica efektywna \nw sytuacji istnienia \naktywa wolnego \nod ryzyka \ni możliwości \nzaciągania długu \npo stopie wolnej \nod ryzyka')

ggplot() + 
  theme_void() +
  geom_line(aes(x=c(0, dfss$sigmas), y=c(rf, dfss$er), colour=labels[1]), size=2) +
  geom_path(aes(x=sigmas, y=er, colour=labels[1]), data=dfss2, size=2) +
  geom_line(aes(x=c(0,.50), y=c(0,0))) +
  geom_polygon(aes(x = c(.48,.48,.50,.48), y = c(-.004,.004,0,-0.004)), fill = "black")+
  geom_line(aes(x=c(0,0), y=c(0,.25))) +
  geom_polygon(aes(x = c(-0.003,.003,0,-0.003), y = c(.23,.23,.25,.23)), fill = "black")+
  geom_path(aes(x=sigmas, y=er, colour=labels[2]), data=dfs, linetype=3, size=.5) +
  geom_path(aes(x=sigmas, y=er, colour=labels[3]), data=dfe) +
  geom_line(aes(x=c(0,(.25-rf)/sh), y=c(rf, .25), colour=labels[4])) +
  geom_point(aes(x=sigmas, y=er), data=dfs[which(dfs$sharpe==sh),]) +
  annotate("text", x=.205, y=.19, label="T")+
  geom_point(aes(x=0, y=rf)) +
  annotate("text", x=-0.01, y=rf, label="R[f]", parse=TRUE)+
  xlim(-0.01, .5) + ylim(-0.005,.25) + xlab("odchylenie standardowe") +
  ylab("oczekiwana stopa zwrotu")+ 
  scale_colour_manual(name='', 
                      breaks=labels,
                      values=c("green", "blue", "blue", "black"),
                      guide=guide_legend(override.aes = list(
                      linetype=c(1, 3, 1, 1), 
                      shape=c(NA, NA, NA, NA), 
                      size=c(1,1,1,1)
                      ))) + 
  theme(legend.position="bottom", 
        legend.box="vertical")

