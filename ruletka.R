p<-c(1/2, 1/3, 1/6)
w1<-c(3, -1, -1)
w2<-c(-1, 2, -1)
w3<-c(-1, -1, 6)
E1<-sum(p*w1)
E2<-sum(p*w2)
E3<-sum(p*w3)
V1<-sum(p*(w1-E1)^2)
V2<-sum(p*(w2-E2)^2)
V3<-sum(p*(w3-E3)^2)
r12<-sum(p*(w1-E1)*(w2-E2))/sqrt(V1*V2)
r23<-sum(p*(w2-E2)*(w3-E3))/sqrt(V2*V3)
r13<-sum(p*(w1-E1)*(w3-E3))/sqrt(V1*V3)

Rho<-matrix(c(1, r12, r13,
              r12, 1, r23,
              r13, r23, 1), 
            nrow=3)
sigma<-c(V1, V2, V3)^.5
mu<-c(E1, E2, E3)
Sigma<-sigma %*% t(sigma) * Rho

weights<-gtools::rdirichlet(1000, c(1,1,1))

Row<-function(x){
c(
sigma1<-as.numeric(sqrt(t(weights[x,]) %*% Sigma %*% weights[x,])),
mu1<-sum(weights[x,]*mu))}
Row<-Vectorize(Row)
simpoints<-t(Row(1:1000))
plot(simpoints, xlab='sigma', ylab='mu', pch=16, cex=1.5-weights[,2]+.2, col=rgb(weights[,1],0,0,.4))

row1<-c(.5,0,.5)
sigmax<-function(x){as.numeric(sqrt(t(x) %*% Sigma %*% x))}
mux<-function(x){sum(x*mu)}

points(sigmax(row1), mux(row1), col='blue', pch=16)

row2<-c(.7,.1,.2)
points(sigmax(row2), mux(row2), col='lightblue', pch=16)

rowA<-c(1,0,0)
points(sigmax(rowA), mux(rowA), col='green', pch=16)
rowB<-c(0,1,0)
points(sigmax(rowB), mux(rowB), col='green', pch=16)
rowC<-c(0,0,1)
points(sigmax(rowC), mux(rowC), col='green', pch=16)

