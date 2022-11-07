Rho<-matrix(c(1, .5,
              .5, 1), 
            nrow=2)
sigma<-c(.12, .1)
mu<-c(.25, .15)

Sigma<-sigma %*% t(sigma) * Rho

IntroCompFinR::globalMin.portfolio(mu, Sigma)

(sigma[2]^2-Sigma[2,1])/(sum(sigma^2)-2*Sigma[2,1])
(sigma[1]^2-Sigma[2,1])/(sum(sigma^2)-2*Sigma[2,1])
