library(ggplot2)
ar<-data.frame(x0=c(0,0,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3)+0.05,
                   y0=c(0,0,1,1,-1,-1,2,2,0,0,-2,-2,3,3,1,1,-1,-1,-3,-3),
                   xend=c(1,1,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,4,4)-0.05,
                   yend=c(1,-1,2,0,0,-2,3,1,1,-1,-1,-3,4,2,2,0,0,-2,-2,-4))

ar$xp<-(ar$x0+ar$xend)/2
ar$yp<-(ar$y0+ar$yend)/2
ar$labelp<-rep(c('p', '1-p'),10)

po<-data.frame(x0=c(0,1,1,2,2,2,3,3,3,3,4,4,4,4,4)
                   , y0=c(0,1,-1,2,0,-2,3,1,-1,-3,4,2,0,-2,-4)
              , label0=c('S[0]', 'S[0]*u', 'S[0]*d', 
                         'S[0]*u^2', 'S[0]*ud', 'S[0]*d^2', 
                         'S[0]*u^3', 'S[0]*u^2*d', 'S[0]*u*d^2', 'S[0]*d^3', 
                         'S[0]*u^4', 'S[0]*u^3*d', 'S[0]*u^2*d^2', 'S[0]*u*d^3', 'S[0]*d^4'))


ggplot(aes(x=x0, y=y0+.4), data=po) + 
  theme_void() +
  geom_segment(aes(x = x0, y = y0, xend = xend, yend = yend),
               data=ar, 
               arrow = arrow(type='closed', angle=7, length=unit(.15,'inch'))) +
  geom_point(aes(x=x0, y=y0), data=po) +
  geom_text(aes(label=label0), parse=TRUE) +
  geom_text(aes(x=xp, y=yp+.3, label=labelp), data=ar, col='grey')

             