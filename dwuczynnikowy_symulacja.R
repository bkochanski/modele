F1<-rnorm(100)
F2<-rnorm(100)

R<-.5*F1+.8*F2+rnorm(100)

df<-data.frame(R, F1, F2)

s<-summary(lm(R~F1+F2, data=df))


