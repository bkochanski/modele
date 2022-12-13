shapiro.test(1:2999)
library(tseries)
??kurtosis
v=c(1:50,100)
jarque.bera.test(v)
n<-length(v)
S<-e1071::skewness(v, type=1)
K<-e1071::kurtosis(v, type=1)+3
(JB=(n)/6*(S^2+1/4*(K-3)^2))
1-pchisq(JB,2)

# e1071::kurtosis(v, type=1)
# e1071::kurtosis(v, type=2)
# e1071::kurtosis(v, type=3)
# e1071::kurtosis(v)



Kx<-e1071::kurtosis(v, type=2)
Sx<-e1071::skewness(v, type=2)

K2=((Kx*(n-2)*(n-3))/(n-1)-6)/(n+1)+3

S2=Sx*(n-2)/sqrt(n*(n-1))   

K2
K
S2
S
