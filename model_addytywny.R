v<-c(0,2,2,2,5,6,6,20)

e1071::kurtosis(v)
e1071::kurtosis(v, type=1)
e1071::kurtosis(v, type=2)
e1071::kurtosis(v, type=3)

u<-replicate(100, rnorm(1))
hist(u)

s0<-100
a<-1.05

N=20

results<-replicate(10000, 
{
u<-rnorm(N,0,2)
s<-c(s0, rep(0,N))
for(i in 1:(N+1)){s[i+1]=a*s[i]+u[i]}
s[N+1]/s[1]
})

mean(results)
var(results)
s0*a^(N)
sum(a^(2*0:(N-1)))*2^2/100^2

#https://www.wolframalpha.com/input?i=sum+of+a+series&assumption=%7B%22F%22%2C+%22Sum%22%2C+%22sumlowerlimit%22%7D+-%3E%220%22&assumption=%7B%22C%22%2C+%22sum+of+a+series%22%7D+-%3E+%7B%22Calculator%22%7D&assumption=%7B%22F%22%2C+%22Sum%22%2C+%22sumfunction%22%7D+-%3E%22x%5E%282k%29%22&assumption=%7B%22F%22%2C+%22Sum%22%2C+%22sumvariable%22%7D+-%3E%22k%22&assumption=%7B%22F%22%2C+%22Sum%22%2C+%22sumupperlimit2%22%7D+-%3E%22infinity%22

(a^(2*N)-1)/(a^2-1)

#mean(replicate(100,var(rnorm(1000))))

var(results)/sum(a^(0:(N-1)))


hist(results, breaks=100)
