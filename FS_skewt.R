##### 1. Pole pod krzywą ####

library(fGarch)

# Parametry rozkładu A-C skewed t:
# położenie:
m <- 0
# skala:
s <- 2
# nachylenie
xi <- 1/1.5
# ogony
nu <- 5000000

# Będziemy obliczać pole pod krzywą gęstości rozkładu Gaussa
# od 
# *można wpisać from <- -Inf, co oznacza minus nieskończoność
from <- -Inf
# do
# *można wpisać to <- Inf, co oznacza (plus) nieskończoność
to <- 2

# Sprawdzenie danych, obliczenie pola pod krzywą
if (from > to) {
  # błąd od > do
  print("!!! Wartość 'od' powinna być niższa od wartości 'do' !!!")
} else {
  
  # Zapis prawdopodobieństwa
  
  if (to==Inf) {
    p=paste0("P(X>", from, ")")
  } else if (from==-Inf) {
    p=paste0("P(X<", to, ")")
  } else {
    p=paste0("P(", from, "<X<", to, ")")
  }
  print(p)
  
  # Obliczenie prawdopodobieństwa, czyli pole pod wycinkiem krzywej:
  result<-fGarch::psstd(to, m, s, nu, xi)-fGarch::psstd(from, m, s, nu, xi)
  print(result)
}

# Rysunek
library(ggplot2)

x1=if(from==-Inf){min(-4*s+m, to-2*s)} else {min(from-2*s, -4*s+m)}
x2=if(to==Inf){max(4*s+m, from+2*s)} else {max(to+2*s, 4*s+m)}

df<-data.frame(y=c(0, 0), 
               x=c(if(from==-Inf){NA}else{from}, if(to==-Inf){NA}else{to}),
               label=c(if(from==-Inf){NA}else{from}, if(to==-Inf){NA}else{to}))

plt<-ggplot(NULL, aes(c(x1, x2))) +
  theme_minimal() +
  xlab('') +
  ylab('') +
  geom_area(stat = "function", 
            fun = function(x){fGarch::dsstd(x, m, s, nu, xi)}, 
            fill = "orange", 
            xlim = c(if(from==-Inf){x1}else{from}, if(to==Inf){x2}else{to})) +
  geom_line(stat = "function", fun = function(x){fGarch::dsstd(x, m, s, nu, xi)}, col = "blue", lty=2, lwd=1) +
  scale_x_continuous(breaks=c(m, m-s, m-2*s, m+s, m+2*s, m-3*s, m+3*s, m-4*s, m+4*s)) +
  geom_point(data = df, aes(x=x, y=y), shape=4) +
  geom_text(data = df, aes(x=x, y=y, label=signif(label, 6)), vjust=1.4) +
  annotate("text", label = paste0("m = ", m, "\ns = ", s, "\nnu = ", nu, "\nxi = ", xi, "\n", p, " = ", signif(result,6)), 
           x = x1, y = fGarch::dsstd(m, m, s, nu, xi)*1.2, size = 6, hjust="inward", vjust = "inward")

suppressWarnings(print(plt))
