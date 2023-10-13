##### 1. Pole pod krzywą ####

# Parametry rozkładu Gaussa:
# średnia:
m <- 0
# odchylenie standardowe:
sd <- 2

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
  result<-pnorm(to, m, sd)-pnorm(from, m, sd)
  print(result)
}

# Rysunek
library(ggplot2)

x1=if(from==-Inf){min(-4*sd+m, to-2*sd)} else {min(from-2*sd, -4*sd+m)}
x2=if(to==Inf){max(4*sd+m, from+2*sd)} else {max(to+2*sd, 4*sd+m)}

df<-data.frame(y=c(0, 0), 
               x=c(if(from==-Inf){NA}else{from}, if(to==-Inf){NA}else{to}),
               label=c(if(from==-Inf){NA}else{from}, if(to==-Inf){NA}else{to}))

plt<-ggplot(NULL, aes(c(x1, x2))) +
  theme_minimal() +
  xlab('') +
  ylab('') +
  geom_area(stat = "function", 
            fun = function(x){dnorm(x, m, sd)}, 
            fill = "orange", 
            xlim = c(if(from==-Inf){x1}else{from}, if(to==Inf){x2}else{to})) +
  geom_line(stat = "function", fun = function(x){dnorm(x, m, sd)}, col = "blue", lty=2, lwd=1) +
  scale_x_continuous(breaks=c(m, m-sd, m-2*sd, m+sd, m+2*sd, m-3*sd, m+3*sd, m-4*sd, m+4*sd)) +
  geom_point(data = df, aes(x=x, y=y), shape=4) +
  geom_text(data = df, aes(x=x, y=y, label=signif(label, 6)), vjust=1.4) +
  annotate("text", label = paste0("M = ", m, "\nSD = ", sd, "\n", p, " = ", signif(result,6)), 
           x = x1, y = dnorm(m, m, sd)*1.2, size = 6, hjust="inward", vjust = "inward")

suppressWarnings(print(plt))
