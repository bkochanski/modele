library(fGarch)

# Input
srednia <- 6.15
odchylenie_s <- 1.218575
sym <- 2
tails <- 5
od <- -Inf
do <- 6.15

# Obliczenia

Pole <- psstd(do, srednia, odchylenie_s, tails, sym) - psstd(od,srednia, odchylenie_s, tails, sym)

# Rysunek
library(ggplot2)
ggplot(data.frame(x=sort(c(od, do, -4:4*odchylenie_s+srednia))), aes(x=x)) +
  geom_function(fun = function(x){dsstd(x, srednia, odchylenie_s, tails, sym)}, colour = "red") +
  stat_function(fun = function(x){dsstd(x, srednia, odchylenie_s, tails, sym)*(x>od & x<do)}, geom = "polygon", color = "blue", fill = "blue", alpha = 0.5) +
  scale_x_continuous(breaks = sort(c(od, do, srednia, srednia-odchylenie_s, srednia+ odchylenie_s)))

# Output

print(Pole)
print(paste0(round(Pole*100,2), '%'))
#sort(c(od, do, srednia, srednia-odchylenie_s, srednia+ odchylenie_s))

test<-rsstd(1000, srednia, odchylenie_s, tails, sym)
hist(test, breaks=50, freq=FALSE)
f<-function(x){dsstd(x, srednia, odchylenie_s, tails, sym)}
curve(f, add=TRUE, col='red', lwd=3, lty=3)

sstdFit(test)

