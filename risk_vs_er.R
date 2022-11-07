#SBBI

df<-data.frame(asset=c("Large-Cap Stocks",	
                       "Small-Cap Stocks",
                       "Long-term Corp Bonds",	
                       "Long-term Gov't Bonds",
                       "Inter-term Gov't Bonds",
                       "U.S. Treasury Bills"),
               mu=c(8.85,	13.18,	3.42,	3.15,	2.41,	0.56),
               sigma=c(20.01,	31.40,	9.51,	10.95,	6.71,	3.83))
#mu=c(11.95,	16.47,	6.30,	6.02,	5.32,	3.46),
#sigma=c(19.99,	32.02,	8.42,	9.96,	5.65,	3.12))


library(ggplot2)
library(ggrepel)

ggplot(aes(x=sigma, y=mu), data=df) + 
  geom_point() +
  theme_bw() +
  xlim(c(0,35)) +
  ylim(c(0,14)) +
  xlab("odchylenie standardowe") +
  ylab("oczekiwany zwrot") +
  geom_text_repel(aes(label=asset))





library(ggplot2)
library(ggrepel)

ggplot(aes(x=sigma, y=mu), data=data.frame(sigma=c(.05, .10, .22, .07, .10, .05, .04),
                                           mu=c(.06, .10, .15, .20, .15, .12, .10), 
                                           asset=c("A", "B", "C", "D", "E", "F", "G"))) +
  xlim(c(0,.22)) +
  ylim(c(0,.22)) +
  geom_rect(aes(xmin = -Inf, xmax = .1, ymin = .1, ymax = Inf),
            alpha = 1/5,
            fill = " lightblue") +  
  geom_point() +
  theme_bw() +
  xlab("odchylenie standardowe") +
  ylab("oczekiwany zwrot") +
  geom_text_repel(aes(label=asset))


