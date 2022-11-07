library(ggplot2)
library(ggrepel)

ggplot() + geom_point(aes(x=c(.1,.2), y=c(.1,.2), colour="A")) +
geom_path(aes(x=c(.1,.2), y=c(.15,.15), colour='B')) +
  geom_path(aes(x=c(.11,.22), y=c(.16,.16), colour='C')) +
  geom_point(aes(x=c(.12,.23), y=c(.17,.17), colour='D'), size=2)+
  scale_colour_manual(name='', 
                      breaks=c('A', 'B', 'C', 'D'),
                      values=c("grey", "blue", "red", "dark green"),
                      guide=guide_legend(override.aes = list(
                        linetype=c("blank", "solid", "solid", "blank"),
                        shape=c(16, NA, NA, 16)
                      )))
