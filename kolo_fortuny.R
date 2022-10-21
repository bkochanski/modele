library(ggplot2)
library(ggforce)

x1<-seq(-.5, .5, 1/100)
polygon1<-data.frame(x=c(0, x1, 0), 
                     y=c(0, sqrt(1-x1^2), 0))

x2<-seq(.5, 1, 1/100)
polygon2<-data.frame(x=c(0, x2, 0), 
                     y=c(0, sqrt(1-x2^2), 0))

x3<-seq(1, .5, -1/100)
polygon3<-data.frame(x=c(0, x3, 0), 
                     y=c(0, -sqrt(1-x3^2), 0))

x4<-seq(.5, -.5, -1/100)
polygon4<-data.frame(x=c(0, x4, 0), 
                     y=c(0, -sqrt(1-x4^2), 0))


x5<-seq(-.5, -1, -1/100)
polygon5<-data.frame(x=c(0, x5, 0), 
                     y=c(0, -sqrt(1-x5^2), 0))

x6<-seq(-1, -.5, 1/100)
polygon6<-data.frame(x=c(0, x6, 0), 
                     y=c(0, sqrt(1-x6^2), 0))


ggplot()+ theme_void() + 
  geom_polygon(data = polygon1, aes(x = x, y = y), fill = "light blue") +
  geom_polygon(data = polygon2, aes(x = x, y = y), fill = "pink") +
  geom_polygon(data = polygon3, aes(x = x, y = y), fill = "light green") +
  geom_polygon(data = polygon4, aes(x = x, y = y), fill = "lemonchiffon") +
  geom_polygon(data = polygon5, aes(x = x, y = y), fill = "plum1") +
  geom_polygon(data = polygon6, aes(x = x, y = y), fill = "goldenrod1") +
  geom_polygon(aes(x = c(0,-.1/2,.1/2,0), y = c(1.1, 1.2, 1.2, 1.1)), fill = "black") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_line(aes(c(-1,1), c(0,0))) +
  geom_line(aes(c(-.5,.5), c(-sqrt(3)/2,sqrt(3)/2))) +
  geom_line(aes(c(-.5,.5), c(sqrt(3)/2,-sqrt(3)/2))) +
  annotate("text", x = 0, y = .75, label="4") +
  annotate("text", x = cos(pi/6)*.75, y = sin(pi/6)*.75, label="-1") +
  annotate("text", x = cos(pi/6)*.75, y = sin(-pi/6)*.75, label="2") +
  annotate("text", x = 0, y = sin(-pi/2)*.75, label="-1") +
  annotate("text", x = -cos(pi/6)*.75, y = sin(-pi/6)*.75, label="3") +
  annotate("text", x = -cos(pi/6)*.75, y = sin(pi/6)*.75, label="0") +
  coord_fixed()

x1<-seq(-1, 1, 1/200)
polygon1<-data.frame(x=c(0, x1, 0), 
                     y=c(0, sqrt(1-x1^2), 0))

x2<-seq(1, .5, -1/100)
polygon2<-data.frame(x=c(0, x2, 0), 
                     y=c(0, -sqrt(1-x2^2), 0))

x3<-seq(.5, -1, -1/200)
polygon3<-data.frame(x=c(0, x3, 0), 
                     y=c(0, -sqrt(1-x3^2), 0))



ggplot()+ theme_void() + 
  geom_polygon(data = polygon1, aes(x = x, y = y), fill = "light blue") +
  geom_polygon(data = polygon2, aes(x = x, y = y), fill = "pink") +
  geom_polygon(data = polygon3, aes(x = x, y = y), fill = "light green") +
  geom_polygon(aes(x = c(0,-.1/2,.1/2,0), y = c(1.1, 1.2, 1.2, 1.1)), fill = "black") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_line(aes(c(-1,1), c(0,0))) +
  geom_line(aes(c(0,.5), c(0,-sqrt(3)/2))) +
  annotate("text", x = 0, y = .75, label="3", size=8) +
  annotate("text", x = cos(pi/6)*.75, y = sin(-pi/6)*.75, label="6") +
  annotate("text", x = -cos(pi/3)*.75, y = sin(-pi/3)*.75, label="2") +
  coord_fixed()


  
              
              