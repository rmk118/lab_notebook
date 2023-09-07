library(tidyverse)

dat_sid<- read.csv("~/Downloads/data_graphing.csv")

ggplot(data=dat_sid)+
  geom_line(aes(y=DEPTH..meters., x=Temperature))+
  scale_y_reverse()+
  scale_x_continuous(name="",labels=NULL,
                     sec.axis = sec_axis( trans=~.*1, name="Temperature"))



ggplot(data=dat_sid, aes(x=DEPTH..meters.))+
  geom_line(aes(y=Dissolved..Oxygen), color="blue", size=1.2)+
  geom_line(aes(y=Dissolved..Oxygen.1/14), color="red", size=1.2)+
  scale_y_continuous(name="DO (mg/L)",
                     sec.axis = sec_axis( trans=~.*14, name="DO (% sat)"))+
  theme_bw()+xlab("Depth (m)")+
  theme(
    axis.title.y = element_text(color = "blue", margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.y.right = element_text(color = "red", margin = margin(t = 0, r = 0, b = 0, l = 10)),
    axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
    text = element_text(size = 15)
  )
