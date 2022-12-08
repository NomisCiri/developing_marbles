library(ggformula)
library(RColorBrewer)

tibble(x=seq(0,1,length.out = 100))%>%
  mutate(xneg=-0.5*x+2,xpos=2*x+2)%>%
  mutate(xpos=case_when(xpos>2.7~2.7,TRUE~xpos),
         xneg=case_when(xneg<1.6~1.6,TRUE~xneg))%>%
  mutate(xina=xneg*xpos)%>%
  pivot_longer(cols =c("xneg","xpos"))%>%ggplot(aes(x=x,y=value,color=name))+
  geom_spline(nknots=5,size=4)+
  scale_color_manual(name="",breaks=c("xpos","xneg"),labels=c("Socioemotional Factors","Reward Sensitivity"),values=brewer.pal(3,"Dark2")[c(1,3)])+
  scale_x_continuous(name="Age",breaks=seq(0,1,length.out = 5),labels=seq(10,26,length.out = 5))+
  scale_y_continuous(name="Weight [au]")+
  theme_cowplot()->one

tibble(x=seq(0,1,length.out = 100))%>%
  mutate(xneg=-0.5*x+2,xpos=2*x+2)%>%
  mutate(xpos=case_when(xpos>2.7~2.7,TRUE~xpos),
         xneg=case_when(xneg<1.6~1.6,TRUE~xneg))%>%
  mutate(xina=xneg*xpos)%>%
  pivot_longer(cols =c("xina"))%>%ggplot(aes(x=x,y=value,color=name))+
  geom_spline(nknots=5,size=4)+
  scale_color_manual(name="",breaks=c("xina"),labels=c("Socioemotional Factors * \nReward Sensitvity"),values=brewer.pal(3,"Dark2")[2])+
  scale_x_continuous(name="Age",breaks=seq(0,1,length.out = 5),labels=seq(10,26,length.out = 5))+
  scale_y_continuous(name="Weight [au]")+
  theme_cowplot()->two


panel<-plot_grid(one,NULL,two,labels=c("a","","b"),rel_widths = c(1,0.1,1),ncol=3)
ggsave(filename = "PhDDiscussion_Panel2.png",width=14,height=4)
