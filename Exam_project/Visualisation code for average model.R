#create new data frame
train_sample.1<- train_sample %>% 
  select(name,transfer.fee,Status,club.to) 
train_sample.1<- train_sample.1%>% 
  mutate(index=1:496)

#creating GGplot
p = ggplot(train_sample.1, aes(x = index , y = transfer.fee))+
  geom_segment(aes(x= index, xend=index, y=transfer.fee, yend=estimate_M1), color="red") +
  geom_point(aes(x = index, y = transfer.fee,text = paste(name, " to ", club.to)), color = "black")   +
  geom_line(aes(x = index, y = estimate_M1), color="green", size =1)

#adding theme
p<-p+theme(axis.title.x=element_blank(),
           axis.text.x =element_blank(),
           axis.ticks= element_line(color=NA),
           panel.grid.major.y = element_line(colour="#CACACA", size=0.2),
           panel.grid.major.x = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           text=element_text(family="Goudy Old Style"))
p
#using plotly to make it interactive
gg <- ggplotly(p)  
gg
