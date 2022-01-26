##################
# plotting zoom
#################

p<- ggplot() +
  geom_point(data = df, aes(x = data, y = nuovi_positivi, color="nuovi positivi", group = 1),col=c("#fc6b03","#cfcaca","#f2d729","#b3190b")[unclass(as.factor(df$color))])+ 
  geom_line(data = df, aes(x = data, y = predict(glm_mod1,type="response"), color = "model 1", group = 1)) + labs(x = "Time", y = "New positives") +
  scale_x_date(date_breaks = '5 days',date_labels = '%Y-%m-%d') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
d1<-df$data[112]
d2<-df$data[123]
g <- ggplot(data = df)+
  geom_point( aes(x = data, y = nuovi_positivi, color="nuovi positivi", group = 1),col=c("#fc6b03","#cfcaca","#f2d729","#b3190b")[unclass(as.factor(df$color))])+
  geom_line(data = df, aes(x = data, y = predict(glm_mod1,type="response"), color = "model 1", group = 1)) +
  scale_x_date(limits = c(d1, d2)) + scale_y_continuous(limits = c(600, 2000)) + theme_bw()+
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    #plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    #panel.grid.major = element_blank(), # get rid of major grid
    #panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg,
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.margin = margin(t = 0,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 0) # Left margin
  )

d3<-df$data[80]
d4<-df$data[123]
h<-  p+ annotation_custom(ggplotGrob(g), xmin =d3 , xmax =d4  ,ymin = 0, ymax = 1000)