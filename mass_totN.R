massreg<-read.table("C:/Users/acahill/Desktop/massreg.txt",header=TRUE)
asutotals<-read.table("C:/Users/acahill/Desktop/asutotals.txt",header=TRUE)

#plotting total biomass by sea

ggplot(massreg, aes(x=Sea, y=Mass,color=Sea))+ 
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.2), cex=6)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  xlab("\nLocation")+ylab("Biomass (g)\n")+
  ylim(0,190)+
  scale_colour_manual(values=c("darkorange2","blue","gold","purple","green","black","red"))+
  scale_x_discrete(labels=c("Baltic","Channel","Biscay","Gulf of Lions","Adriatic","Black","Red"))+
  annotate("text", x = 1, y = 100, label = "ab", size = 6)+
  annotate("text", x = 2, y = 50, label = "ab", size = 6)+
  annotate("text", x = 3, y = 50, label = "ab", size = 6)+
  annotate("text", x = 4, y = 50, label = "a", size = 6)+
  annotate("text", x = 5, y = 160, label = "b", size = 6)+
  annotate("text", x = 6, y = 185, label = "c", size = 6)+
  annotate("text", x = 7, y = 50, label = "a", size = 6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#plotting total number by sea

ggplot(asutotals, aes(x=Sea, y=total_N,color=Sea))+ 
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.2), cex=6)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  xlab("\nLocation")+ylab("Total individuals\n")+
  ylim(0,10500)+
  scale_colour_manual(values=c("darkorange2","blue","gold","purple","green","black","red"))+
  scale_x_discrete(labels=c("Baltic","Channel","Biscay","Gulf of Lions","Adriatic","Black","Red"))+
  annotate("text", x = 1, y = 3500, label = "a", size = 6)+
  annotate("text", x = 2, y = 5500, label = "a", size = 6)+
  annotate("text", x = 3, y = 10250, label = "a", size = 6)+
  annotate("text", x = 4, y = 2500, label = "a", size = 6)+
  annotate("text", x = 5, y = 3000, label = "a", size = 6)+
  annotate("text", x = 6, y = 10499, label = "b", size = 6)+
  annotate("text", x = 7, y = 2500, label = "a", size = 6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
