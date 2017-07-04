library(ggplot2)
library(gridExtra)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


dfsmallaff <- data.frame(
  Correct_Community = c("First", "Second", "Third+"),
  value = c(20.83, 33.33, 45.83))

leg <- ggplot(dfbigcov, aes(x="", y=value, fill=Correct_Community)) +
  geom_bar(width = 2, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void()+
  scale_fill_manual(values=c('SpringGreen3','yellow3','salmon3'),
                    name="",
                    labels=c("First   ", "Second  ", "Third or more")) + 
  theme(legend.position = "top") +
  theme(legend.title=element_text(size=40), legend.text=element_text(size=20))


leg <- get_legend(leg)


sa <- ggplot(dfsmallaff, aes(x="", y=value, fill=Correct_Community)) +
  geom_bar(width = 10, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values=c('SpringGreen3','yellow3','salmon3')) +
  theme_void()+
  theme(legend.position="none") +
  ggtitle("Tree affinity \n 10 - 25 tips") +
  theme(plot.title = element_text(vjust=1, hjust=0.5, size = 24))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

dfsmallcov<- data.frame(
  Correct_Community = c("First", "Second", "Third+"),
  value = c(75, 15.63, 9.38))

sc <- ggplot(dfsmallcov, aes(x="", y=value, fill=Correct_Community)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values=c('SpringGreen3','yellow3','salmon3')) +
  theme_void()+
  theme(legend.position="none") +
  ggtitle("Bipartition covariance\n 10 - 25 tips") +
  theme(plot.title = element_text(vjust=1, hjust=0.5, size = 24))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))


dfbigaff<- data.frame(
  Correct_Community = c("First", "Second", "Third+"),
  value = c(63.89, 36.11, 0))

ba <- ggplot(dfbigaff, aes(x="", y=value, fill=Correct_Community)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values=c('SpringGreen3','yellow3','salmon3')) +
  theme_void() +
  theme(legend.position="none") +
  ggtitle("Tree affinity\n 50 - 100 tips") +
  theme(plot.title = element_text(hjust=0.5, size = 24)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

dfbigcov<- data.frame(
  Correct_Community = c("First", "Second", "Third+"),
  value = c(65.91, 9.09, 25))

bc <- ggplot(dfbigcov, aes(x="", y=value, fill=Correct_Community)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values=c('SpringGreen3','yellow3','salmon3')) +
  theme_void() +
  theme(legend.position="none") +
  ggtitle("Bipartition covariance \n 50 - 100 tips") +
  theme(plot.title = element_text(vjust=1, hjust=0.5, size = 24))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

ggsave("foo.pdf", arrangeGrob(sa, sc, ba, bc, leg), width = 40, height = 40, units = "cm")

pdf("filename.pdf",width = 30, height = 30)
grid.arrange(sa, sc, ba, bc, leg, ncol=2, nrow = 3, 
             layout_matrix = rbind(c(1,2), c(3,4), c(5,5)), 
             widths = c(5, 5), heights = c(5, 5, 0.5))
dev.off()
    
