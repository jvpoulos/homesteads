library(ggplot2)
library(latex2exp)
library(gridExtra)
library(grid)
library(ggpubr)
library(plyr)

# state gov't spending datasets

load(paste0(results.directory, "plots/exp_pc_N_19_T_203_numruns_2000_num_treated_10_simultaneuous_0.rds"))
exp1 <- df1
exp2 <- df2
rm(df1,df2)

load(paste0(results.directory, "plots/rev_pc_N_19_T_203_numruns_2000_num_treated_10_simultaneuous_0.rds"))
rev1 <- df1
rev2 <- df2
rm(df1,df2)

ddply(exp1, .(Method), summarize, avg = mean(y)) # print summaries by method
ddply(rev1, .(Method), summarize, avg = mean(y)) 

# reshape for boxplots
n.estimators <- length(unique(exp1$Method))
R <- 2000
n.comparisons <- length(unique(exp1$x))

exp.results <- data.frame("x"=factor(rep(rep(unique(exp1$x), each=R), times=n.estimators)), 
                          "y"= unlist(exp2[,c(5:29)]),
                          "Method"=rep(rep(unique(exp1$Method), each=R), each=n.comparisons))

rev.results <- data.frame("x"=factor(rep(rep(unique(rev1$x), each=R), times=n.estimators)), 
                          "y"= unlist(rev2[,c(5:29)]),
                          "Method"=rep(rep(unique(rev1$Method), each=R), each=n.comparisons))

x.label <- round(unique(exp1$x),2)

exp <- ggplot(data=exp.results,
       aes(x, y, fill=Method))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  xlab(" ") + ylab(" ") + 
  ggtitle("Expenditure") +
  scale_fill_discrete(name = "Method:") +
  scale_x_discrete(labels=x.label) +
  theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=22)) +
  theme(axis.title=element_text(family="serif", size=20)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14, angle = 45, vjust = 0, hjust=0.5)) +
  theme(legend.text=element_text(family="serif", size=14)) +
  theme(legend.title=element_text(family="serif", size=14)) +
  theme(strip.text.x = element_text(family="serif", size=14)) +
  theme(strip.text.y = element_text(family="serif", size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines")) 

rev <- ggplot(data=rev.results,
              aes(x, y, fill=Method))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  xlab(" ") + ylab(" ") + 
  ggtitle("Revenue") +
  scale_fill_discrete(name = "Method:") +
  scale_x_discrete(labels=x.label) +
  coord_cartesian(ylim=c(0.07,1.5)) +
  theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=22)) +
  theme(axis.title=element_text(family="serif", size=20)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14, angle = 45, vjust = 0, hjust=0.5)) +
  theme(legend.text=element_text(family="serif", size=14)) +
  theme(legend.title=element_text(family="serif", size=14)) +
  theme(strip.text.x = element_text(family="serif", size=14)) +
  theme(strip.text.y = element_text(family="serif", size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines")) 

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(rev)

ggsave(paste0(results.directory, "plots/capacity-stag.png"), grid.arrange(arrangeGrob(exp+ theme(legend.position="none"), 
                                                                                      rev+ theme(legend.position="none"),
                                                                                      bottom=textGrob(TeX('$a_{i}^{\\prime}/T$'), gp=gpar(fontfamily="serif", fontsize=16), vjust=-1.5, hjust=-0.35),
                                                                                      left=textGrob("RMSE", gp=gpar(fontfamily="serif", fontsize=16), vjust= 1.5 ,rot=90), nrow=1), 
                                                                          mylegend, nrow=2,heights=c(10, 1)), scale=1.25)


# synthetic control datasets

load(paste0(results.directory, "plots/basque_N_16_T_43_numruns_2000_num_treated_8_simultaneuous_0.rds"))
basque1 <- df1
basque2 <- df2
rm(df1,df2)

load(paste0(results.directory, "plots/germany_N_16_T_44_numruns_2000_num_treated_8_simultaneuous_0.rds"))
germany1 <- df1
germany2 <- df2
rm(df1,df2)

load(paste0(results.directory, "plots/california_N_38_T_31_numruns_2000_num_treated_19_simultaneuous_0.rds"))
california1 <- df1
california2 <- df2
rm(df1,df2)

print(ddply(basque1, .(Method), summarize, avg = mean(y))) # print summaries by method
print(ddply(germany1, .(Method), summarize, avg = mean(y))) 
print(ddply(california1, .(Method), summarize, avg = mean(y))) 

basque.results <- data.frame("x"=factor(rep(rep(unique(basque1$x), each=R), times=n.estimators)), 
                          "y"= unlist(basque2[,c(5:29)]),
                          "Method"=rep(rep(unique(basque1$Method), each=R), each=n.comparisons))

germany.results <- data.frame("x"=factor(rep(rep(unique(germany1$x), each=R), times=n.estimators)), 
                          "y"= unlist(germany2[,c(5:29)]),
                          "Method"=rep(rep(unique(germany1$Method), each=R), each=n.comparisons))

california.results <- data.frame("x"=factor(rep(rep(unique(california1$x), each=R), times=n.estimators)), 
                          "y"= unlist(california2[,c(5:29)]),
                          "Method"=rep(rep(unique(california1$Method), each=R), each=n.comparisons))

x.label.basque <- round(unique(basque1$x),2)
x.label.germany <- round(unique(germany1$x),2)
x.label.california <- round(unique(california1$x),2)

basque <- ggplot(data=basque.results,
              aes(x, y, fill=Method))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  xlab(" ") + ylab(" ") + 
  ggtitle("Basque Country terrorism") +
  scale_fill_discrete(name = "Method:") +
  scale_x_discrete(labels=x.label.basque) +
  coord_cartesian(ylim=c(0.08,1.7)) +
  theme(legend.position="right") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=22)) +
  theme(axis.title=element_text(family="serif", size=20)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14, angle = 45, vjust = 0, hjust=0.5)) +
  theme(legend.text=element_text(family="serif", size=14)) +
  theme(legend.title=element_text(family="serif", size=14)) +
  theme(strip.text.x = element_text(family="serif", size=14)) +
  theme(strip.text.y = element_text(family="serif", size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines")) 

germany <- ggplot(data=germany.results,
                 aes(x, y, fill=Method))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  xlab(" ") + ylab(" ") + 
  ggtitle("West German reunification") +
  scale_fill_discrete(name = "Method:") +
  scale_x_discrete(labels=x.label.germany) +
  coord_cartesian(ylim=c(300,6500)) +
  theme(legend.position="right") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=22)) +
  theme(axis.title=element_text(family="serif", size=20)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14, angle = 45, vjust = 0, hjust=0.5)) +
  theme(legend.text=element_text(family="serif", size=14)) +
  theme(legend.title=element_text(family="serif", size=14)) +
  theme(strip.text.x = element_text(family="serif", size=14)) +
  theme(strip.text.y = element_text(family="serif", size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines")) 

california <- ggplot(data=california.results,
                 aes(x, y, fill=Method))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  xlab(" ") + ylab(" ") + 
  ggtitle("California smoking ban") +
  scale_fill_discrete(name = "Method:") +
  scale_x_discrete(labels=x.label.california) +
  coord_cartesian(ylim=c(1.6,35)) +
  theme(legend.position="right") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=22)) +
  theme(axis.title=element_text(family="serif", size=20)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14, angle = 45, vjust = 0, hjust=0.5)) +
  theme(legend.text=element_text(family="serif", size=14)) +
  theme(legend.title=element_text(family="serif", size=14)) +
  theme(strip.text.x = element_text(family="serif", size=14)) +
  theme(strip.text.y = element_text(family="serif", size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines")) 

mylegend2 <- g_legend(basque)

ggsave(paste0(results.directory, "plots/synth-stag.png"), grid.arrange(arrangeGrob(basque+ theme(legend.position="none"), 
                                                                                   california + theme(legend.position="none"),
                                                                                      germany+ theme(legend.position="none"),
                                                                                   as_ggplot(mylegend2),
                                                                                   bottom=textGrob(TeX('$a_{i}^{\\prime}/T$'), gp=gpar(fontfamily="serif", fontsize=16), hjust=-0.35),
                                                                                   left=textGrob("RMSE", gp=gpar(fontfamily="serif", fontsize=16), vjust= 1.5 ,rot=90), ncol=2)), scale=1.5)
