library(ggplot2)
library(latex2exp)
library(gridExtra)
library(grid)
library(ggpubr)

# state gov't spending datasets

load(paste0(results.directory, "plots/exp_pc_N_19_T_203_numruns_1000_num_treated_10_simultaneuous_0.rds"))
exp1 <- df1
exp2 <- df2
rm(df1,df2)

load(paste0(results.directory, "plots/rev_pc_N_19_T_203_numruns_1000_num_treated_10_simultaneuous_0.rds"))
rev1 <- df1
rev2 <- df2
rm(df1,df2)

exp <- ggplot(data = exp1, aes(x, y, color = Method, shape = Method)) +
  geom_point(size = 4, position=position_dodge(width=0.2)) +
  geom_line(position=position_dodge(width=0.2), linetype = "dashed") +
  geom_errorbar(
    aes(ymin = lb, ymax = ub),
    width = 0.2,
    linetype = "solid",
    position=position_dodge(width=0.2)) +
  scale_shape_manual("Method:",values=c(1:4)) +
  scale_color_discrete("Method:")+
  scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1), labels=c("0","0.25","0.5","0.75","1")) +
  theme_bw() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Expenditure") +
  theme(legend.position="bottom")+
  theme(plot.title = element_text(hjust = 0.5, family="serif", size=14)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background

rev <- ggplot(data = rev1, aes(x, y, color = Method, shape = Method)) +
  geom_point(size = 4, position=position_dodge(width=0.2)) +
  geom_line(position=position_dodge(width=0.2), linetype = "dashed") +
  geom_errorbar(
    aes(ymin = lb, ymax = ub),
    width = 0.2,
    linetype = "solid",
    position=position_dodge(width=0.2)) +
  scale_shape_manual("Method:",values=c(1:4)) +
  scale_color_discrete("Method:")+
  scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1), labels=c("0","0.25","0.5","0.75","1")) +
  theme_bw() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Revenue") +
  theme(legend.position="bottom")+
  theme(plot.title = element_text(hjust = 0.5, family="serif", size=14)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(rev)

ggsave(paste0(results.directory, "plots/capacity-stag.png"), grid.arrange(arrangeGrob(exp+ theme(legend.position="none"), 
                                                                                      rev+ theme(legend.position="none"),
                                                                                      bottom=textGrob(TeX('$a_{i}^{\\prime}/T$'), gp=gpar(fontfamily="serif", fontsize=16), vjust=-1.5, hjust=-0.35),
                                                                                      left=textGrob("Average RMSE", gp=gpar(fontfamily="serif", fontsize=16), vjust= 1.5 ,rot=90), nrow=1), 
                                                                          mylegend, nrow=2,heights=c(10, 1)), scale=1.25)


# synthetic control datasets

load(paste0(results.directory, "plots/basque_N_16_T_43_numruns_1000_num_treated_8_simultaneuous_0.rds"))
basque1 <- df1
basque2 <- df2
rm(df1,df2)

load(paste0(results.directory, "plots/germany_N_16_T_44_numruns_1000_num_treated_8_simultaneuous_0.rds"))
germany1 <- df1
germany2 <- df2
rm(df1,df2)

load(paste0(results.directory, "plots/california_N_38_T_31_numruns_1000_num_treated_19_simultaneuous_0.rds"))
california1 <- df1
california2 <- df2
rm(df1,df2)

basque <-ggplot(data = basque1, aes(x, y, color = Method, shape = Method)) +
  geom_point(size = 4, position=position_dodge(width=0.2)) +
  geom_line(position=position_dodge(width=0.2), linetype = "dashed") +
  geom_errorbar(
    aes(ymin = lb, ymax = ub),
    width = 0.2,
    linetype = "solid",
    position=position_dodge(width=0.2)) +
  scale_shape_manual("Method:",values=c(1:4)) +
  scale_color_discrete("Method:")+
  scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1), labels=c("0","0.25","0.5","0.75","1")) +
  theme_bw() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Basque Country terrorism") +
  theme(legend.position="right")+
  theme(plot.title = element_text(hjust = 0.5, family="serif", size=14)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background

germany <- ggplot(data = germany1, aes(x, y, color = Method, shape = Method)) +
  geom_point(size = 4, position=position_dodge(width=0.2)) +
  geom_line(position=position_dodge(width=0.2), linetype = "dashed") +
  geom_errorbar(
    aes(ymin = lb, ymax = ub),
    width = 0.2,
    linetype = "solid",
    position=position_dodge(width=0.2)) +
  scale_shape_manual("Method:",values=c(1:4)) +
  scale_color_discrete("Method:")+
  scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1), labels=c("0","0.25","0.5","0.75","1")) +
  theme_bw() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("West German reunification") +
  theme(legend.position="right")+
  theme(plot.title = element_text(hjust = 0.5, family="serif", size=14)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background

california <- ggplot(data = california1, aes(x, y, color = Method, shape = Method)) +
  geom_point(size = 4, position=position_dodge(width=0.2)) +
  geom_line(position=position_dodge(width=0.2), linetype = "dashed") +
  geom_errorbar(
    aes(ymin = lb, ymax = ub),
    width = 0.2,
    linetype = "solid",
    position=position_dodge(width=0.2)) +
  scale_shape_manual("Method:",values=c(1:4)) +
  scale_color_discrete("Method:")+
  scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1), labels=c("0","0.25","0.5","0.75","1")) +
  theme_bw() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("California smoking ban") +
  theme(legend.position="right")+
  theme(plot.title = element_text(hjust = 0.5, family="serif", size=14)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background

mylegend2 <- g_legend(basque)

ggsave(paste0(results.directory, "plots/synth-stag.png"), grid.arrange(arrangeGrob(basque+ theme(legend.position="none"), 
                                                                                   california + theme(legend.position="none"),
                                                                                      germany+ theme(legend.position="none"),
                                                                                   as_ggplot(mylegend2),
                                                                                   bottom=textGrob(TeX('$a_{i}^{\\prime}/T$'), gp=gpar(fontfamily="serif", fontsize=16), hjust=-0.35),
                                                                                   left=textGrob("Average RMSE", gp=gpar(fontfamily="serif", fontsize=16), vjust= 1.5 ,rot=90), ncol=2)), scale=1.5)