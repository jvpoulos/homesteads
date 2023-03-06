############################################################################################
# Plot matrix completion simulation results                                                #
############################################################################################
library(tidyverse)
library(ggplot2)
library(data.table)
library(latex2exp)
library(dplyr)
library(grid)
library(gtable)
library(scales)

R <- 100

# Load results data

filenames <- c(list.files(path="outputs/20230220", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230221", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230224", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230228", pattern = ".rds", full.names = TRUE))

filenames <- filenames[grep("results_N",filenames)] 
filenames <- filenames[-which(duplicated(substr(filenames, 18, nchar(filenames))))]

results <- list() # structure is: [[filename]][[metric]]
for(f in filenames){
  print(f)
  result.matrix <- readRDS(f)
  abs.bias <- sapply(1: R, function(i) result.matrix[,i][["abs_bias"]])
  CP <- sapply(1: R, function(i) result.matrix[,i][["cp"]])
  boot.var <- sapply(1: R, function(i) result.matrix[,i][["boot_var"]])
  results[[f]] <- list("abs_bias"=abs.bias,"CP"=CP,"boot_var"=boot.var)
}

###CONTINUE HERE

# Create New lists
# structure is: [[estimator]][[filename]]

abs.bias <- lapply(1:length(filenames), function(f) results[[f]]$abs_bias)


CP <- lapply(1:length(filenames), function(f) results[[f]]$CP)

boot.var <- lapply(1:length(filenames), function(f) results[[f]]$boot_var)


# Create dataframe for plot
results.df <- data.frame("abs_bias"=as.numeric(unlist(abs.bias)),
                         "Coverage"=as.numeric(unlist(CP)),
                         "boot_var"=as.numeric(unlist(boot.var)),
                         # "Estimator"=c(rep("MC (weights + covars.)",length.out=length(c(unlist(CP[[1]])))),
                         #               rep("SCM",length.out=length(c(unlist(CP[[2]])))),
                         #               rep("DID",length.out=length(c(unlist(CP[[3]])))),
                         #               rep("IFE",length.out=length(c(unlist(CP[[4]]))))),
                         "filename"=c(sapply(1:length(filenames), function(i) rep(filenames[i], length.out=R))))

results.df$Estimator <- NA
results.df[grep("mc_plain",results.df$filename),]$Estimator <- "MC"
results.df[grep("mc_weights",results.df$filename),]$Estimator <- "MC-W"
results.df[grep("DID",results.df$filename),]$Estimator <- "DID"
results.df[grep("ADH",results.df$filename),]$Estimator <- "SCM"
results.df[grep("ENT",results.df$filename),]$Estimator <- "SCM-L1"

results.df$d <- NA
results.df[grep("basque",results.df$filename),]$d <- "Basque Country terrorism"
results.df[grep("germany",results.df$filename),]$d <- "West German reunification"
results.df[grep("california",results.df$filename),]$d <- "California smoking ban"

results.df$N_t <- "0.5"
results.df$T0 <- NA

# create coverage rate variable

results.df <- results.df %>% 
  group_by(Estimator,d,T0,N_t) %>% 
  mutate(CP = mean(Coverage)) 

# reshape and plot
results.df$id <- with(results.df, paste(Estimator,T0,d,N_t, sep = "_"))
results_long <- reshape2::melt(results.df[!colnames(results.df) %in% c("id","filename")], id.vars=c("Estimator","T0","d","N_t"))  # convert to long format

# abs.bias (NxT)
sim.results.abs.bias <- ggplot(data=results_long[results_long$variable=="abs_bias",],
                               aes(x=factor(T0), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  ylab("Absolute bias") +  xlab(TeX('Placebo $a_i^{\\prime}/T$')) +
  facet_grid(~d) +
  scale_fill_discrete(name = "Estimator:") +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+ # ,breaks= pretty_breaks()
  # coord_cartesian(ylim=c(0,0.03)) +
  theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=22)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines")) 

ggsave("plots/mc_simulation_placebo_abs_bias.png",plot = sim.results.abs.bias)

# coverage
sim.results.coverage <- ggplot(data=results_long[results_long$variable=="CP",],
                               aes(x=factor(T0), y=value, colour=Estimator, group=forcats::fct_rev(Estimator)))  +   geom_line()  +
  xlab(TeX('Placebo $a_i^{\\prime}/T$')) + ylab("Coverage probability (%)") +
  scale_fill_discrete(name = "Estimator:") +
  scale_y_continuous(breaks= pretty_breaks()) +
  # coord_cartesian(ylim=c(0.95,1)) +
  geom_hline(yintercept = 0.95, linetype="dotted")+
  theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines"))

ggsave("plots/mc_simulation_placebo_coverage.png",plot = sim.results.coverage)

# boot_var

sim.results.boot.var <- ggplot(data=results_long[results_long$variable=="boot_var",],
                               aes(x=factor(T0), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  xlab(TeX('Placebo $a_i^{\\prime}/T$'))  + ylab("Bootstrap variance") +
  scale_fill_discrete(name = "Estimator:") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+ #,breaks= pretty_breaks()
  coord_cartesian(ylim=c(0,0.6)) +
  theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines"))

ggsave("plots/mc_simulation_placebo_boot_var.png",plot = sim.results.boot.var)


# # Create dataframe for plot
# results.df <- data.frame("rmse"=as.numeric(unlist(rmse)),
#                         "abs_bias"=as.numeric(unlist(abs.bias)),
#                          "rel_abs_bias"=as.numeric(unlist(rel.abs.bias)),
#                          "Coverage"=as.numeric(unlist(CP)),
#                          "boot_var"=as.numeric(unlist(boot.var)),
#                          "fr_obs"= rep(sapply(1:length(filenames), function(f) results[[f]]$fr_obs), n.estimators),
#                          "Estimator"=c(rep("MC (plain)",length.out=length(c(unlist(CP[[1]])))), 
#                                        rep("MC (weights)",length.out=length(c(unlist(CP[[2]])))),
#                          rep("MC (weights + covars.)",length.out=length(c(unlist(CP[[3]])))),
#                          rep("SCM",length.out=length(c(unlist(CP[[4]])))),
#                          rep("DID",length.out=length(c(unlist(CP[[5]])))),
#                          rep("IFE",length.out=length(c(unlist(CP[[6]]))))),
#                          "filename"=c(rep(unlist(sapply(1:length(filenames), function(i) rep(filenames[i], length.out=n))), n.estimators)))
# 
# results.df$NT <- NA
# results.df$R <- NA
# results.df$N_t <- NA
# results.df$T0 <- NA
#   
# for(s in c("N_40_T_40")){
#   if(length(grep(s, results.df$filename))>0){
#     print(s)
#     if(s=="N_40_T_40"){
#       NT <- "1600"
#     }
#     results.df[grep(s, results.df$filename),]$NT <- NT
#   }
# }
# 
# for(s in c("R_10","R_20","R_30","R_40")){
#   if(length(grep(s, results.df$filename))>0){
#     print(s)
#     if(s=="R_10"){
#       R <- "10"
#     }
#     if(s=="R_20"){
#       R <- "20"
#     }
#     if(s=="R_30"){
#       R <- "30"
#     }
#     if(s=="R_40"){
#       R <- "40"
#     }
#     results.df[grep(s, results.df$filename),]$R <- R
#   }
# }
# 
# for(s in c("T0_20")){
#   if(length(grep(s, results.df$filename))>0){
#     print(s)
#     if(s=="T0_20"){
#       T0 <- "0.5"
#     }
#     results.df[grep(s, results.df$filename),]$T0 <- T0
#   }
# }
# 
# for(s in c("N_t16","N_t24","N_t32")){
#   if(length(grep(s, results.df$filename))>0){
#     print(s)
#     if(s=="N_t16"){
#       N_t <- "0.4"
#     }
#     if(s=="N_t24"){
#       N_t <- "0.6"
#     }
#     if(s=="N_t32"){
#       N_t <- "0.8"
#     }
#     results.df[grep(s, results.df$filename),]$N_t  <- N_t
#   }
# }

# create coverage rate variable

# results.df <- results.df %>% 
#   group_by(Estimator,NT,R,T0,N_t) %>% 
#   mutate(CP = mean(Coverage)) 
# 
# # reshape and plot
# results.df$id <- with(results.df, paste(NT,R,T0,N_t, sep = "_"))
# results_long <- reshape2::melt(results.df[!colnames(results.df) %in% c("id","filename")], id.vars=c("Estimator","NT","R","T0","N_t","fr_obs"))  # convert to long format
# 
# variable_names <- list(
#   '0.4'= TeX("$N_{ST} = 16$"),
#   '0.6'= TeX("$N_{ST} = 24$"),
#   '0.8'= TeX("$N_{ST} = 32$")) 
# 
# labeller <- function(variable,value){
#   return(variable_names[value])
# }
# 
# # rmse (NxT)
# sim.results.rmse <- ggplot(data=results_long[results_long$variable=="rmse",],
#                            aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
#   facet_grid(. ~  N_t, scales = "free", labeller=labeller)  + ylab("RMSE") +  xlab("Rank") +
#   scale_fill_discrete(name = "Estimator:") +
#   coord_cartesian(ylim = c(0,4)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks= pretty_breaks())+
#   theme(legend.position="none") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
#   theme(axis.title=element_text(family="serif", size=16)) +
#   theme(axis.text.y=element_text(family="serif", size=14)) +
#   theme(axis.text.x=element_text(family="serif", size=14)) +
#   theme(legend.text=element_text(family="serif", size = 14)) +
#   theme(legend.title=element_text(family="serif", size = 14)) +
#   theme(strip.text.x = element_text(family="serif", size = 14)) +
#   theme(strip.text.y = element_text(family="serif", size = 14)) +
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
#   theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
#   theme(panel.spacing = unit(1, "lines")) 
# 
# ggsave("plots/simulation_rmse.png",plot = sim.results.rmse)
# 
# # abs.bias (NxT)
# sim.results.abs.bias <- ggplot(data=results_long[results_long$variable=="abs_bias",],
#                                aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
#   facet_grid(. ~  N_t, scales = "free", labeller=labeller)  + ylab("Absolute bias") +  xlab("Rank") +
#   scale_fill_discrete(name = "Estimator:") +
#   coord_cartesian(ylim = c(0,2.5)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks= pretty_breaks())+
#   theme(legend.position="none") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
#   theme(axis.title=element_text(family="serif", size=16)) +
#   theme(axis.text.y=element_text(family="serif", size=14)) +
#   theme(axis.text.x=element_text(family="serif", size=14)) +
#   theme(legend.text=element_text(family="serif", size = 14)) +
#   theme(legend.title=element_text(family="serif", size = 14)) +
#   theme(strip.text.x = element_text(family="serif", size = 14)) +
#   theme(strip.text.y = element_text(family="serif", size = 14)) +
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
#   theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
#   theme(panel.spacing = unit(1, "lines")) 
# 
# ggsave("plots/simulation_abs_bias.png",plot = sim.results.abs.bias)
# 
# sim.results.abs.bias.slides <- ggplot(data=results_long[results_long$variable=="abs_bias" & results_long$N_t==0.8 & results_long$Estimator!="MC (plain)",],
#                                       aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
#   ylab("Absolute bias") +  xlab(" ") +
#   scale_fill_discrete(name = "Estimator:") +
#   coord_cartesian(ylim = c(0,2.5)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks= pretty_breaks())+
#   theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
#   theme(axis.title=element_text(family="serif", size=16)) +
#   theme(axis.text.y=element_text(family="serif", size=14)) +
#   theme(axis.text.x=element_text(family="serif", size=14)) +
#   theme(legend.text=element_text(family="serif", size = 14)) +
#   theme(legend.title=element_text(family="serif", size = 14)) +
#   theme(strip.text.x = element_text(family="serif", size = 14)) +
#   theme(strip.text.y = element_text(family="serif", size = 14)) +
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l =0))) +
#   theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l =0))) +
#   theme(panel.spacing = unit(1, "lines")) 
# # ggtitle(TeX("$NT = 40 \\times 40, \\, N_{ST} = 32$"))
# 
# # coverage
# sim.results.coverage <- ggplot(data=results_long[results_long$variable=="CP",],
#                                aes(x=factor(R), y=value, colour=Estimator, group=forcats::fct_rev(Estimator)))  +   geom_line()  +
#   facet_grid(.~N_t, scales = "free", labeller=labeller)  +  xlab("Rank") + ylab("Coverage probability (%)") +
#   scale_colour_discrete(name = "Estimator:") +
#   scale_y_continuous(breaks= pretty_breaks()) +
#   geom_hline(yintercept = 0.95, linetype="dotted")+
#   theme(legend.position="none") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
#   theme(axis.title=element_text(family="serif", size=16)) +
#   theme(axis.text.y=element_text(family="serif", size=14)) +
#   theme(axis.text.x=element_text(family="serif", size=14)) +
#   theme(legend.text=element_text(family="serif", size = 14)) +
#   theme(legend.title=element_text(family="serif", size = 14)) +
#   theme(strip.text.x = element_text(family="serif", size = 14)) +
#   theme(strip.text.y = element_text(family="serif", size = 14)) +
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
#   theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
#   theme(panel.spacing = unit(1, "lines"))
# 
# ggsave("plots/simulation_coverage.png",plot = sim.results.coverage)
# 
# sim.results.coverage.slides <- ggplot(data=results_long[results_long$variable=="CP" & results_long$N_t==0.8 & results_long$Estimator!="MC (plain)",],
#                                       aes(x=factor(R), y=value, colour=Estimator, group=forcats::fct_rev(Estimator)))  +   geom_line()  +
#   xlab(" ") + ylab("Coverage probability (%)") +
#   scale_colour_discrete(name = "Estimator:") +
#   scale_y_continuous(breaks= pretty_breaks()) +
#   geom_hline(yintercept = 0.95, linetype="dotted")+
#   theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
#   theme(axis.title=element_text(family="serif", size=16)) +
#   theme(axis.text.y=element_text(family="serif", size=14)) +
#   theme(axis.text.x=element_text(family="serif", size=14)) +
#   theme(legend.text=element_text(family="serif", size = 14)) +
#   theme(legend.title=element_text(family="serif", size = 14)) +
#   theme(strip.text.x = element_text(family="serif", size = 14)) +
#   theme(strip.text.y = element_text(family="serif", size = 14)) +
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l =0))) +
#   theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l =0))) +
#   theme(panel.spacing = unit(1, "lines"))
# 
# # boot_var
# 
# sim.results.boot.var <- ggplot(data=results_long[results_long$variable=="boot_var",],
#                                aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
#   facet_grid(.~N_t, scales = "free", labeller=labeller)  +  xlab("Rank")  + ylab("Bootstrap variance") +
#   scale_fill_discrete(name = "Estimator:") +
#   coord_cartesian(ylim = c(0,2.5)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks= pretty_breaks())+
#   theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
#   theme(axis.title=element_text(family="serif", size=16)) +
#   theme(axis.text.y=element_text(family="serif", size=14)) +
#   theme(axis.text.x=element_text(family="serif", size=14)) +
#   theme(legend.text=element_text(family="serif", size = 14)) +
#   theme(legend.title=element_text(family="serif", size = 14)) +
#   theme(strip.text.x = element_text(family="serif", size = 14)) +
#   theme(strip.text.y = element_text(family="serif", size = 14)) +
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
#   theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
#   theme(panel.spacing = unit(1, "lines"))
# 
# ggsave("plots/simulation_boot_var.png",plot = sim.results.boot.var)
# 
# sim.results.boot.var.slides <- ggplot(data=results_long[results_long$variable=="boot_var" & results_long$N_t==0.8 & results_long$Estimator!="MC (plain)",],
#                                       aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
#   ylab("Bootstrap variance") +  xlab("Rank") +
#   scale_fill_discrete(name = "Estimator:") +
#   coord_cartesian(ylim = c(0,2.5)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks= pretty_breaks())+
#   theme(legend.position="bottom") +
#   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
#   theme(axis.title=element_text(family="serif", size=16)) +
#   theme(axis.text.y=element_text(family="serif", size=14)) +
#   theme(axis.text.x=element_text(family="serif", size=14)) +
#   theme(legend.text=element_text(family="serif", size = 14)) +
#   theme(legend.title=element_text(family="serif", size = 14)) +
#   theme(strip.text.x = element_text(family="serif", size = 14)) +
#   theme(strip.text.y = element_text(family="serif", size = 14)) +
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l =0))) +
#   theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l =0))) +
#   theme(panel.spacing = unit(1, "lines"))
# # ggtitle(TeX("$NT = 40 \\times 40, \\, N_{ST} = 32$"))
# 
# # plot for slides
# 
# ggarrange(sim.results.abs.bias.slides, sim.results.boot.var.slides, sim.results.coverage.slides, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
# ggsave("plots/simulation_slides.png",last_plot(),scale=1.12)