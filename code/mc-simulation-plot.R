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
library(ggpubr)

n.estimators <- 6

# Load results data

filenames <- c(list.files(path="outputs/20220118", pattern = ".rds", full.names = TRUE),
                list.files(path="outputs/20220116", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20220115", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20220114", pattern = ".rds", full.names = TRUE))

filenames <- filenames[-grep("placebo",filenames)] # exclude placebo results
filenames <- filenames[-grep("R_40",filenames)] # exlude R=40

rmse.vars <- c('est_mc_plain_RMSE','est_mc_weights_RMSE','est_mc_weights_covars_RMSE','est_model_ADH_RMSE','est_model_DID_RMSE','est_model_IFE_RMSE')
abs.bias.vars <- c('est_mc_plain_abs_bias','est_mc_weights_abs_bias','est_mc_weights_covars_abs_bias','est_model_ADH_abs_bias','est_model_DID_abs_bias','est_model_IFE_abs_bias')
rel.abs.bias.vars <- c('est_mc_plain_rel_abs_bias','est_mc_weights_rel_abs_bias','est_mc_weights_covars_rel_abs_bias','est_model_ADH_rel_abs_bias','est_model_DID_rel_abs_bias','est_model_IFE_rel_abs_bias')
cp.vars <- c('est_mc_plain_cp','est_mc_weights_cp','est_mc_weights_covars_cp','est_model_ADH_cp','est_model_DID_cp','est_model_IFE_cp')
boot.var.vars <- c('est_mc_plain_boot_var','est_mc_weights_boot_var','est_mc_weights_covars_boot_var','est_model_ADH_boot_var','est_model_DID_boot_var','est_model_IFE_boot_var')

results <- list() # structure is: [[filename]][[metric]]
for(f in filenames){
  print(f)
  result.matrix <- readRDS(f)
  n <- nrow(result.matrix )
  rmse <- matrix(NA, n, n.estimators)
  abs.bias <- matrix(NA, n, n.estimators)
  rel.abs.bias <- matrix(NA, n, n.estimators)
  CP <- matrix(NA, n, n.estimators)
  boot.var <- matrix(NA, n, n.estimators)
  for(i in rmse.vars){
    rmse[,which(rmse.vars==i)] <- unlist(result.matrix[,i])
  }
  for(i in abs.bias.vars){
    abs.bias[,which(abs.bias.vars==i)] <- unlist(result.matrix[,i])
  }
  for(i in rel.abs.bias.vars){
    rel.abs.bias[,which(rel.abs.bias.vars==i)] <- unlist(result.matrix[,i])
  }
  for(i in cp.vars){
    CP[,which(cp.vars==i)] <- unlist(result.matrix[,i])
  }
  for(i in boot.var.vars){
    boot.var[,which(boot.var.vars==i)] <- unlist(result.matrix[,i])
  }
  fr_obs <- unlist(result.matrix[,"fr_obs"])
  results[[f]] <- list("rmse"=rmse,"abs_bias"=abs.bias,"rel_abs_bias"=rel.abs.bias,"CP"=CP,"boot_var"=boot.var,"fr_obs"=fr_obs,"n"=n)
}

# Create New lists
# structure is: [[estimator]][[filename]]

rmse <- list()
for(i in 1:length(rmse.vars)){
  rmse[[i]] <- lapply(1:length(filenames), function(f) results[[f]]$abs_bias[,i])
}

abs.bias <- list()
for(i in 1:length(abs.bias.vars)){
  abs.bias[[i]] <- lapply(1:length(filenames), function(f) results[[f]]$abs_bias[,i])
}

rel.abs.bias <- list()
for(i in 1:length(rel.abs.bias.vars)){
  rel.abs.bias[[i]] <- lapply(1:length(filenames), function(f) results[[f]]$rel_abs_bias[,i])
}

CP <- list()
for(i in 1:length(cp.vars)){
  CP[[i]] <- lapply(1:length(filenames), function(f) results[[f]]$CP[,i])
}

boot.var <- list()
for(i in 1:length(boot.var.vars)){
  boot.var[[i]] <- lapply(1:length(filenames), function(f) results[[f]]$boot_var[,i])
}

# Create dataframe for plot
results.df <- data.frame("rmse"=as.numeric(unlist(rmse)),
                        "abs_bias"=as.numeric(unlist(abs.bias)),
                         "rel_abs_bias"=as.numeric(unlist(rel.abs.bias)),
                         "Coverage"=as.numeric(unlist(CP)),
                         "boot_var"=as.numeric(unlist(boot.var)),
                         "fr_obs"= rep(sapply(1:length(filenames), function(f) results[[f]]$fr_obs), n.estimators),
                         "Estimator"=c(rep("MC (plain)",length.out=length(c(unlist(CP[[1]])))), 
                                       rep("MC (weights)",length.out=length(c(unlist(CP[[2]])))),
                         rep("MC (weights + covars.)",length.out=length(c(unlist(CP[[3]])))),
                         rep("SCM",length.out=length(c(unlist(CP[[4]])))),
                         rep("DID",length.out=length(c(unlist(CP[[5]])))),
                         rep("IFE",length.out=length(c(unlist(CP[[6]]))))),
                         "filename"=c(rep(unlist(sapply(1:length(filenames), function(i) rep(filenames[i], length.out=n))), n.estimators)))

results.df$NT <- NA
results.df$R <- NA
results.df$N_t <- NA
results.df$T0 <- NA
  
for(s in c("N_40_T_40")){
  if(length(grep(s, results.df$filename))>0){
    print(s)
    if(s=="N_40_T_40"){
      NT <- "1600"
    }
    results.df[grep(s, results.df$filename),]$NT <- NT
  }
}

for(s in c("R_10","R_20","R_30","R_40")){
  if(length(grep(s, results.df$filename))>0){
    print(s)
    if(s=="R_10"){
      R <- "10"
    }
    if(s=="R_20"){
      R <- "20"
    }
    if(s=="R_30"){
      R <- "30"
    }
    if(s=="R_40"){
      R <- "40"
    }
    results.df[grep(s, results.df$filename),]$R <- R
  }
}

for(s in c("T0_20")){
  if(length(grep(s, results.df$filename))>0){
    print(s)
    if(s=="T0_20"){
      T0 <- "0.5"
    }
    results.df[grep(s, results.df$filename),]$T0 <- T0
  }
}

for(s in c("N_t16","N_t24","N_t32")){
  if(length(grep(s, results.df$filename))>0){
    print(s)
    if(s=="N_t16"){
      N_t <- "0.4"
    }
    if(s=="N_t24"){
      N_t <- "0.6"
    }
    if(s=="N_t32"){
      N_t <- "0.8"
    }
    results.df[grep(s, results.df$filename),]$N_t  <- N_t
  }
}

# create coverage rate variable

results.df <- results.df %>% 
  group_by(Estimator,NT,R,T0,N_t) %>% 
  mutate(CP = mean(Coverage)) 

# reshape and plot
results.df$id <- with(results.df, paste(NT,R,T0,N_t, sep = "_"))
results_long <- reshape2::melt(results.df[!colnames(results.df) %in% c("id","filename")], id.vars=c("Estimator","NT","R","T0","N_t","fr_obs"))  # convert to long format

variable_names <- list(
  '0.4'= TeX("$N_{ST} = 16$"),
  '0.6'= TeX("$N_{ST} = 24$"),
  '0.8'= TeX("$N_{ST} = 32$")) 

labeller <- function(variable,value){
  return(variable_names[value])
}

# rmse (NxT)
sim.results.rmse <- ggplot(data=results_long[results_long$variable=="rmse",],
                               aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  facet_grid(. ~  N_t, scales = "free", labeller=labeller)  + ylab("RMSE") +  xlab("Rank") +
  scale_fill_discrete(name = "Estimator:") +
  coord_cartesian(ylim = c(0,4)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks= pretty_breaks())+
  theme(legend.position="none") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
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

ggsave("plots/simulation_rmse.png",plot = sim.results.rmse)

# abs.bias (NxT)
sim.results.abs.bias <- ggplot(data=results_long[results_long$variable=="abs_bias",],
                           aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  facet_grid(. ~  N_t, scales = "free", labeller=labeller)  + ylab("Absolute bias") +  xlab("Rank") +
  scale_fill_discrete(name = "Estimator:") +
  coord_cartesian(ylim = c(0,2.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks= pretty_breaks())+
  theme(legend.position="none") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
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

ggsave("plots/simulation_abs_bias.png",plot = sim.results.abs.bias)

sim.results.abs.bias.slides <- ggplot(data=results_long[results_long$variable=="abs_bias" & results_long$N_t==0.8 & results_long$Estimator!="MC (plain)",],
                               aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  ylab("Absolute bias") +  xlab(" ") +
  scale_fill_discrete(name = "Estimator:") +
  coord_cartesian(ylim = c(0,2.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks= pretty_breaks())+
  theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines")) 
 # ggtitle(TeX("$NT = 40 \\times 40, \\, N_{ST} = 32$"))

# coverage
sim.results.coverage <- ggplot(data=results_long[results_long$variable=="CP",],
                           aes(x=factor(R), y=value, colour=Estimator, group=forcats::fct_rev(Estimator)))  +   geom_line()  +
  facet_grid(.~N_t, scales = "free", labeller=labeller)  +  xlab("Rank") + ylab("Coverage probability (%)") +
  scale_colour_discrete(name = "Estimator:") +
  scale_y_continuous(breaks= pretty_breaks()) +
  geom_hline(yintercept = 0.95, linetype="dotted")+
  theme(legend.position="none") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
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

ggsave("plots/simulation_coverage.png",plot = sim.results.coverage)

sim.results.coverage.slides <- ggplot(data=results_long[results_long$variable=="CP" & results_long$N_t==0.8 & results_long$Estimator!="MC (plain)",],
                               aes(x=factor(R), y=value, colour=Estimator, group=forcats::fct_rev(Estimator)))  +   geom_line()  +
   xlab(" ") + ylab("Coverage probability (%)") +
  scale_colour_discrete(name = "Estimator:") +
  scale_y_continuous(breaks= pretty_breaks()) +
  geom_hline(yintercept = 0.95, linetype="dotted")+
  theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines"))

# boot_var

sim.results.boot.var <- ggplot(data=results_long[results_long$variable=="boot_var",],
                           aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  facet_grid(.~N_t, scales = "free", labeller=labeller)  +  xlab("Rank")  + ylab("Bootstrap variance") +
  scale_fill_discrete(name = "Estimator:") +
  coord_cartesian(ylim = c(0,2.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks= pretty_breaks())+
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

ggsave("plots/simulation_boot_var.png",plot = sim.results.boot.var)

sim.results.boot.var.slides <- ggplot(data=results_long[results_long$variable=="boot_var" & results_long$N_t==0.8 & results_long$Estimator!="MC (plain)",],
                                      aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  ylab("Bootstrap variance") +  xlab("Rank") +
  scale_fill_discrete(name = "Estimator:") +
  coord_cartesian(ylim = c(0,2.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks= pretty_breaks())+
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines"))
 # ggtitle(TeX("$NT = 40 \\times 40, \\, N_{ST} = 32$"))

# plot for slides

ggarrange(sim.results.abs.bias.slides, sim.results.boot.var.slides, sim.results.coverage.slides, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
ggsave("plots/simulation_slides.png",last_plot(),scale=1.12)

# Get color hues

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

gg_color_hue(n.estimators)