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
               list.files(path="outputs/20230228", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230308", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230406", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230409", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230417", pattern = ".rds", full.names = TRUE))

filenames <- filenames[grep("results_N",filenames)] 
filenames <- filenames[-which(duplicated(substr(filenames, 18, nchar(filenames))))]

results <- list() # structure is: [[filename]][[metric]]
for(f in filenames){
  print(f)
  result.matrix <- readRDS(f)
  abs.bias <- sapply(1: R, function(i) result.matrix[,i][["abs_bias"]])
  CP <- sapply(1: R, function(i) result.matrix[,i][["cp"]])
  boot.var <- sapply(1: R, function(i) result.matrix[,i][["boot_var"]])
  rank.error <- sapply(1: R, function(i) result.matrix[,i][["rank_error"]])
  results[[f]] <- list("abs_bias"=abs.bias,"CP"=CP,"boot_var"=boot.var, "rank_error"=rank.error)
}

# Create New lists
# structure is: [[estimator]][[filename]]

abs.bias <- lapply(1:length(filenames), function(f) results[[f]]$abs_bias)

CP <- lapply(1:length(filenames), function(f) results[[f]]$CP)

boot.var <- lapply(1:length(filenames), function(f) results[[f]]$boot_var)

rank.error <- lapply(1:length(filenames), function(f) results[[f]]$rank_error)

# Create dataframe for plot
results.df <- data.frame("abs_bias"=as.numeric(unlist(abs.bias)),
                         "Coverage"=as.numeric(unlist(CP)),
                         "boot_var"=as.numeric(unlist(boot.var)),
                         "filename"=c(sapply(1:length(filenames), function(i) rep(filenames[i], length.out=R))))

results.df$Estimator <- NA
results.df[grep("mc_plain",results.df$filename),]$Estimator <- "MC"
results.df[grep("mc_weights",results.df$filename),]$Estimator <- "MC-W"
results.df[grep("DID",results.df$filename),]$Estimator <- "DID"
results.df[grep("ADH",results.df$filename),]$Estimator <- "SCM"
results.df[grep("ENT",results.df$filename),]$Estimator <- "SCM-L1"

results.df$rank_error <- NA
results.df$rank_error[results.df$Estimator %in% c("MC","MC-W")] <- as.numeric(unlist(rank.error))

results.df$R <- NA
results.df[grep("R_10",results.df$filename),]$R <- "10"
results.df[grep("R_20",results.df$filename),]$R <- "20"
results.df[grep("R_40",results.df$filename),]$R <- "40"

results.df$N_t <- "0.5"

# create coverage rate variable

results.df <- results.df %>% 
  group_by(Estimator,R,N_t) %>% 
  mutate(CP = mean(Coverage)) 

# summary stats by group
setDT(results.df)[, .(avg = mean(abs_bias)) , by = .(Estimator)]
setDT(results.df)[, .(avg = mean(boot_var)) , by = .(Estimator)]
setDT(results.df)[, .(avg = mean(Coverage)) , by = .(Estimator)]

# reshape and plot
results.df$id <- with(results.df, paste(Estimator,R,N_t, sep = "_"))
results_long <- reshape2::melt(results.df[,c("abs_bias","Coverage","boot_var","rank_error", "Estimator", "R", "N_t", "CP")], id.vars=c("Estimator","R","N_t"))  # convert to long format

# abs.bias (NxT)
sim.results.abs.bias <- ggplot(data=results_long[results_long$variable=="abs_bias",],
                               aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  ylab("Absolute bias") +  xlab("Rank") +
  scale_fill_discrete(name = "Estimator:") +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+ # ,breaks= pretty_breaks()
   coord_cartesian(ylim=c(0,1.5)) +
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
                               aes(x=factor(R), y=value, colour=Estimator, group=forcats::fct_rev(Estimator)))  +   geom_line()  +
  xlab("Rank") + ylab("Coverage probability (%)") +
  scale_fill_discrete(name = "Estimator:") +
  scale_y_continuous(breaks= pretty_breaks()) +
  coord_cartesian(ylim=c(0,1)) +
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
                               aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  xlab("Rank")  + ylab("Bootstrap variance") +
  scale_fill_discrete(name = "Estimator:") +
#  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+ #,breaks= pretty_breaks()
  coord_cartesian(ylim=c(0,0.3)) +
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

sim.results.rank.error <- ggplot(data=results_long[results_long$variable=="rank_error" ,],
                               aes(x=factor(R), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
  xlab("Rank")  + ylab("Absolute difference between actual and estimated rank") +
  scale_fill_discrete(name = "Estimator:") +
#  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+ #,breaks= pretty_breaks()
 # coord_cartesian(ylim=c(0,0.45)) +
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

ggsave("plots/mc_simulation_placebo_rank_error.png",plot = sim.results.rank.error)