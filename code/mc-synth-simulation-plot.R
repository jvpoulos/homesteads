############################################################################################
# Plot matrix completion synthetic control simulation results                             #
############################################################################################

library(tidyverse)
library(ggplot2)
library(data.table)
library(latex2exp)
library(dplyr)
library(grid)
library(gtable)
library(scales)

R <- 1000

# Load results data

filenames <- c(list.files(path="outputs/20230202", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230203", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230204", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230205", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230206", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230207", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230211", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230216", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230218", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230220", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230223", pattern = ".rds", full.names = TRUE))

filenames <- filenames[grep("synth_sim",filenames)] 
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

results.df[grep("T0_1", results.df$filename),]$T0 <- ifelse(results.df$d=="basque","0.12", # NEED TO ADJUST PER Dataset
                                                            ifelse(results.df$d=="california","0.13"),
                                                            ifelse(results.df$d=="germany","0.11",NA))
results.df[grep("T0_2", results.df$filename),]$T0 <- ifelse(results.df$d=="basque","0,30",
                                                            ifelse(results.df$d=="california","0.32"),
                                                            ifelse(results.df$d=="germany","0.32",NA))
results.df[grep("T0_3", results.df$filename),]$T0 <- ifelse(results.df$d=="basque","0.12",
                                                            ifelse(results.df$d=="california","0.13"),
                                                            ifelse(results.df$d=="germany","0.11",NA))
results.df[grep("T0_4", results.df$filename),]$T0 <- ifelse(results.df$d=="basque","0.12",
                                                            ifelse(results.df$d=="california","0.13"),
                                                            ifelse(results.df$d=="germany","0.11",NA))

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

ggsave("plots/mc_synth_simulation_placebo_abs_bias.png",plot = sim.results.abs.bias)

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

ggsave("plots/mc_synth_simulation_placebo_coverage.png",plot = sim.results.coverage)

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

ggsave("plots/mc_synth_simulation_placebo_boot_var.png",plot = sim.results.boot.var)