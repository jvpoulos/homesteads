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
               list.files(path="outputs/20230223", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230306", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230311", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230312", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230317", pattern = ".rds", full.names = TRUE),
               list.files(path="outputs/20230322", pattern = ".rds", full.names = TRUE))

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

results.df[which(results.df$filename%in%grep("T0_1", results.df[results.df$d=="Basque Country terrorism",]$filename,value=TRUE)),]$T0 <- "0.14"
results.df[which(results.df$filename%in%grep("T0_2", results.df[results.df$d=="Basque Country terrorism",]$filename,value=TRUE)),]$T0 <- "0.40"  
results.df[which(results.df$filename%in%grep("T0_3", results.df[results.df$d=="Basque Country terrorism",]$filename,value=TRUE)),]$T0 <- "0.63" 
results.df[which(results.df$filename%in%grep("T0_4", results.df[results.df$d=="Basque Country terrorism",]$filename,value=TRUE)),]$T0 <- "0.88"

results.df[which(results.df$filename%in%grep("T0_1", results.df[results.df$d=="California smoking ban",]$filename,value=TRUE)),]$T0 <- "0.12"
results.df[which(results.df$filename%in%grep("T0_2", results.df[results.df$d=="California smoking ban",]$filename,value=TRUE)),]$T0 <- "0.35"  
results.df[which(results.df$filename%in%grep("T0_3", results.df[results.df$d=="California smoking ban",]$filename,value=TRUE)),]$T0 <- "0.56" 
results.df[which(results.df$filename%in%grep("T0_4", results.df[results.df$d=="California smoking ban",]$filename,value=TRUE)),]$T0 <- "0.79"

results.df[which(results.df$filename%in%grep("T0_1", results.df[results.df$d=="West German reunification",]$filename,value=TRUE)),]$T0 <- "0.14"
results.df[which(results.df$filename%in%grep("T0_2", results.df[results.df$d=="West German reunification",]$filename,value=TRUE)),]$T0 <- "0.40"  
results.df[which(results.df$filename%in%grep("T0_3", results.df[results.df$d=="West German reunification",]$filename,value=TRUE)),]$T0 <- "0.65" 
results.df[which(results.df$filename%in%grep("T0_4", results.df[results.df$d=="West German reunification",]$filename,value=TRUE)),]$T0 <- "0.91"

# create coverage rate variable

results.df <- results.df %>% 
  group_by(Estimator,d,T0,N_t) %>% 
  mutate(CP = mean(Coverage)) 

# summary stats by group
setDT(results.df)[, .(avg = mean(abs_bias)) , by = .(Estimator, d)]
setDT(results.df)[, .(avg = mean(boot_var)) , by = .(Estimator, d)]
setDT(results.df)[, .(avg = mean(Coverage)) , by = .(Estimator, d)]

# reshape and plot
results.df$id <- with(results.df, paste(Estimator,T0,d,N_t, sep = "_"))
results_long <- reshape2::melt(data.frame(results.df)[!colnames(results.df) %in% c("id","filename")], id.vars=c("Estimator","T0","d","N_t"))  # convert to long format

# abs.bias (NxT)
for(d in c("basque","california","germany")){
  d.name <- c("Basque Country terrorism","California smoking ban","West German reunification")[which(d%in%c("basque","california","germany"))]
  sim.results.abs.bias <- ggplot(data=results_long[results_long$variable=="abs_bias" & results_long$d==d.name,],
                                 aes(x=factor(T0), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
    ylab("Absolute bias") +  xlab(TeX('Placebo $a_i^{\\prime}/T$')) +
    # facet_grid(~d) +
    scale_fill_discrete(name = "Estimator:") +
   # scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+ # ,breaks= pretty_breaks()
    coord_cartesian(ylim=c(0,1)) +
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
  
  ggsave(paste0("plots/",d,"_placebo_abs_bias.png"),plot = sim.results.abs.bias, scale=1.2)
}

# coverage

sim.results.coverage <- ggplot(data=results_long[results_long$variable=="CP",],
                               aes(x=factor(T0), y=value, colour=Estimator, group=forcats::fct_rev(Estimator)))  +   geom_line()  +
  facet_grid(~d, scales="free",labeller = labeller(d = label_wrap_gen(width = 15))) +
  xlab(TeX('Placebo $a_i^{\\prime}/T$')) + ylab("Coverage probability (%)") +
  scale_fill_discrete(name = "Estimator:") +
  scale_y_continuous(breaks= pretty_breaks()) +
   coord_cartesian(ylim=c(0,1)) +
  geom_hline(yintercept = 0.95, linetype="dotted")+
  theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=12)) +
  theme(axis.text.x=element_text(family="serif", size=12)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines")) 

ggsave(paste0("plots/mc_synth_simulation_placebo_coverage.png"),plot = sim.results.coverage, scale=1.2)

# boot_var

for(d in c("basque","california","germany")){
  d.name <- c("Basque Country terrorism","California smoking ban","West German reunification")[which(d%in%c("basque","california","germany"))]
  sim.results.boot.var <- ggplot(data=results_long[results_long$variable=="boot_var" & results_long$d==d.name,],
                                 aes(x=factor(T0), y=value, fill=Estimator))  + geom_boxplot(outlier.alpha = 0.3,outlier.size = 1, outlier.stroke = 0.1, lwd=0.25) +
    xlab(TeX('Placebo $a_i^{\\prime}/T$'))  + ylab("Bootstrap variance") +
    scale_fill_discrete(name = "Estimator:") +
   # scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+ #,breaks= pretty_breaks()
  #  coord_cartesian(ylim=c(0,0.6)) +
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
  
  ggsave(paste0("plots/",d,"_placebo_boot_var.png"),plot = sim.results.boot.var, scale=1.2)
}

# Get color hues

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

n.estimators <- 5
gg_color_hue(n.estimators)