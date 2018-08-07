#####################################
### lstm ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)
library(ftsa)

# setup

west.revpc.n.pre <- nrow(rev.pc.y.west[rev.pc.y.west$year<1862,])
west.revpc.n.placebo <- ncol(rev.pc.x.west.imp[!colnames(rev.pc.x.west.imp) %in% c("year")])

west.revpc.x <- rev.pc.x.west.imp[!colnames(rev.pc.x.west.imp) %in% c("year")]
west.revpc.y <- rev.pc.y.west[!colnames(rev.pc.y.west) %in% c("year")]

# import predictions

west.revpc.lstm.pred.treated <- read_csv(paste0(results.directory, "lstm/west-revpc/treated-gans/weights.40-0.089.hdf5-west-revpc-test.csv"), col_names = FALSE)
west.revpc.lstm.pred.control <- read_csv(paste0(results.directory, "lstm/west-revpc/control/weights.20-30.174.hdf5-west-revpc-test.csv"), col_names = FALSE)

# Actual versus predicted
west.revpc.lstm <- data.frame(
  "y.pred" = rbind(matrix(NA, west.revpc.n.pre, west.revpc.n.placebo+1), as.matrix(cbind(west.revpc.lstm.pred.treated, west.revpc.lstm.pred.control))),
  "y.true" = cbind(west.revpc.y, west.revpc.x),
  "year" =  rev.pc.y.west$year
)

# Post-period MSE and MAPE (all controls)

west.revpc.control.forecast <- as.matrix(west.revpc.lstm.pred.control)
west.revpc.control.true <- as.matrix(west.revpc.x[(west.revpc.n.pre+1):nrow(west.revpc.x),])

west.revpc.lstm.mse <- error(forecast=west.revpc.control.forecast, true=west.revpc.control.true, method = "mse") # post-intervention MSE
west.revpc.lstm.mse

west.revpc.lstm.preds <- rbind(matrix(NA, west.revpc.n.pre, west.revpc.n.placebo+1), as.matrix(cbind(west.revpc.lstm.pred.treated, west.revpc.lstm.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

west.revpc.treat.forecast <-  as.matrix(west.revpc.lstm.pred.treated)

west.revpc.treat.true <- as.matrix(west.revpc.y[1][(west.revpc.n.pre+1):nrow(west.revpc.y),])

west.revpc.t.stat <- rowMeans(west.revpc.treat.true-west.revpc.treat.forecast) # real t stat

# P-values for both treated and placebo treated

west.revpc.p.values.treated <- PermutationTest(west.revpc.control.forecast, west.revpc.control.true, west.revpc.t.stat, west.revpc.n.placebo, np=10000)

west.revpc.p.values.control <- sapply(1:west.revpc.n.placebo, function(c){
  west.revpc.t.stat.control <- rowMeans(as.matrix(west.revpc.control.true[,c])-as.matrix(west.revpc.control.forecast[,c]))
  PermutationTest(west.revpc.control.forecast[,-c], west.revpc.control.true[,-c], west.revpc.t.stat.control, west.revpc.n.placebo-1, np=10000)
})

lstm.west.revpc.fpr <- sum(west.revpc.p.values.control <=0.05)/length(west.revpc.p.values.control) #FPR
lstm.west.revpc.fpr
sum(p.adjust(west.revpc.p.values.control, "bonferroni") <=0.05)/length(west.revpc.p.values.control) # adjusted

# CIs for treated

lstm.west.revpc.CI.treated <- PermutationCI(west.revpc.control.forecast, west.revpc.control.true, west.revpc.t.stat, west.revpc.n.placebo, c.range=c(-6,6), np=10000, l=1000)

# Plot pointwise impacts

# Pointwise impacts
west.revpc.lstm.control <- data.frame(
  "pointwise.control" = west.revpc.x[(west.revpc.n.pre+1):nrow(west.revpc.x),]-west.revpc.control.forecast,
  "year" =  sort(rev.pc.x.west.imp$year)[sort(rev.pc.x.west.imp$year)>=1862] # x year isn't sorted
)

west.revpc.lstm.treat <- data.frame(
  "pointwise.treat" = west.revpc.y[(west.revpc.n.pre+1):nrow(west.revpc.y),]-west.revpc.treat.forecast, 
  "year" =  rev.pc.y.west$year[rev.pc.y.west$year>=1862]
)

theme.blank <- theme(axis.text=element_text(size=14)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5, size=16)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , panel.grid.major = element_blank()
                     , panel.grid.minor = element_blank()
                     , legend.text=element_text(size=14)
                     , legend.title = element_blank()
                     , legend.position = c(0.8,0.9)
                     , legend.justification = c(1,0))

west.revpc.lstm.control.long <- melt(west.revpc.lstm.control, id="year")  # convert to long format
west.revpc.lstm.control.long$group <- "Control"

west.revpc.lstm.treat.long <- melt(west.revpc.lstm.treat, id="year")  # convert to long format
west.revpc.lstm.treat.long$group <- "Treated"

west.revpc.lstm.long <- rbind(west.revpc.lstm.treat.long, west.revpc.lstm.control.long)

west.revpc.lstm.long$ymin <- NA
west.revpc.lstm.long$ymax <- NA

west.revpc.lstm.long$ymin[west.revpc.lstm.long$group=="Treated"] <- lstm.west.revpc.CI.treated[,1]
west.revpc.lstm.long$ymax[west.revpc.lstm.long$group=="Treated"] <- lstm.west.revpc.CI.treated[,2]

lstm.plot.west.revpc <- ggplot(data=west.revpc.lstm.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log per-capita state government revenue (1982$)") + 
  xlab("Year") +
  scale_x_continuous(breaks=seq(1860, 1980, 20)) +
  scale_alpha_manual(values=c(0.1, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  coord_cartesian(ylim=c(-60, 60)) +
  #ggtitle("lstm treatment effects: Revenue in West") +
  theme.blank + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/lstm-plot-effects-west-revpc.png"), lstm.plot.west.revpc, width=11, height=8.5)

mean(west.revpc.lstm.long$value[west.revpc.lstm.long$variable=="X1"])/mean(west.revpc.y[(west.revpc.n.pre+1):nrow(west.revpc.y),]) # get mean % treatment effect
mean(west.revpc.lstm.long$ymin[west.revpc.lstm.long$variable=="X1"])/mean(west.revpc.y[(west.revpc.n.pre+1):nrow(west.revpc.y),])
mean(west.revpc.lstm.long$ymax[west.revpc.lstm.long$variable=="X1"])/mean(west.revpc.y[(west.revpc.n.pre+1):nrow(west.revpc.y),])

# Plot p-values

west.revpc.lstm.control <- data.frame(
  "p.values.control" = west.revpc.p.values.control,
  "year" =  rev.pc.y.west$year[rev.pc.y.west$year>=1862]
)

west.revpc.lstm.treat <- data.frame(
  "p.values.treat" = west.revpc.p.values.treated,
  "year" =  rev.pc.y.west$year[rev.pc.y.west$year>=1862]
)

west.revpc.lstm.control.long <- melt(west.revpc.lstm.control, id="year")  # convert to long format
west.revpc.lstm.control.long$group <- "Control"

west.revpc.lstm.treat.long <- melt(west.revpc.lstm.treat, id="year")  # convert to long format
west.revpc.lstm.treat.long$group <- "Treated"

west.revpc.lstm.long <- rbind(west.revpc.lstm.treat.long, west.revpc.lstm.control.long)

lstm.plot.pvalues.west.revpc <- ggplot(data=west.revpc.lstm.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  #ggtitle("lstm p-values: Revenue in West") +
  theme.blank + theme(legend.position = c(0.8,0.8)) + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/lstm-plot-pvalues-west-revpc.png"), lstm.plot.pvalues.west.revpc, width=11, height=8.5)

# Plot actual versus predicted with credible intervals for the holdout period

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.25,0.85)
                     , legend.justification = c(1,0))

west.revpc.lstm.plot <- ggplot(data=west.revpc.lstm, aes(x=year)) +
  geom_line(aes(y=west.revpc.y[[1]], colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=west.revpc.lstm.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita state government revenue (1982$)") + xlab("") +
  geom_vline(xintercept=1862, linetype=2) + 
  #ggtitle(paste0("lstm actual vs. counterfactual outcome: Revenue in West")) +
  theme.blank + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/lstm-plot-west-revpc.png"), west.revpc.lstm.plot, width=11, height=8.5)