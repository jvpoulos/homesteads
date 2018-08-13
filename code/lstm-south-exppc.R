#####################################
### lstm ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

# setup

south.exppc.n.pre <- nrow(exp.pc.y.south[exp.pc.y.south$year<1866,])-1

south.exppc.x.indices <- grep("exp.pc", colnames(exp.pc.x.south.imp))
south.exppc.n.placebo <- ncol(exp.pc.x.south.imp[south.exppc.x.indices])

south.exppc.x <- exp.pc.x.south.imp[south.exppc.x.indices]
south.exppc.y <- exp.pc.y.south[!colnames(exp.pc.y.south) %in% c("year")]

# import predictions

south.exppc.lstm.pred.treated <- read_csv(paste0(results.directory, "lstm/south-exppc/treated-gans/weights.30-2.550.hdf5-south-exppc-test.csv"), col_names = FALSE)
south.exppc.lstm.pred.control <- read_csv(paste0(results.directory, "lstm/south-exppc/control/weights.1920-18.812.hdf5-south-exppc-test.csv"), col_names = FALSE)

# Actual versus predicted
south.exppc.lstm <- data.frame(
  "y.pred" = rbind(matrix(NA, south.exppc.n.pre, south.exppc.n.placebo+1), as.matrix(cbind(south.exppc.lstm.pred.treated, south.exppc.lstm.pred.control))),
  "y.true" = cbind(south.exppc.y, south.exppc.x),
  "year" =  exp.pc.y.south$year
)

# Post-period MSE and MAPE (all controls)

south.exppc.control.forecast <- as.matrix(south.exppc.lstm.pred.control)
south.exppc.control.true <- as.matrix(south.exppc.x[(south.exppc.n.pre+1):nrow(south.exppc.x),])

south.exppc.lstm.mse <- mean((south.exppc.control.true-south.exppc.control.forecast)**2) # post-intervention MSE
south.exppc.lstm.mse

south.exppc.lstm.preds <- rbind(matrix(NA, south.exppc.n.pre, south.exppc.n.placebo+1), as.matrix(cbind(south.exppc.lstm.pred.treated, south.exppc.lstm.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

south.exppc.treat.forecast <-  as.matrix(south.exppc.lstm.pred.treated)

south.exppc.treat.true <- as.matrix(south.exppc.y[1][(south.exppc.n.pre+1):nrow(south.exppc.y),])

south.exppc.t.stat <- rowMeans(south.exppc.treat.true-south.exppc.treat.forecast) # real t stat

# P-values for both treated and placebo treated

south.exppc.p.values.treated <- PermutationTest(south.exppc.control.forecast, south.exppc.control.true, south.exppc.t.stat, south.exppc.n.placebo, np=10000)

south.exppc.p.values.control <- sapply(1:south.exppc.n.placebo, function(c){
  south.exppc.t.stat.control <- rowMeans(as.matrix(south.exppc.control.true[,c])-as.matrix(south.exppc.control.forecast[,c]))
  PermutationTest(south.exppc.control.forecast[,-c], south.exppc.control.true[,-c], south.exppc.t.stat.control, south.exppc.n.placebo-1, np=10000)
})

lstm.south.exppc.fpr <- sum(south.exppc.p.values.control <=0.05)/length(south.exppc.p.values.control) #FPR
lstm.south.exppc.fpr
sum(p.adjust(south.exppc.p.values.control, "bonferroni") <=0.05)/length(south.exppc.p.values.control) # adjusted

# CIs for treated

south.exppc.CI.treated <- PermutationCI(south.exppc.control.forecast, south.exppc.control.true, south.exppc.t.stat, south.exppc.n.placebo, c.range=c(-10,10), np=10000, l=1000)

# Plot pointwise impacts

# Pointwise impacts
south.exppc.lstm.control <- data.frame(
  "pointwise.control" = south.exppc.x[(south.exppc.n.pre+1):nrow(south.exppc.x),]-south.exppc.control.forecast,
  "year" =  exp.pc.x.south.imp$year[exp.pc.x.south.imp$year>=1865]
)

south.exppc.lstm.treat <- data.frame(
  "pointwise.treat" = south.exppc.y[(south.exppc.n.pre+1):nrow(south.exppc.y),]-south.exppc.treat.forecast, 
  "year" =  exp.pc.y.south$year[exp.pc.y.south$year>=1865]
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

south.exppc.lstm.control.long <- melt(south.exppc.lstm.control, id="year")  # convert to long format
south.exppc.lstm.control.long$group <- "Control"

south.exppc.lstm.treat.long <- melt(south.exppc.lstm.treat, id="year")  # convert to long format
south.exppc.lstm.treat.long$group <- "Treated"

south.exppc.lstm.long <- rbind(south.exppc.lstm.treat.long, south.exppc.lstm.control.long)

south.exppc.lstm.long$ymin <- NA
south.exppc.lstm.long$ymax <- NA

south.exppc.lstm.long$ymin[south.exppc.lstm.long$group=="Treated"] <- south.exppc.CI.treated[,1]
south.exppc.lstm.long$ymax[south.exppc.lstm.long$group=="Treated"] <- south.exppc.CI.treated[,2]

lstm.plot.south.exppc <- ggplot(data=south.exppc.lstm.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log per-capita state government total expenditure (1982$)") + 
  xlab("Year") +
  scale_x_continuous(breaks=seq(1860, 1980, 20)) +
  scale_alpha_manual(values=c(0.1, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  coord_cartesian(ylim=c(-10, 10)) +
  #ggtitle("lstm treatment effects: Expenditure in South") +
  theme.blank + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/lstm-plot-effects-south-exppc.png"), lstm.plot.south.exppc, width=11, height=8.5)

mean(south.exppc.lstm.long$value[south.exppc.lstm.long$variable=="X1"])
mean(south.exppc.lstm.long$ymin[south.exppc.lstm.long$variable=="X1"])
mean(south.exppc.lstm.long$ymax[south.exppc.lstm.long$variable=="X1"])

# Plot p-values

south.exppc.lstm.control <- data.frame(
  "p.values.control" = south.exppc.p.values.control,
  "year" =  exp.pc.y.south$year[exp.pc.y.south$year>=1865]
)

south.exppc.lstm.treat <- data.frame(
  "p.values.treat" = south.exppc.p.values.treated,
  "year" =  exp.pc.y.south$year[exp.pc.y.south$year>=1865]
)

south.exppc.lstm.control.long <- melt(south.exppc.lstm.control, id="year")  # convert to long format
south.exppc.lstm.control.long$group <- "Control"

south.exppc.lstm.treat.long <- melt(south.exppc.lstm.treat, id="year")  # convert to long format
south.exppc.lstm.treat.long$group <- "Treated"

south.exppc.lstm.long <- rbind(south.exppc.lstm.treat.long, south.exppc.lstm.control.long)

lstm.plot.pvalues.south.exppc <- ggplot(data=south.exppc.lstm.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  #ggtitle("lstm p-values: Expenditure in South") +
  theme.blank + theme(legend.position = c(0.8,0.8)) + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/lstm-plot-pvalues-south-exppc.png"), lstm.plot.pvalues.south.exppc, width=11, height=8.5)

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

south.exppc.lstm.plot <- ggplot(data=south.exppc.lstm, aes(x=year)) +
  geom_line(aes(y=south.exppc.y[[1]], colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=south.exppc.lstm.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita state government total expenditure (1982$)") + xlab("") +
  geom_vline(xintercept=1866, linetype=2) + 
 # ggtitle(paste0("lstm actual vs. counterfactual outcome: Expenditure in South")) +
  theme.blank + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/lstm-plot-south-exppc.png"), south.exppc.lstm.plot, width=11, height=8.5)