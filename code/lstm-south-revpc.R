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

south.revpc.n.pre <- nrow(rev.pc.y.south[rev.pc.y.south$year<1866,])-1
south.revpc.n.placebo <- ncol(rev.pc.x.south.imp[!colnames(rev.pc.x.south.imp) %in% c("year")])

south.revpc.x <- rev.pc.x.south.imp[!colnames(rev.pc.x.south.imp) %in% c("year")]
south.revpc.y <- rev.pc.y.south[!colnames(rev.pc.y.south) %in% c("year")]

# import predictions

south.revpc.lstm.pred.treated <- read_csv(paste0(results.directory, "lstm/south-revpc/treated-gans/weights.130-0.129.hdf5-south-revpc-test.csv"), col_names = FALSE)
south.revpc.lstm.pred.control <- read_csv(paste0(results.directory, "lstm/south-revpc/control/weights.2850-0.840.hdf5-south-revpc-test.csv"), col_names = FALSE)

# Actual versus predicted
south.revpc.lstm <- data.frame(
  "y.pred" = rbind(matrix(NA, south.revpc.n.pre, south.revpc.n.placebo+1), as.matrix(cbind(south.revpc.lstm.pred.treated, south.revpc.lstm.pred.control))),
  "y.true" = cbind(south.revpc.y, south.revpc.x),
  "year" =  rev.pc.y.south$year
)

# Post-period MSE and MAPE (all controls)

south.revpc.control.forecast <- as.matrix(south.revpc.lstm.pred.control)
south.revpc.control.true <- as.matrix(south.revpc.x[(south.revpc.n.pre+1):nrow(south.revpc.x),])

south.revpc.lstm.mse <- error(forecast=south.revpc.control.forecast, true=south.revpc.control.true, method = "mse") # post-intervention MSE
south.revpc.lstm.mse

south.revpc.lstm.preds <- rbind(matrix(NA, south.revpc.n.pre, south.revpc.n.placebo+1), as.matrix(cbind(south.revpc.lstm.pred.treated, south.revpc.lstm.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

south.revpc.treat.forecast <-  as.matrix(south.revpc.lstm.pred.treated)

south.revpc.treat.true <- as.matrix(south.revpc.y[1][(south.revpc.n.pre+1):nrow(south.revpc.y),])

south.revpc.t.stat <- rowMeans(south.revpc.treat.true-south.revpc.treat.forecast) # real t stat

# P-values for both treated and placebo treated

south.revpc.p.values.treated <- PermutationTest(south.revpc.control.forecast, south.revpc.control.true, south.revpc.t.stat, south.revpc.n.placebo, np=10000)

south.revpc.p.values.control <- sapply(1:south.revpc.n.placebo, function(c){
  south.revpc.t.stat.control <- rowMeans(as.matrix(south.revpc.control.true[,c])-as.matrix(south.revpc.control.forecast[,c]))
  PermutationTest(south.revpc.control.forecast[,-c], south.revpc.control.true[,-c], south.revpc.t.stat.control, south.revpc.n.placebo-1, np=10000)
})

lstm.south.revpc.fpr <- sum(south.revpc.p.values.control <=0.05)/length(south.revpc.p.values.control) #FPR
lstm.south.revpc.fpr
sum(p.adjust(south.revpc.p.values.control, "bonferroni") <=0.05)/length(south.revpc.p.values.control) # adjusted

# CIs for treated

lstm.south.revpc.CI.treated <- PermutationCI(south.revpc.control.forecast, south.revpc.control.true, south.revpc.t.stat, south.revpc.n.placebo, c.range=c(-9,9), np=10000, l=1000)

# Plot pointwise impacts

# Pointwise impacts
south.revpc.lstm.control <- data.frame(
  "pointwise.control" = south.revpc.x[(south.revpc.n.pre+1):nrow(south.revpc.x),]-south.revpc.control.forecast,
  "year" =  sort(rev.pc.x.south.imp$year)[sort(rev.pc.x.south.imp$year)>=1865] # x year isn't sorted
)

south.revpc.lstm.treat <- data.frame(
  "pointwise.treat" = south.revpc.y[(south.revpc.n.pre+1):nrow(south.revpc.y),]-south.revpc.treat.forecast, 
  "year" =  rev.pc.y.south$year[rev.pc.y.south$year>=1865]
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

south.revpc.lstm.control.long <- melt(south.revpc.lstm.control, id="year")  # convert to long format
south.revpc.lstm.control.long$group <- "Control"

south.revpc.lstm.treat.long <- melt(south.revpc.lstm.treat, id="year")  # convert to long format
south.revpc.lstm.treat.long$group <- "Treated"

south.revpc.lstm.long <- rbind(south.revpc.lstm.treat.long, south.revpc.lstm.control.long)

south.revpc.lstm.long$ymin <- NA
south.revpc.lstm.long$ymax <- NA

south.revpc.lstm.long$ymin[south.revpc.lstm.long$group=="Treated"] <- lstm.south.revpc.CI.treated[,1]
south.revpc.lstm.long$ymax[south.revpc.lstm.long$group=="Treated"] <- lstm.south.revpc.CI.treated[,2]

lstm.plot.south.revpc <- ggplot(data=south.revpc.lstm.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log per-capita state government total revenue (1982$)") + 
  xlab("Year") +
  scale_x_continuous(breaks=seq(1860, 1980, 20)) +
  scale_alpha_manual(values=c(0.1, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  coord_cartesian(ylim=c(-10, 10)) +
  #ggtitle("lstm treatment effects: Revenue in South") +
  theme.blank + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/lstm-plot-effects-south-revpc.png"), lstm.plot.south.revpc, width=11, height=8.5)

mean(south.revpc.lstm.long$value[south.revpc.lstm.long$variable=="X1"])/mean(south.revpc.y[(south.revpc.n.pre+1):nrow(south.revpc.y),]) # get mean % treatment effect
mean(south.revpc.lstm.long$ymin[south.revpc.lstm.long$variable=="X1"])/mean(south.revpc.y[(south.revpc.n.pre+1):nrow(south.revpc.y),])
mean(south.revpc.lstm.long$ymax[south.revpc.lstm.long$variable=="X1"])/mean(south.revpc.y[(south.revpc.n.pre+1):nrow(south.revpc.y),])

# Plot p-values

south.revpc.lstm.control <- data.frame(
  "p.values.control" = south.revpc.p.values.control,
  "year" =  rev.pc.y.south$year[rev.pc.y.south$year>=1865]
)

south.revpc.lstm.treat <- data.frame(
  "p.values.treat" = south.revpc.p.values.treated,
  "year" =  rev.pc.y.south$year[rev.pc.y.south$year>=1865]
)

south.revpc.lstm.control.long <- melt(south.revpc.lstm.control, id="year")  # convert to long format
south.revpc.lstm.control.long$group <- "Control"

south.revpc.lstm.treat.long <- melt(south.revpc.lstm.treat, id="year")  # convert to long format
south.revpc.lstm.treat.long$group <- "Treated"

south.revpc.lstm.long <- rbind(south.revpc.lstm.treat.long, south.revpc.lstm.control.long)

lstm.plot.pvalues.south.revpc <- ggplot(data=south.revpc.lstm.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
 # ggtitle("lstm p-values: Revenue in South") +
  theme.blank + theme(legend.position = c(0.8,0.8)) + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/lstm-plot-pvalues-south-revpc.png"), lstm.plot.pvalues.south.revpc, width=11, height=8.5)

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

south.revpc.lstm.plot <- ggplot(data=south.revpc.lstm, aes(x=year)) +
  geom_line(aes(y=south.revpc.y[[1]], colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=south.revpc.lstm.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita state government total revenue (1982$)") + xlab("") +
  geom_vline(xintercept=1866, linetype=2) + 
 # ggtitle(paste0("lstm actual vs. counterfactual outcome: Revenue in South")) +
  theme.blank + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/lstm-plot-south-revpc.png"), south.revpc.lstm.plot, width=11, height=8.5)