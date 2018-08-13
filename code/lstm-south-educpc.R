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

south.educpc.n.pre <- nrow(educ.pc.y.south[educ.pc.y.south$year<1866,])-1

south.educpc.x.indices <- grep("educ.pc", colnames(educ.pc.x.south.imp))
south.educpc.n.placebo <- ncol(educ.pc.x.south.imp[south.educpc.x.indices])

south.educpc.x <- educ.pc.x.south.imp[south.educpc.x.indices]
south.educpc.y <- educ.pc.y.south[!colnames(educ.pc.y.south) %in% c("year")]

# import predictions

south.educpc.lstm.pred.treated <- read_csv(paste0(results.directory, "lstm/south-educpc/treated-gans/weights.320-0.084.hdf5-south-educpc-test.csv"), col_names = FALSE)
south.educpc.lstm.pred.control <- read_csv(paste0(results.directory, "lstm/south-educpc/control/weights.1040-0.771.hdf5-south-educpc-test.csv"), col_names = FALSE)

# Actual versus predicted
south.educpc.lstm <- data.frame(
  "y.pred" = rbind(matrix(NA, south.educpc.n.pre, south.educpc.n.placebo+1), as.matrix(cbind(south.educpc.lstm.pred.treated, south.educpc.lstm.pred.control))),
  "y.true" = cbind(south.educpc.y, south.educpc.x),
  "year" =  educ.pc.y.south$year
)

# Post-period MSE and MAPE (all controls)

south.educpc.control.forecast <- as.matrix(south.educpc.lstm.pred.control)
south.educpc.control.true <- as.matrix(south.educpc.x[(south.educpc.n.pre+1):nrow(south.educpc.x),])

south.educpc.lstm.mse <- mean((south.educpc.control.true-south.educpc.control.forecast)**2) # post-intervention MSE
south.educpc.lstm.mse

south.educpc.lstm.preds <- rbind(matrix(NA, south.educpc.n.pre, south.educpc.n.placebo+1), as.matrix(cbind(south.educpc.lstm.pred.treated, south.educpc.lstm.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

south.educpc.treat.forecast <-  as.matrix(south.educpc.lstm.pred.treated)

south.educpc.treat.true <- as.matrix(south.educpc.y[1][(south.educpc.n.pre+1):nrow(south.educpc.y),])

south.educpc.t.stat <- rowMeans(south.educpc.treat.true-south.educpc.treat.forecast) # real t stat

# P-values for both treated and placebo treated

south.educpc.p.values.treated <- PermutationTest(south.educpc.control.forecast, south.educpc.control.true, south.educpc.t.stat, south.educpc.n.placebo, np=10000)

south.educpc.p.values.control <- sapply(1:south.educpc.n.placebo, function(c){
  south.educpc.t.stat.control <- rowMeans(as.matrix(south.educpc.control.true[,c])-as.matrix(south.educpc.control.forecast[,c]))
  PermutationTest(south.educpc.control.forecast[,-c], south.educpc.control.true[,-c], south.educpc.t.stat.control, south.educpc.n.placebo-1, np=10000)
})

lstm.south.educpc.fpr <- sum(south.educpc.p.values.control <=0.05)/length(south.educpc.p.values.control) #FPR
lstm.south.educpc.fpr
sum(p.adjust(south.educpc.p.values.control, "bonferroni") <=0.05)/length(south.educpc.p.values.control) # adjusted

# CIs for treated

south.educpc.CI.treated <- PermutationCI(south.educpc.control.forecast, south.educpc.control.true, south.educpc.t.stat, south.educpc.n.placebo, c.range=c(-5,5), np=10000, l=1000)

# Plot pointwise impacts

# Pointwise impacts
south.educpc.lstm.control <- data.frame(
  "pointwise.control" = south.educpc.x[(south.educpc.n.pre+1):nrow(south.educpc.x),]-south.educpc.control.forecast,
  "year" =  educ.pc.x.south.imp$year[educ.pc.x.south.imp$year>=1865]
)

south.educpc.lstm.treat <- data.frame(
  "pointwise.treat" = south.educpc.y[(south.educpc.n.pre+1):nrow(south.educpc.y),]-south.educpc.treat.forecast, 
  "year" =  educ.pc.y.south$year[educ.pc.y.south$year>=1865]
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
                     , legend.position = c(0.2,0.9)
                     , legend.justification = c(1,0))

south.educpc.lstm.control.long <- melt(south.educpc.lstm.control, id="year")  # convert to long format
south.educpc.lstm.control.long$group <- "Control"

south.educpc.lstm.treat.long <- melt(south.educpc.lstm.treat, id="year")  # convert to long format
south.educpc.lstm.treat.long$group <- "Treated"

south.educpc.lstm.long <- rbind(south.educpc.lstm.treat.long, south.educpc.lstm.control.long)

south.educpc.lstm.long$ymin <- NA
south.educpc.lstm.long$ymax <- NA

south.educpc.lstm.long$ymin[south.educpc.lstm.long$group=="Treated"] <- south.educpc.CI.treated[,1]
south.educpc.lstm.long$ymax[south.educpc.lstm.long$group=="Treated"] <- south.educpc.CI.treated[,2]

lstm.plot.south.educpc <- ggplot(data=south.educpc.lstm.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log per-capita state government education spending (1982$)") + 
  xlab("Year") +
  scale_x_continuous(breaks=seq(1860, 1980, 20)) +
  scale_alpha_manual(values=c(0.1, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  coord_cartesian(ylim=c(-10, 10)) +
  #ggtitle("lstm treatment effects: Education spending in South") +
  theme.blank + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/lstm-plot-effects-south-educpc.png"), lstm.plot.south.educpc, width=11, height=8.5)

mean(south.educpc.lstm.long$value[south.educpc.lstm.long$variable=="X1"])
mean(south.educpc.lstm.long$ymin[south.educpc.lstm.long$variable=="X1"])
mean(south.educpc.lstm.long$ymax[south.educpc.lstm.long$variable=="X1"])

# Plot p-values

south.educpc.lstm.control <- data.frame(
  "p.values.control" = south.educpc.p.values.control,
  "year" =  educ.pc.y.south$year[educ.pc.y.south$year>=1865]
)

south.educpc.lstm.treat <- data.frame(
  "p.values.treat" = south.educpc.p.values.treated,
  "year" =  educ.pc.y.south$year[educ.pc.y.south$year>=1865]
)

south.educpc.lstm.control.long <- melt(south.educpc.lstm.control, id="year")  # convert to long format
south.educpc.lstm.control.long$group <- "Control"

south.educpc.lstm.treat.long <- melt(south.educpc.lstm.treat, id="year")  # convert to long format
south.educpc.lstm.treat.long$group <- "Treated"

south.educpc.lstm.long <- rbind(south.educpc.lstm.treat.long, south.educpc.lstm.control.long)

lstm.plot.pvalues.south.educpc <- ggplot(data=south.educpc.lstm.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  #ggtitle("lstm p-values: Education spending in South") +
  theme.blank + theme(legend.position = c(0.8,0.8)) + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/lstm-plot-pvalues-south-educpc.png"), lstm.plot.pvalues.south.educpc, width=11, height=8.5)

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

south.educpc.lstm.plot <- ggplot(data=south.educpc.lstm, aes(x=year)) +
  geom_line(aes(y=south.educpc.y[[1]], colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=south.educpc.lstm.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita state government education spending (1982$)") + xlab("") +
  geom_vline(xintercept=1866, linetype=2) + 
#  ggtitle(paste0("lstm actual vs. counterfactual outcome: Education spending in South")) +
  theme.blank + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/lstm-plot-south-educpc.png"), south.educpc.lstm.plot, width=11, height=8.5)