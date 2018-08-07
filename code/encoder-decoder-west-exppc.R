#####################################
### encoder.decoder ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)
library(ftsa)

# setup

west.exppc.n.pre <- nrow(exp.pc.y.west[exp.pc.y.west$year<1862,])
west.exppc.n.placebo <- ncol(exp.pc.x.west.imp[!colnames(exp.pc.x.west.imp) %in% c("year")])

west.exppc.x <- exp.pc.x.west.imp[!colnames(exp.pc.x.west.imp) %in% c("year")]
west.exppc.y <- exp.pc.y.west[!colnames(exp.pc.y.west) %in% c("year")]

# import predictions

west.exppc.encoder.decoder.pred.treated <- read_csv(paste0(results.directory, "encoder-decoder/west-exppc/treated-gans/weights.450-8.144.hdf5-west-exppc-test.csv"), col_names = FALSE)
west.exppc.encoder.decoder.pred.control <- read_csv(paste0(results.directory, "encoder-decoder/west-exppc/control/weights.1000-2.424.hdf5-west-exppc-test.csv"), col_names = FALSE)

# Actual versus predicted
west.exppc.encoder.decoder <- data.frame(
  "y.pred" = rbind(matrix(NA, west.exppc.n.pre, west.exppc.n.placebo+1), as.matrix(cbind(west.exppc.encoder.decoder.pred.treated, west.exppc.encoder.decoder.pred.control))),
  "y.true" = cbind(west.exppc.y, west.exppc.x),
  "year" =  exp.pc.y.west$year
)

# Post-period MSE and MAPE (all controls)

west.exppc.control.forecast <- as.matrix(west.exppc.encoder.decoder.pred.control)
west.exppc.control.true <- as.matrix(west.exppc.x[(west.exppc.n.pre+1):nrow(west.exppc.x),])

west.exppc.encoder.decoder.mse <- error(forecast=west.exppc.control.forecast, true=west.exppc.control.true, method = "mse") # post-intervention MSE
west.exppc.encoder.decoder.mse

west.exppc.encoder.decoder.preds <- rbind(matrix(NA, west.exppc.n.pre, west.exppc.n.placebo+1), as.matrix(cbind(west.exppc.encoder.decoder.pred.treated, west.exppc.encoder.decoder.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

west.exppc.treat.forecast <-  as.matrix(west.exppc.encoder.decoder.pred.treated)

west.exppc.treat.true <- as.matrix(west.exppc.y[1][(west.exppc.n.pre+1):nrow(west.exppc.y),])

west.exppc.t.stat <- rowMeans(west.exppc.treat.true-west.exppc.treat.forecast) # real t stat

# P-values for both treated and placebo treated

west.exppc.p.values.treated <- PermutationTest(west.exppc.control.forecast, west.exppc.control.true, west.exppc.t.stat, west.exppc.n.placebo, np=10000)

west.exppc.p.values.control <- sapply(1:west.exppc.n.placebo, function(c){
  west.exppc.t.stat.control <- rowMeans(as.matrix(west.exppc.control.true[,c])-as.matrix(west.exppc.control.forecast[,c]))
  PermutationTest(west.exppc.control.forecast[,-c], west.exppc.control.true[,-c], west.exppc.t.stat.control, west.exppc.n.placebo-1, np=10000)
})

encoder.decoder.west.exppc.fpr <- sum(west.exppc.p.values.control <=0.05)/length(west.exppc.p.values.control) #FPR
encoder.decoder.west.exppc.fpr
sum(p.adjust(west.exppc.p.values.control, "bonferroni") <=0.05)/length(west.exppc.p.values.control) # adjusted

# CIs for treated

west.exppc.CI.treated <- PermutationCI(west.exppc.control.forecast, west.exppc.control.true, west.exppc.t.stat, west.exppc.n.placebo, c.range=c(-6,6), np=10000, l=1000)

# Plot pointwise impacts

# Pointwise impacts
west.exppc.encoder.decoder.control <- data.frame(
  "pointwise.control" = west.exppc.x[(west.exppc.n.pre+1):nrow(west.exppc.x),]-west.exppc.control.forecast,
  "year" =  sort(exp.pc.x.west.imp$year)[sort(exp.pc.x.west.imp$year)>=1862] # x year isn't sorted
)

west.exppc.encoder.decoder.treat <- data.frame(
  "pointwise.treat" = west.exppc.y[(west.exppc.n.pre+1):nrow(west.exppc.y),]-west.exppc.treat.forecast, 
  "year" =  exp.pc.y.west$year[exp.pc.y.west$year>=1862]
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

west.exppc.encoder.decoder.control.long <- melt(west.exppc.encoder.decoder.control, id="year")  # convert to long format
west.exppc.encoder.decoder.control.long$group <- "Control"

west.exppc.encoder.decoder.treat.long <- melt(west.exppc.encoder.decoder.treat, id="year")  # convert to long format
west.exppc.encoder.decoder.treat.long$group <- "Treated"

west.exppc.encoder.decoder.long <- rbind(west.exppc.encoder.decoder.treat.long, west.exppc.encoder.decoder.control.long)

west.exppc.encoder.decoder.long$ymin <- NA
west.exppc.encoder.decoder.long$ymax <- NA

west.exppc.encoder.decoder.long$ymin[west.exppc.encoder.decoder.long$group=="Treated"] <- west.exppc.CI.treated[,1]
west.exppc.encoder.decoder.long$ymax[west.exppc.encoder.decoder.long$group=="Treated"] <- west.exppc.CI.treated[,2]

encoder.decoder.plot.west.exppc <- ggplot(data=west.exppc.encoder.decoder.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log per-capita state government total expenditure (1982$)") + 
  xlab("Year") +
  scale_x_continuous(breaks=seq(1860, 1980, 20)) +
  scale_alpha_manual(values=c(0.1, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  coord_cartesian(ylim=c(-8, 8)) +
 # ggtitle("Encoder-decoder treatment effects: Expenditure in West") +
  theme.blank + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-effects-west-exppc.png"), encoder.decoder.plot.west.exppc, width=11, height=8.5)

mean(west.exppc.encoder.decoder.long$value[west.exppc.encoder.decoder.long$variable=="X1"])/mean(west.exppc.y[(west.exppc.n.pre+1):nrow(west.exppc.y),]) # get mean % treatment effect
mean(west.exppc.encoder.decoder.long$ymin[west.exppc.encoder.decoder.long$variable=="X1"])/mean(west.exppc.y[(west.exppc.n.pre+1):nrow(west.exppc.y),])
mean(west.exppc.encoder.decoder.long$ymax[west.exppc.encoder.decoder.long$variable=="X1"])/mean(west.exppc.y[(west.exppc.n.pre+1):nrow(west.exppc.y),])

exp(mean(west.exppc.y[(west.exppc.n.pre+1):nrow(west.exppc.y),])*mean(west.exppc.encoder.decoder.long$value[west.exppc.encoder.decoder.long$variable=="X1"])/mean(west.exppc.y[(west.exppc.n.pre+1):nrow(west.exppc.y),])) 
exp(mean(west.exppc.y[(west.exppc.n.pre+1):nrow(west.exppc.y),])*mean(west.exppc.encoder.decoder.long$ymin[west.exppc.encoder.decoder.long$variable=="X1"])/mean(west.exppc.y[(west.exppc.n.pre+1):nrow(west.exppc.y),])) # mean in real terms
exp(mean(west.exppc.y[(west.exppc.n.pre+1):nrow(west.exppc.y),])*mean(west.exppc.encoder.decoder.long$ymax[west.exppc.encoder.decoder.long$variable=="X1"])/mean(west.exppc.y[(west.exppc.n.pre+1):nrow(west.exppc.y),]))

# Plot p-values

west.exppc.encoder.decoder.control <- data.frame(
  "p.values.control" = west.exppc.p.values.control,
  "year" =  exp.pc.y.west$year[exp.pc.y.west$year>=1862]
)

west.exppc.encoder.decoder.treat <- data.frame(
  "p.values.treat" = west.exppc.p.values.treated,
  "year" =  exp.pc.y.west$year[exp.pc.y.west$year>=1862]
)

west.exppc.encoder.decoder.control.long <- melt(west.exppc.encoder.decoder.control, id="year")  # convert to long format
west.exppc.encoder.decoder.control.long$group <- "Control"

west.exppc.encoder.decoder.treat.long <- melt(west.exppc.encoder.decoder.treat, id="year")  # convert to long format
west.exppc.encoder.decoder.treat.long$group <- "Treated"

west.exppc.encoder.decoder.long <- rbind(west.exppc.encoder.decoder.treat.long, west.exppc.encoder.decoder.control.long)

encoder.decoder.plot.pvalues.west.exppc <- ggplot(data=west.exppc.encoder.decoder.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  #ggtitle("Encoder-decoder p-values: Expenditure in West") +
  theme.blank + theme(legend.position = c(0.8,0.8)) + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-pvalues-west-exppc.png"), encoder.decoder.plot.pvalues.west.exppc, width=11, height=8.5)

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

west.exppc.encoder.decoder.plot <- ggplot(data=west.exppc.encoder.decoder, aes(x=year)) +
  geom_line(aes(y=west.exppc.y[[1]], colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=west.exppc.encoder.decoder.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita state government total expenditure (1982$)") + xlab("") +
  geom_vline(xintercept=1862, linetype=2) + 
  #ggtitle(paste0("Encoder-decoder actual vs. counterfactual outcome: Expenditure in West")) +
  theme.blank + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-west-exppc.png"), west.exppc.encoder.decoder.plot, width=11, height=8.5)