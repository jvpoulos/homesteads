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

south.revpc.n.pre <- nrow(rev.pc.y.south[rev.pc.y.south$year<1866,])-1

south.revpc.x.indices <- grep("rev.pc", colnames(rev.pc.x.south.imp))
south.revpc.n.placebo <- ncol(rev.pc.x.south.imp[south.revpc.x.indices])

south.revpc.x <- rev.pc.x.south.imp[south.revpc.x.indices]
south.revpc.y <- rev.pc.y.south[!colnames(rev.pc.y.south) %in% c("year")]

# import predictions

south.revpc.encoder.decoder.pred.treated <- read_csv(paste0(results.directory, "encoder-decoder/south-revpc/treated-gans/weights.1290-0.001.hdf5-south-revpc-test.csv"), col_names = FALSE)
south.revpc.encoder.decoder.pred.control <- read_csv(paste0(results.directory, "encoder-decoder/south-revpc/control/weights.1140-0.347.hdf5-south-revpc-test.csv"), col_names = FALSE)

# Actual versus predicted
south.revpc.encoder.decoder <- data.frame(
  "y.pred" = rbind(matrix(NA, south.revpc.n.pre, south.revpc.n.placebo+1), as.matrix(cbind(south.revpc.encoder.decoder.pred.treated, south.revpc.encoder.decoder.pred.control))),
  "y.true" = cbind(south.revpc.y, south.revpc.x),
  "year" =  rev.pc.y.south$year
)

# Post-period MSE and MAPE (all controls)

south.revpc.control.forecast <- as.matrix(south.revpc.encoder.decoder.pred.control)
south.revpc.control.true <- as.matrix(south.revpc.x[(south.revpc.n.pre+1):nrow(south.revpc.x),])

south.revpc.encoder.decoder.mse <- error(forecast=south.revpc.control.forecast, true=south.revpc.control.true, method = "mse") # post-intervention MSE
south.revpc.encoder.decoder.mse

south.revpc.encoder.decoder.preds <- rbind(matrix(NA, south.revpc.n.pre, south.revpc.n.placebo+1), as.matrix(cbind(south.revpc.encoder.decoder.pred.treated, south.revpc.encoder.decoder.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

south.revpc.treat.forecast <-  as.matrix(south.revpc.encoder.decoder.pred.treated)

south.revpc.treat.true <- as.matrix(south.revpc.y[1][(south.revpc.n.pre+1):nrow(south.revpc.y),])

south.revpc.t.stat <- rowMeans(south.revpc.treat.true-south.revpc.treat.forecast) # real t stat

# P-values for both treated and placebo treated

south.revpc.p.values.treated <- PermutationTest(south.revpc.control.forecast, south.revpc.control.true, south.revpc.t.stat, south.revpc.n.placebo, np=10000)

south.revpc.p.values.control <- sapply(1:south.revpc.n.placebo, function(c){
  south.revpc.t.stat.control <- rowMeans(as.matrix(south.revpc.control.true[,c])-as.matrix(south.revpc.control.forecast[,c]))
  PermutationTest(south.revpc.control.forecast[,-c], south.revpc.control.true[,-c], south.revpc.t.stat.control, south.revpc.n.placebo-1, np=10000)
})

encoder.decoder.south.revpc.fpr <- sum(south.revpc.p.values.control <=0.05)/length(south.revpc.p.values.control) #FPR
encoder.decoder.south.revpc.fpr
sum(p.adjust(south.revpc.p.values.control, "bonferroni") <=0.05)/length(south.revpc.p.values.control) # adjusted

# CIs for treated

south.revpc.CI.treated <- PermutationCI(south.revpc.control.forecast, south.revpc.control.true, south.revpc.t.stat, south.revpc.n.placebo, c.range=c(-10,10), np=20000, l=1000)

# Plot pointwise impacts

# Pointwise impacts
south.revpc.encoder.decoder.control <- data.frame(
  "pointwise.control" = south.revpc.x[(south.revpc.n.pre+1):nrow(south.revpc.x),]-south.revpc.control.forecast,
  "year" =  rev.pc.x.south.imp$year
)

south.revpc.encoder.decoder.treat <- data.frame(
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

south.revpc.encoder.decoder.control.long <- melt(south.revpc.encoder.decoder.control, id="year")  # convert to long format
south.revpc.encoder.decoder.control.long$group <- "Control"

south.revpc.encoder.decoder.treat.long <- melt(south.revpc.encoder.decoder.treat, id="year")  # convert to long format
south.revpc.encoder.decoder.treat.long$group <- "Treated"

south.revpc.encoder.decoder.long <- rbind(south.revpc.encoder.decoder.treat.long, south.revpc.encoder.decoder.control.long)

south.revpc.encoder.decoder.long$ymin <- NA
south.revpc.encoder.decoder.long$ymax <- NA

south.revpc.encoder.decoder.long$ymin[south.revpc.encoder.decoder.long$group=="Treated"] <- south.revpc.CI.treated[,1]
south.revpc.encoder.decoder.long$ymax[south.revpc.encoder.decoder.long$group=="Treated"] <- south.revpc.CI.treated[,2]

encoder.decoder.plot.south.revpc <- ggplot(data=south.revpc.encoder.decoder.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
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
  #ggtitle("Encoder-decoder treatment effects: Revenue in South") +
  theme.blank + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-effects-south-revpc.png"), encoder.decoder.plot.south.revpc, width=11, height=8.5)

mean(south.revpc.encoder.decoder.long$value[south.revpc.encoder.decoder.long$variable=="X1"])
mean(south.revpc.encoder.decoder.long$ymin[south.revpc.encoder.decoder.long$variable=="X1"])
mean(south.revpc.encoder.decoder.long$ymax[south.revpc.encoder.decoder.long$variable=="X1"])

# Plot p-values

south.revpc.encoder.decoder.control <- data.frame(
  "p.values.control" = south.revpc.p.values.control,
  "year" =  rev.pc.y.south$year[rev.pc.y.south$year>=1865]
)

south.revpc.encoder.decoder.treat <- data.frame(
  "p.values.treat" = south.revpc.p.values.treated,
  "year" =  rev.pc.y.south$year[rev.pc.y.south$year>=1865]
)

south.revpc.encoder.decoder.control.long <- melt(south.revpc.encoder.decoder.control, id="year")  # convert to long format
south.revpc.encoder.decoder.control.long$group <- "Control"

south.revpc.encoder.decoder.treat.long <- melt(south.revpc.encoder.decoder.treat, id="year")  # convert to long format
south.revpc.encoder.decoder.treat.long$group <- "Treated"

south.revpc.encoder.decoder.long <- rbind(south.revpc.encoder.decoder.treat.long, south.revpc.encoder.decoder.control.long)

encoder.decoder.plot.pvalues.south.revpc <- ggplot(data=south.revpc.encoder.decoder.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
 # ggtitle("Encoder-decoder p-values: Revenue in South") +
  theme.blank + theme(legend.position = c(0.8,0.8)) + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-pvalues-south-revpc.png"), encoder.decoder.plot.pvalues.south.revpc, width=11, height=8.5)

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

south.revpc.encoder.decoder.plot <- ggplot(data=south.revpc.encoder.decoder, aes(x=year)) +
  geom_line(aes(y=south.revpc.y[[1]], colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=south.revpc.encoder.decoder.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita state government total revenue (1982$)") + xlab("") +
  geom_vline(xintercept=1866, linetype=2) + 
 # ggtitle(paste0("Encoder-decoder actual vs. counterfactual outcome: Revenue in South")) +
  theme.blank + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-south-revpc.png"), south.revpc.encoder.decoder.plot, width=11, height=8.5)