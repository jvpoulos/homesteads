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

west.educpc.n.pre <- nrow(educ.pc.y.west[educ.pc.y.west$year<1862,])
west.educpc.n.placebo <- ncol(educ.pc.x.west.imp[!colnames(educ.pc.x.west.imp) %in% c("year")])

west.educpc.x <- educ.pc.x.west.imp[!colnames(educ.pc.x.west.imp) %in% c("year")]
west.educpc.y <- educ.pc.y.west[!colnames(educ.pc.y.west) %in% c("year")]

# import predictions

west.educpc.encoder.decoder.pred.treated <- read_csv(paste0(results.directory, "encoder-decoder/west-educpc/treated-gans/weights.110-0.586.hdf5-west-educpc-test.csv"), col_names = FALSE)
west.educpc.encoder.decoder.pred.control <- read_csv(paste0(results.directory, "encoder-decoder/west-educpc/control/weights.90-5.872.hdf5-west-educpc-test.csv"), col_names = FALSE)

# Actual versus predicted
west.educpc.encoder.decoder <- data.frame(
  "y.pred" = rbind(matrix(NA, west.educpc.n.pre, west.educpc.n.placebo+1), as.matrix(cbind(west.educpc.encoder.decoder.pred.treated, west.educpc.encoder.decoder.pred.control))),
  "y.true" = cbind(west.educpc.y, west.educpc.x),
  "year" =  educ.pc.y.west$year
)

# Post-period MSE and MAPE (all controls)

west.educpc.control.forecast <- as.matrix(west.educpc.encoder.decoder.pred.control)
west.educpc.control.true <- as.matrix(west.educpc.x[(west.educpc.n.pre+1):nrow(west.educpc.x),])

west.educpc.encoder.decoder.mse <- error(forecast=west.educpc.control.forecast, true=west.educpc.control.true, method = "mse") # post-intervention MSE
west.educpc.encoder.decoder.mse

west.educpc.encoder.decoder.preds <- rbind(matrix(NA, west.educpc.n.pre, west.educpc.n.placebo+1), as.matrix(cbind(west.educpc.encoder.decoder.pred.treated, west.educpc.encoder.decoder.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

west.educpc.treat.forecast <-  as.matrix(west.educpc.encoder.decoder.pred.treated)

west.educpc.treat.true <- as.matrix(west.educpc.y[1][(west.educpc.n.pre+1):nrow(west.educpc.y),])

west.educpc.t.stat <- rowMeans(west.educpc.treat.true-west.educpc.treat.forecast) # real t stat

# P-values for both treated and placebo treated

west.educpc.p.values.treated <- PermutationTest(west.educpc.control.forecast, west.educpc.control.true, west.educpc.t.stat, west.educpc.n.placebo, np=10000)

west.educpc.p.values.control <- sapply(1:west.educpc.n.placebo, function(c){
  west.educpc.t.stat.control <- rowMeans(as.matrix(west.educpc.control.true[,c])-as.matrix(west.educpc.control.forecast[,c]))
  PermutationTest(west.educpc.control.forecast[,-c], west.educpc.control.true[,-c], west.educpc.t.stat.control, west.educpc.n.placebo-1, np=10000)
})

encoder.decoder.west.educpc.fpr <- sum(west.educpc.p.values.control <=0.05)/length(west.educpc.p.values.control) #FPR
encoder.decoder.west.educpc.fpr
sum(p.adjust(west.educpc.p.values.control, "bonferroni") <=0.05)/length(west.educpc.p.values.control) # adjusted

# CIs for treated

west.educpc.CI.treated <- PermutationCI(west.educpc.control.forecast, west.educpc.control.true, west.educpc.t.stat, west.educpc.n.placebo, c.range=c(-9,3), np=10000, l=100)

# Plot pointwise impacts

# Pointwise impacts
west.educpc.encoder.decoder.control <- data.frame(
  "pointwise.control" = west.educpc.x[(west.educpc.n.pre+1):nrow(west.educpc.x),]-west.educpc.control.forecast,
  "year" =  sort(educ.pc.x.west.imp$year)[sort(educ.pc.x.west.imp$year)>=1862] # x year isn't sorted
)

west.educpc.encoder.decoder.treat <- data.frame(
  "pointwise.treat" = west.educpc.y[(west.educpc.n.pre+1):nrow(west.educpc.y),]-west.educpc.treat.forecast, 
  "year" =  educ.pc.y.west$year[educ.pc.y.west$year>=1862]
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

west.educpc.encoder.decoder.control.long <- melt(west.educpc.encoder.decoder.control, id="year")  # convert to long format
west.educpc.encoder.decoder.control.long$group <- "Control"

west.educpc.encoder.decoder.treat.long <- melt(west.educpc.encoder.decoder.treat, id="year")  # convert to long format
west.educpc.encoder.decoder.treat.long$group <- "Treated"

west.educpc.encoder.decoder.long <- rbind(west.educpc.encoder.decoder.treat.long, west.educpc.encoder.decoder.control.long)

west.educpc.encoder.decoder.long$ymin <- NA
west.educpc.encoder.decoder.long$ymax <- NA

west.educpc.encoder.decoder.long$ymin[west.educpc.encoder.decoder.long$group=="Treated"] <- west.educpc.CI.treated[,1]
west.educpc.encoder.decoder.long$ymax[west.educpc.encoder.decoder.long$group=="Treated"] <- west.educpc.CI.treated[,2]

encoder.decoder.plot.west.educpc <- ggplot(data=west.educpc.encoder.decoder.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log per-capita state government education spending (1982$)") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  ggtitle("Encoder-decoder treatment effects: Education spending in West") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-effects-west-educpc.png"), encoder.decoder.plot.west.educpc, width=11, height=8.5)

# Plot p-values

west.educpc.encoder.decoder.control <- data.frame(
  "p.values.control" = west.educpc.p.values.control,
  "year" =  educ.pc.y.west$year[educ.pc.y.west$year>=1862]
)

west.educpc.encoder.decoder.treat <- data.frame(
  "p.values.treat" = west.educpc.p.values.treated,
  "year" =  educ.pc.y.west$year[educ.pc.y.west$year>=1862]
)

west.educpc.encoder.decoder.control.long <- melt(west.educpc.encoder.decoder.control, id="year")  # convert to long format
west.educpc.encoder.decoder.control.long$group <- "Control"

west.educpc.encoder.decoder.treat.long <- melt(west.educpc.encoder.decoder.treat, id="year")  # convert to long format
west.educpc.encoder.decoder.treat.long$group <- "Treated"

west.educpc.encoder.decoder.long <- rbind(west.educpc.encoder.decoder.treat.long, west.educpc.encoder.decoder.control.long)

encoder.decoder.plot.pvalues.west.educpc <- ggplot(data=west.educpc.encoder.decoder.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  ggtitle("Encoder-decoder p-values: Education spending in West") +
  theme.blank + theme(legend.position = c(0.8,0.8)) + guides(colour=FALSE) 

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-pvalues-west-educpc.png"), encoder.decoder.plot.pvalues.west.educpc, width=11, height=8.5)

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

west.educpc.encoder.decoder.plot <- ggplot(data=west.educpc.encoder.decoder, aes(x=year)) +
  geom_line(aes(y=west.educpc.y[[1]], colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=west.educpc.encoder.decoder.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita state government education spending (1982$)") + xlab("") +
  geom_vline(xintercept=1862, linetype=2) + 
  ggtitle(paste0("Encoder-decoder actual vs. counterfactual outcome: Education spending in West")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-west-educpc.png"), west.educpc.encoder.decoder.plot, width=11, height=8.5)