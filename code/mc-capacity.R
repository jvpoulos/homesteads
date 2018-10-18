###################################
# MC Estimation (state-level measures)   #
###################################

# install.packages("devtools")
# install.packages("latex2exp")
# library(devtools) 
# install_github("susanathey/MCPanel")

library(MCPanel)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

educ.pc.cv <- mcnnm_cv(educ.pc.M, educ.pc.mask, num_folds=10)
educ.pc.cv$min_RMSE

exp.pc.cv <- mcnnm_cv(exp.pc.M, exp.pc.mask, num_folds=10)
exp.pc.cv$min_RMSE

rev.pc.cv <- mcnnm_cv(rev.pc.M, rev.pc.mask, num_folds=10)
rev.pc.cv$min_RMSE

# Actual versus predicted

# educpc.mc <- data.frame( # for plot
#   "y.pred.south" = rbind(matrix(NA, educpc.n.pre, educpc.n.placebo+1), as.matrix(cbind(educpc.mc.pred.treated, educpc.mc.pred.control))),
#   "y.true.south" = rowMeans(rev.pc[c("rev.pc.MS", "rev.pc.FL", "rev.pc.LA", "rev.pc.AL", "rev.pc.AR")], na.rm=TRUE),
#   "year" =  educ.pc$year
# )

# Calculate real treated pooled intervention effect

educpc.treat.forecast <-  as.matrix(educpc.mc.pred.treated)

educpc.treat.true <- as.matrix(educpc.y[1][(educpc.n.pre+1):nrow(educpc.y),])

educpc.t.stat <- rowMeans(educpc.treat.true-educpc.treat.forecast) # real t stat

# P-values for both treated and placebo treated

educpc.p.values.treated <- PermutationTest(educpc.control.forecast, educpc.control.true, educpc.t.stat, educpc.n.placebo, np=10000)

educpc.p.values.control <- sapply(1:educpc.n.placebo, function(c){
  educpc.t.stat.control <- rowMeans(as.matrix(educpc.control.true[,c])-as.matrix(educpc.control.forecast[,c]))
  PermutationTest(educpc.control.forecast[,-c], educpc.control.true[,-c], educpc.t.stat.control, educpc.n.placebo-1, np=10000)
})

mc.educpc.fpr <- sum(educpc.p.values.control <=0.05)/length(educpc.p.values.control) #FPR
mc.educpc.fpr
sum(p.adjust(educpc.p.values.control, "bonferroni") <=0.05)/length(educpc.p.values.control) # adjusted

# CIs for treated

educpc.CI.treated <- PermutationCI(educpc.control.forecast, educpc.control.true, educpc.t.stat, educpc.n.placebo, c.range=c(-10,10), np=10000, l=1000)

# Plot pointwise impacts

# Pointwise impacts
educpc.mc.control <- data.frame(
  "pointwise.control" = educpc.x[(educpc.n.pre+1):nrow(educpc.x),]-educpc.control.forecast,
  "year" =  educ.pc.x.imp$year[educ.pc.x.imp$year>=1862]
)

educpc.mc.treat <- data.frame(
  "pointwise.treat" = educpc.y[(educpc.n.pre+1):nrow(educpc.y),]-educpc.treat.forecast, 
  "year" =  educ.pc.y$year[educ.pc.y$year>=1862]
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

educpc.mc.control.long <- melt(educpc.mc.control, id="year")  # convert to long format
educpc.mc.control.long$group <- "Control"

educpc.mc.treat.long <- melt(educpc.mc.treat, id="year")  # convert to long format
educpc.mc.treat.long$group <- "Treated"

educpc.mc.long <- rbind(educpc.mc.treat.long, educpc.mc.control.long)

educpc.mc.long$ymin <- NA
educpc.mc.long$ymax <- NA

educpc.mc.long$ymin[educpc.mc.long$group=="Treated"] <- educpc.CI.treated[,1]
educpc.mc.long$ymax[educpc.mc.long$group=="Treated"] <- educpc.CI.treated[,2]

mc.plot.educpc <- ggplot(data=educpc.mc.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log per-capita state government education spending (1982$)") + 
  xlab("Year") +
  scale_x_continuous(breaks=seq(1860, 1980, 20)) +
  scale_alpha_manual(values=c(0.1, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  coord_cartesian(ylim=c(-8, 8)) +
  # ggtitle("mc treatment effects: Education spending in West") +
  theme.blank + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/mc-plot-effects-west-educpc.png"), mc.plot.educpc, width=11, height=8.5)

# mean(educpc.treat.true)-mean(as.matrix(educpc.y[1][1:(educpc.n.pre),])) # 1st diff (treated post-pre)
# 
# mean(educpc.treat.forecast)-mean(as.matrix(educpc.y[1][1:(educpc.n.pre),])) # 2nd diff (counterfactual post-pre)

mean(educpc.mc.long$value[educpc.mc.long$variable=="X1"])
mean(educpc.mc.long$ymin[educpc.mc.long$variable=="X1"])
mean(educpc.mc.long$ymax[educpc.mc.long$variable=="X1"])

# Plot p-values

educpc.mc.control <- data.frame(
  "p.values.control" = educpc.p.values.control,
  "year" =  educ.pc.y$year[educ.pc.y$year>=1862]
)

educpc.mc.treat <- data.frame(
  "p.values.treat" = educpc.p.values.treated,
  "year" =  educ.pc.y$year[educ.pc.y$year>=1862]
)

educpc.mc.control.long <- melt(educpc.mc.control, id="year")  # convert to long format
educpc.mc.control.long$group <- "Control"

educpc.mc.treat.long <- melt(educpc.mc.treat, id="year")  # convert to long format
educpc.mc.treat.long$group <- "Treated"

educpc.mc.long <- rbind(educpc.mc.treat.long, educpc.mc.control.long)

mc.plot.pvalues.educpc <- ggplot(data=educpc.mc.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  # ggtitle("mc p-values: Education spending in West") +
  theme.blank + theme(legend.position = c(0.8,0.8)) + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/mc-plot-pvalues-west-educpc.png"), mc.plot.pvalues.educpc, width=11, height=8.5)

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

educpc.mc.plot <- ggplot(data=educpc.mc, aes(x=year)) +
  geom_line(aes(y=educpc.y[[1]], colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=educpc.mc.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita state government education spending (1982$)") + xlab("") +
  geom_vline(xintercept=1862, linetype=2) + 
  # ggtitle(paste0("mc actual vs. counterfactual outcome: Education spending in West")) +
  theme.blank + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/mc-plot-west-educpc.png"), educpc.mc.plot, width=11, height=8.5)
