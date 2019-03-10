# CIs

iid_block_ci <- readRDS(paste0(results.directory, "mc/iid_block_ci.rds"))
iid_block_ci <- lapply(1:3, function(x) lapply(iid_block_ci[[x]],rowRanges, na.rm=TRUE))

moving_block_ci <- readRDS(paste0(results.directory, "mc/moving_block_ci.rds"))
moving_block_ci <- lapply(1:3, function(x) lapply(moving_block_ci[[x]],rowRanges, na.rm=TRUE))

## Plot all estimates

# Data for plot

plot.data.did <- data.frame(region=rep(c("West","South"),each=3),
                            variable= rep(c("Education","Revenue", "Expenditures"),times=2),
                            y = c(educ.pc.all.did.delta,
                                  rev.pc.all.did.delta,
                                  exp.pc.all.did.delta,
                                  educ.pc.all.south.did.delta,
                                  rev.pc.all.south.did.delta,
                                  exp.pc.all.south.did.delta),
                            y.lo = c(educ.pc.all.did.CI[1],
                                     rev.pc.all.did.CI[1],
                                     exp.pc.all.did.CI[1],
                                     educ.pc.all.south.did.CI[1],
                                     rev.pc.all.south.did.CI[1],
                                     exp.pc.all.south.did.CI[1]),
                            y.hi = c(educ.pc.all.did.CI[2],
                                     rev.pc.all.did.CI[2],
                                     exp.pc.all.did.CI[2],
                                     educ.pc.all.south.did.CI[2],
                                     rev.pc.all.south.did.CI[2],
                                     exp.pc.all.south.did.CI[2]))


# Plot forest plots

source(paste0(code.directory,"ForestPlot2.R"))

plot.data.did$variable <- as.factor(plot.data.did$variable)
did.state <- ForestPlot2(plot.data.did,ylab="Estimated effect of log per-capita cumulative homesteads",xlab="",title="",leglab="Region") + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/did-state.png"), did.state, width=11, height=8.5)