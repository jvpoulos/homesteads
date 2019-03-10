###################################
# DD Estimation (state-level measures)   #
###################################
library(data.table)
library(dplyr)
library(boot)

source(paste0(code.directory,"RunDiD.R"))

# Load data
capacity.outcomes <- readRDS(paste0(data.directory,"capacity-outcomes.rds"))
capacity.covars <- readRDS(paste0(data.directory,"capacity-covariates.rds"))
capacity.covars <- data.frame(capacity.covars)
capacity.covars$state <- rownames(capacity.covars)

capacity.outcomes.M <- list("rev.pc"=capacity.outcomes[["rev.pc"]]$M,"exp.pc"=capacity.outcomes[["exp.pc"]]$M,"educ.pc"=capacity.outcomes[["educ.pc"]]$M)

capacity.outcomes.panel <- lapply(capacity.outcomes.M, melt)

capacity.outcomes.panel <- lapply(capacity.outcomes.panel, merge, y=capacity.covars, by.x="Var1",by.y="state", all.x=TRUE)

# Log per-capita total number of patents issued under the HSA 
#funds.did<- homestead.funds.long

# Create var for when treatment started

funds.did$time <- NA
funds.did$time <- 0
funds.did$time[(funds.did$year >= 1862)] <- 1

funds.did$treat <- 0
funds.did$treat <- funds.did$homesteads.pc 

funds.did$did <- NA
funds.did$did <- funds.did$treat* funds.did$time 

# DD Estimates

# educ.pc
educ.f1 <- formula(educ.pc ~ factor(state_code) + 
                time + did)

educ.f2 <- formula(educ.pc ~ factor(state_code) + 
                farmval + 
                time + did)

# All years
educ.pc.all.did <- boot(data=funds.did,
                        statistic=RunDiD,
                        f1=educ.f1,
                        R=1000,
                        strata=as.factor(funds.did$state_code), # stratify by state
                        parallel="multicore", ncpus = cores)

educ.pc.all.did.delta <- educ.pc.all.did$t0

educ.pc.all.did.CI <- boot.ci(educ.pc.all.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs

# All years
educ.pc.all.robust.did <- boot(data=funds.did,
                        statistic=RunDiD,
                        f1=educ.f2,
                        R=1000,
                        strata=as.factor(funds.did$state_code), # stratify by state
                        parallel="multicore", ncpus = cores)

educ.pc.all.robust.did.delta <- educ.pc.all.robust.did$t0
educ.pc.all.robust.did.delta

educ.pc.all.robust.did.CI <- boot.ci(educ.pc.all.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
educ.pc.all.robust.did.CI

# rev.pc
rev.pc.f1 <- formula(rev.pc ~ factor(state_code) + 
                     time + did)

rev.pc.f2 <- formula(rev.pc ~ factor(state_code) + 
                     farmval +
                     time + did)

# All years
rev.pc.all.did <- boot(data=funds.did,
                        statistic=RunDiD,
                        f1=rev.pc.f1,
                        R=1000,
                        strata=as.factor(funds.did$state_code), # stratify by state
                        parallel="multicore", ncpus = cores)

rev.pc.all.did.delta <- rev.pc.all.did$t0

rev.pc.all.did.CI <- boot.ci(rev.pc.all.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs

# All years
rev.pc.all.robust.did <- boot(data=funds.did,
                               statistic=RunDiD,
                               f1=rev.pc.f2,
                               R=1000,
                               strata=as.factor(funds.did$state_code), # stratify by state
                               parallel="multicore", ncpus = cores)

rev.pc.all.robust.did.delta <- rev.pc.all.robust.did$t0
rev.pc.all.robust.did.delta

rev.pc.all.robust.did.CI <- boot.ci(rev.pc.all.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rev.pc.all.robust.did.CI

# exp.pc
exp.pc.f1 <- formula(exp.pc ~ factor(state_code) + 
                       time + did)

exp.pc.f2 <- formula(exp.pc ~ factor(state_code) + 
                       farmval +
                       time + did)

# All years
exp.pc.all.did <- boot(data=funds.did,
                       statistic=RunDiD,
                       f1=exp.pc.f1,
                       R=1000,
                       strata=as.factor(funds.did$state_code), # stratify by state
                       parallel="multicore", ncpus = cores)

exp.pc.all.did.delta <- exp.pc.all.did$t0

exp.pc.all.did.CI <- boot.ci(exp.pc.all.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs

# All years
exp.pc.all.robust.did <- boot(data=funds.did,
                              statistic=RunDiD,
                              f1=exp.pc.f2,
                              R=1000,
                              strata=as.factor(funds.did$state_code), # stratify by state
                              parallel="multicore", ncpus = cores)

exp.pc.all.robust.did.delta <- exp.pc.all.robust.did$t0
exp.pc.all.robust.did.delta

exp.pc.all.robust.did.CI <- boot.ci(exp.pc.all.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
exp.pc.all.robust.did.CI

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

## Plot all estimates (robust)

# Data for plot

plot.data.did <- data.frame(region=rep(c("West","South"),each=3),
                            variable= rep(c("Education","Revenue", "Expenditures"),times=2),
                            y = c(educ.pc.all.robust.did.delta,
                                  rev.pc.all.robust.did.delta,
                                  exp.pc.all.robust.did.delta,
                                  educ.pc.all.south.robust.did.delta,
                                  rev.pc.all.south.robust.did.delta,
                                  exp.pc.all.south.robust.did.delta),
                            y.lo = c(educ.pc.all.robust.did.CI[1],
                                     rev.pc.all.robust.did.CI[1],
                                     exp.pc.all.robust.did.CI[1],
                                     educ.pc.all.south.robust.did.CI[1],
                                     rev.pc.all.south.robust.did.CI[1],
                                     exp.pc.all.south.robust.did.CI[1]),
                            y.hi = c(educ.pc.all.robust.did.CI[2],
                                     rev.pc.all.robust.did.CI[2],
                                     exp.pc.all.robust.did.CI[2],
                                     educ.pc.all.south.robust.did.CI[2],
                                     rev.pc.all.south.robust.did.CI[2],
                                     exp.pc.all.south.robust.did.CI[2]))


# Plot forest plots

plot.data.did$variable <- as.factor(plot.data.did$variable)
did.state.robust <- ForestPlot2(plot.data.did,ylab="Estimated effect of log per-capita cumulative homesteads",xlab="",title="",leglab="Region")  + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/did-state-robust.png"), did.state.robust, width=11, height=8.5)
