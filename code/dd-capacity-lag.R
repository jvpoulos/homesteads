###################################
# DD Estimation (state-level measures)   #
###################################
library(dplyr)
library(boot)

source(paste0(code.directory,"RunDid.R"))

## Western public land states

funds.did<- homestead.funds.long[homestead.funds.long$state_code %in% western.pub,]

# Create var for when treatment started

funds.did$time <- NA
funds.did$time <- 0
funds.did$time[(funds.did$year >= 1862)] <- 1

funds.did$treat <- 0
funds.did$treat <- funds.did$homesteads.pc.lag # treat: homesteads.pc.lag (ln)

funds.did$did <- NA
funds.did$did <- funds.did$treat* funds.did$time 

# DD Estimates

# educ.pc

# All years
educ.pc.all.did <- boot(data=funds.did,
                        statistic=RunDiD,
                        f1=educ.f1,
                        R=1000,
                        strata=as.factor(funds.did$state_code), # stratify by state
                        parallel="multicore", ncpus = cores)

educ.pc.all.did.delta <- educ.pc.all.did$t0
educ.pc.all.did.delta

educ.pc.all.did.CI <- boot.ci(educ.pc.all.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
educ.pc.all.did.CI

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

# Sanity check

#summary(lm(educ.f1, data=funds.did.south))

# rev.pc

# All years
rev.pc.all.did <- boot(data=funds.did,
                        statistic=RunDiD,
                        f1=rev.pc.f1,
                        R=1000,
                        strata=as.factor(funds.did$state_code), # stratify by state
                        parallel="multicore", ncpus = cores)

rev.pc.all.did.delta <- rev.pc.all.did$t0
rev.pc.all.did.delta

rev.pc.all.did.CI <- boot.ci(rev.pc.all.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rev.pc.all.did.CI

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
# All years
exp.pc.all.did <- boot(data=funds.did,
                       statistic=RunDiD,
                       f1=exp.pc.f1,
                       R=1000,
                       strata=as.factor(funds.did$state_code), # stratify by state
                       parallel="multicore", ncpus = cores)

exp.pc.all.did.delta <- exp.pc.all.did$t0
exp.pc.all.did.delta

exp.pc.all.did.CI <- boot.ci(exp.pc.all.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
exp.pc.all.did.CI

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

## Southern public land states

funds.did.south<- homestead.funds.long[homestead.funds.long$state_code %in% southern.pub,]

# Create var for when treatment started

funds.did.south$time <- NA
funds.did.south$time <- 0
funds.did.south$time[(funds.did.south$year >= 1866)] <- 1

funds.did.south$treat <- 0
funds.did.south$treat <- funds.did.south$homesteads.pc.lag # treat: homesteads.pc.lag (ln)

funds.did.south$did <- NA
funds.did.south$did <- funds.did.south$treat* funds.did.south$time 

# DD Estimates

# educ.pc

# All years
educ.pc.all.south.did <- boot(data=funds.did.south,
                        statistic=RunDiD,
                        f1=educ.f1,
                        R=1000,
                        strata=as.factor(funds.did.south$state_code), # stratify by state
                        parallel="multicore", ncpus = cores)

educ.pc.all.south.did.delta <- educ.pc.all.south.did$t0
educ.pc.all.south.did.delta

educ.pc.all.south.did.CI <- boot.ci(educ.pc.all.south.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
educ.pc.all.south.did.CI

# All years
educ.pc.all.south.robust.did <- boot(data=funds.did.south,
                               statistic=RunDiD,
                               f1=educ.f2,
                               R=1000,
                               strata=as.factor(funds.did.south$state_code), # stratify by state
                               parallel="multicore", ncpus = cores)

educ.pc.all.south.robust.did.delta <- educ.pc.all.south.robust.did$t0
educ.pc.all.south.robust.did.delta

educ.pc.all.south.robust.did.CI <- boot.ci(educ.pc.all.south.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
educ.pc.all.south.robust.did.CI

# Sanity check

#summary(lm(educ.f1, data=funds.did.south))

# rev.pc

# All years
rev.pc.all.south.did <- boot(data=funds.did.south,
                       statistic=RunDiD,
                       f1=rev.pc.f1,
                       R=1000,
                       strata=as.factor(funds.did.south$state_code), # stratify by state
                       parallel="multicore", ncpus = cores)

rev.pc.all.south.did.delta <- rev.pc.all.south.did$t0
rev.pc.all.south.did.delta

rev.pc.all.south.did.CI <- boot.ci(rev.pc.all.south.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rev.pc.all.south.did.CI

# All years
rev.pc.all.south.robust.did <- boot(data=funds.did.south,
                              statistic=RunDiD,
                              f1=rev.pc.f2,
                              R=1000,
                              strata=as.factor(funds.did.south$state_code), # stratify by state
                              parallel="multicore", ncpus = cores)

rev.pc.all.south.robust.did.delta <- rev.pc.all.south.robust.did$t0
rev.pc.all.south.robust.did.delta

rev.pc.all.south.robust.did.CI <- boot.ci(rev.pc.all.south.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rev.pc.all.south.robust.did.CI

# exp.pc

# All years
exp.pc.all.south.did <- boot(data=funds.did.south,
                       statistic=RunDiD,
                       f1=exp.pc.f1,
                       R=1000,
                       strata=as.factor(funds.did.south$state_code), # stratify by state
                       parallel="multicore", ncpus = cores)

exp.pc.all.south.did.delta <- exp.pc.all.south.did$t0
exp.pc.all.south.did.delta

exp.pc.all.south.did.CI <- boot.ci(exp.pc.all.south.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
exp.pc.all.south.did.CI

# All years
exp.pc.all.south.robust.did <- boot(data=funds.did.south,
                              statistic=RunDiD,
                              f1=exp.pc.f2,
                              R=1000,
                              strata=as.factor(funds.did.south$state_code), # stratify by state
                              parallel="multicore", ncpus = cores)

exp.pc.all.south.robust.did.delta <- exp.pc.all.south.robust.did$t0
exp.pc.all.south.robust.did.delta

exp.pc.all.south.robust.did.CI <- boot.ci(exp.pc.all.south.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
exp.pc.all.south.robust.did.CI

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

ForestPlot2 <- function(d, xlab, ylab, title="", leglab, ylim=NULL){
  p <- ggplot(d, aes(x=variable, y = y, ymin=y.lo, ymax=y.hi,colour=region)) +
    geom_pointrange(size=1, alpha=0.6) +
    #  coord_flip() +
    geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0), colour="black", lty=2) +
      scale_y_continuous(labels = scales::percent) +
    labs(colour = leglab) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(ylim=ylim) +
    ylab(ylab) +
    xlab(xlab) #switch because of the coord_flip() above
  return(p)
}

plot.data.did$variable <- as.factor(plot.data.did$variable)
summary.plot <- ForestPlot2(plot.data.did,ylab="Estimated effect of lagged log per-capita homesteads",xlab="",title="DD estimates on state-level measures, with lagged treatment",leglab="Region")

ggsave(paste0(results.directory,"plots/did-state-lag.png"), summary.plot, width=11, height=8.5)

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
summary.plot <- ForestPlot2(plot.data.did,ylab="Estimated effect of lagged log per-capita homesteads",xlab="",title="DD estimates on state-level measures, with lagged treatment",leglab="Region")

ggsave(paste0(results.directory,"plots/did-state-robust-lag.png"), summary.plot, width=11, height=8.5)
