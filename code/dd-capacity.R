###################################
# DD Estimation for comparison    #
###################################
library(dplyr)
library(boot)

source(paste0(code.directory,"RunDid.R"))

## All public land states

funds.did<- homestead.funds.long[homestead.funds.long$state_code %in% pub.states,]

# Create var for when treatment started

funds.did$time <- NA
funds.did$time <- 0
funds.did$time[(funds.did$year >= 1862)] <- 1

funds.did$treat <- 0
funds.did$treat <- funds.did$homesteads.pc # treat: homesteads.pc (ln)

funds.did$did <- NA
funds.did$did <- funds.did$treat* funds.did$time 

# DD Estimates

f1 <- formula(educ.pc ~ factor(state_code) + 
                log(ns.pop) +  log(adultm) +
                time + did)

f2 <- formula(educ.pc ~ factor(state_code) + 
                log(ns.pop) +  log(adultm) +
                log(farms) + farmsize + farmval + # farmsize/farmval in logs
                time + did)

# All years
educ.pc.all.did <- boot(data=funds.did,
                        statistic=RunDiD,
                        f1=f1,
                        R=1000,
                        strata=as.factor(funds.did$state_code), # stratify by state
                        parallel="multicore", ncpus = cores)

educ.pc.all.did.delta <- educ.pc.all.did$t0
educ.pc.all.did.delta

educ.pc.all.did.CI <- boot.ci(educ.pc.all.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
educ.pc.all.did.CI

# Sanity check

summary(lm(f1, data=funds.did))

# By year
test.years <- seq(1870,1950,10)

# aland.south.did.t <- foreach(x= test.years, .combine=cbind, .multicombine = TRUE) %dopar% {
#   df <- did.south[did.south$year ==x,]
#   RunBoot = function(df){
#     boot.result <- boot(data=df,
#                         statistic=RunDiD,
#                         f1=f2, # state dids
#                         R=1000,
#                         strata=as.factor(df$state), # stratify by state
#                         parallel="multicore", ncpus = cores)
#     return(list("delta"= boot.result$t0,
#                 "CI.low"=boot.ci(boot.result, conf=0.95, index=1, type="norm")$normal[2],
#                 "CI.high"=boot.ci(boot.result, conf=0.95, index=1, type="norm")$normal[3],
#                 "N" = nrow(df)))
#   }
#   RunBoot(df)
# }
# 
# saveRDS(aland.south.did.t, paste0(data.directory,"aland-south-did-t.rds"))

# Western public land state counties 

did.west <- census.ts.wide[census.ts.wide$state.abb %in% c(western.pub, state.land.states),] # compare western pub. (treated) vs. state land states (control)

did.west$treat <- NA
did.west$treat <- ifelse(did.west$state.abb %in% western.pub,1,0)

did.west$did <- NA
did.west$did <- did.west$treat* did.west$time

# All years

aland.west.did <- boot(data=did.west,
                       statistic=RunDiD,
                       f1=f1,
                       R=1000,
                       strata=as.factor(did.west$state), # stratify by state
                       parallel="multicore", ncpus = cores)

aland.west.did.delta <- aland.west.did$t0
aland.west.did.delta

aland.west.did.CI <- boot.ci(aland.west.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.west.did.CI

# By year

# aland.west.did.t <- foreach(x= test.years, .combine=cbind, .multicombine = TRUE) %dopar% {
#   df <- did.west[did.west$year ==x,]
#   RunBoot = function(df){
#     boot.result <- boot(data=df,
#                         statistic=RunDiD,
#                         f1=f2,
#                         R=1000,
#                         strata=as.factor(df$state), # stratify by state
#                         parallel="multicore", ncpus = cores)
#     return(list("delta"= boot.result$t0,
#                 "CI.low"=boot.ci(boot.result, conf=0.95, index=1, type="norm")$normal[2],
#                 "CI.high"=boot.ci(boot.result, conf=0.95, index=1, type="norm")$normal[3],
#                 "N" = nrow(df)))
#   }
#   RunBoot(df)
# }
# 
# saveRDS(aland.west.did.t, paste0(data.directory,"aland-west-did-t.rds"))

# All public land states

did.all <- census.ts.wide # compare pub land states vs. state land states

did.all$treat <- NA
did.all$treat <- ifelse(did.all$state.abb %in% pub.states,1,0)

did.all$did <- NA
did.all$did <- did.all$treat* did.all$time

# All years

aland.all.did <- boot(data=did.all,
                      statistic=RunDiD,
                      f1=f1,
                      R=100, # reduce iterations
                      strata=as.factor(did.all$state), # stratify by state
                      parallel="multicore", ncpus = cores)

aland.all.did.delta <- aland.all.did$t0
aland.all.did.delta

aland.all.did.CI <- boot.ci(aland.all.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.all.did.CI

# # By year
# 
# aland.all.did.t <- foreach(x= test.years, .combine=cbind, .multicombine = TRUE) %dopar% {
#   df <- did.all[did.all$year ==x,]
#   RunBoot = function(df){
#     boot.result <- boot(data=df,
#                         statistic=RunDiD,
#                         f1=f2,
#                         R=1000,
#                         strata=as.factor(df$state), # stratify by state
#                         parallel="multicore", ncpus = cores)
#     return(list("delta"= boot.result$t0,
#                 "CI.low"=boot.ci(boot.result, conf=0.95, index=1, type="norm")$normal[2],
#                 "CI.high"=boot.ci(boot.result, conf=0.95, index=1, type="norm")$normal[3],
#                 "N" = nrow(df)))
#   }
#   RunBoot(df)
# }

# # Data for plot 
# 
# plot.data.year.did <- data.frame(variable= rep(c("South","West","All"),each=length(test.years)),
#                                 y = c(unlist(aland.south.did.t['delta',]),
#                                       unlist(aland.west.did.t['delta',]),
#                                       unlist(aland.all.did.t['delta',])),
#                                 N = c(unlist(aland.south.did.t['N',]),
#                                       unlist(aland.west.did.t['N',]),
#                                       unlist(aland.all.did.t['N',])),
#                                 y.lo = c(unlist(aland.south.did.t['CI.low',]),
#                                          unlist(aland.west.did.t['CI.low',]),
#                                          unlist(aland.all.did.t['CI.low',])),
#                                 y.hi = c(unlist(aland.south.did.t['CI.high',]),
#                                          unlist(aland.west.did.t['CI.high',]),
#                                          unlist(aland.all.did.t['CI.high',])))
# 
# plot.data.year.did$x <- rep(test.years,3)
# 
# # Plot forest plots
# 
# ForestPlot2 <- function(d, xlab, ylab, title="", leglab, ylim=NULL){
#   p <- ggplot(d, aes(x=x, y = y, ymin=y.lo, ymax=y.hi,colour=variable)) + 
#     geom_pointrange(size=1, alpha=0.6) + 
#     #  coord_flip() +
#     geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0), colour="black", lty=2) +
#     #  scale_y_continuous(labels = scales::percent) +
#     labs(colour = leglab) +
#     ggtitle(title) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     coord_cartesian(ylim=ylim) +
#     ylab(ylab) +
#     xlab(xlab) #switch because of the coord_flip() above
#   return(p)
# }
# 
# plot.data.year.did$x <- as.factor(plot.data.year.did$x)
# summary.plot.year <- ForestPlot2(plot.data.year.did,ylab="Estimated efdidct of per-capita homesteads",xlab="",title="DD",leglab="Treated")
# 
# ggsave(paste0(results.directory,"did-homestead.png"), summary.plot.year, width=11, height=8.5)