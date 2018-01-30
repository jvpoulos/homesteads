###################################
#  Fixed effects model            #
###################################

library(boot)
library(tidyr)
library(zoo)
library(readxl)
library(parallel)
library(doParallel)
library(foreach)

source(paste0(homestead.code.directory,"RunFE.R"))

f1 <- formula(aland.gini ~ homesteads.pc.lag + id) 

# Southern public land state counties 

fe.south <- census.ts.wide[census.ts.wide$state.abb %in% c(southern.pub),]

# All years
aland.south.fe <- boot(data=fe.south,
                          statistic=RunFE,
                          f1=f1,
                          R=1000,
                          strata=as.factor(fe.south$county), # stratify by county
                          parallel="multicore", ncpus = cores)

aland.south.fe.delta <- aland.south.fe$t0
aland.south.fe.delta

aland.south.fe.CI <- boot.ci(aland.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.south.fe.CI

# # By year
# test.years <- seq(1870,1950,10)
# 
# aland.south.fe.t <- foreach(x= test.years[3:9], .combine=cbind, .multicombine = TRUE) %dopar% {
#   df <- fe.south[fe.south$year ==x,]
#   RunBoot = function(df){
#     boot.result <- boot(data=df,
#                         statistic=RunFE,
#                         f1=f1, 
#                         R=1000,
#                         strata=as.factor(df$county), 
#                         parallel="multicore", ncpus = cores)
#     return(list("delta"= boot.result$t0,
#                 "CI.low"=boot.ci(boot.result, conf=0.95, index=1, type="norm")$normal[2],
#                 "CI.high"=boot.ci(boot.result, conf=0.95, index=1, type="norm")$normal[3],
#            "N" = nrow(df)))
#   }
#   RunBoot(df)
# }

# Western public land state counties

fe.west <- census.ts.wide[census.ts.wide$state.abb %in% c(western.pub),]

# All years

aland.west.fe <- boot(data=fe.west,
                       statistic=RunFE,
                       f1=f1,
                       R=1000,
                       strata=as.factor(fe.west$county),
                       parallel="multicore", ncpus = cores)

aland.west.fe.delta <- aland.west.fe$t0
aland.west.fe.delta

aland.west.fe.CI <- boot.ci(aland.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.west.fe.CI

# # By year
# 
# aland.west.fe.t <- foreach(x= test.years[3:9], .combine=cbind, .multicombine = TRUE) %dopar% {
#   df <- fe.west[fe.west$year ==x,]
#   RunBoot = function(df){
#     boot.result <- boot(data=df,
#                         statistic=RunFE,
#                         f1=f1,
#                         R=1000,
#                         strata=as.factor(df$county),
#                         parallel="multicore", ncpus = cores)
#     return(list("delta"= boot.result$t0,
#                 "CI.low"=boot.ci(boot.result, conf=0.95, index=1, type="norm")$normal[2],
#                 "CI.high"=boot.ci(boot.result, conf=0.95, index=1, type="norm")$normal[3],
#                 "N" = nrow(df)))
#   }
#   RunBoot(df)
# }

# All public land states

fe.all <- census.ts.wide[census.ts.wide$state.abb %in% c(pub.states),]

# All years

aland.all.fe <- boot(data=fe.all,
                      statistic=RunFE,
                      f1=f1,
                      R=1000,
                      strata=as.factor(fe.all$county),
                      parallel="multicore", ncpus = cores)

aland.all.fe.delta <- aland.all.fe$t0
aland.all.fe.delta

aland.all.fe.CI <- boot.ci(aland.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.all.fe.CI

# # By year
# 
# aland.all.fe.t <- foreach(x= test.years[3:9], .combine=cbind, .multicombine = TRUE) %dopar% {
#   df <- fe.all[fe.all$year ==x,]
#   RunBoot = function(df){
#     boot.result <- boot(data=df,
#                         statistic=RunFE,
#                         f1=f1,
#                         R=1000,
#                         strata=as.factor(df$county), 
#                         parallel="multicore", ncpus = cores)
#     return(list("delta"= boot.result$t0,
#                 "CI.low"=boot.ci(boot.result, conf=0.95, index=1, type="norm")$normal[2],
#                 "CI.high"=boot.ci(boot.result, conf=0.95, index=1, type="norm")$normal[3],
#                 "N" = nrow(df)))
#   }
#   RunBoot(df)
# }
# 
# # Data for plot 
# 
# plot.data.year.fe <- data.frame(variable= rep(c("South","West","All"),each=length(test.years[3:9])),
#                               y = c(unlist(aland.south.fe.t['delta',]),
#                                     unlist(aland.west.fe.t['delta',]),
#                                     unlist(aland.all.fe.t['delta',])),
#                               N = c(unlist(aland.south.fe.t['N',]),
#                                     unlist(aland.west.fe.t['N',]),
#                                     unlist(aland.all.fe.t['N',])),
#                               y.lo = c(unlist(aland.south.fe.t['CI.low',]),
#                                        unlist(aland.west.fe.t['CI.low',]),
#                                        unlist(aland.all.fe.t['CI.low',])),
#                               y.hi = c(unlist(aland.south.fe.t['CI.high',]),
#                                        unlist(aland.west.fe.t['CI.high',]),
#                                        unlist(aland.all.fe.t['CI.high',])))
#  
# plot.data.year.fe$x <- rep(test.years[3:9],3)
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
# plot.data.year.fe$x <- as.factor(plot.data.year.fe$x)
# summary.plot.year <- ForestPlot2(plot.data.year.fe,ylab="Estimated effect of lagged per-capita homesteads",xlab="",title="Lagged fixed effects",leglab="Treated")
# 
# ggsave(paste0(results.directory,"fe-homestead.png"), summary.plot.year, width=11, height=8.5)