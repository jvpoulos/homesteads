# Plot time-series and causal impacts

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)
require(ggplot2)

source(paste0(code.directory,"TsPlot.R"))

PlotMCCapacity <- function(x,y.title,limits,breaks,t0=which(colnames(capacity.outcomes[["rev.pc"]]$M)=="1869"),imp){
  ## Create time series data
  
  boot <- readRDS(paste0(results.directory, "mc/", gsub('\\.', '-', x), "-boot-",imp,".rds"))
  
  observed <- capacity.outcomes[[x]]$M
  
  mc_est <- readRDS(paste0(results.directory, "mc/mc_est_",imp,".rds"))
  
  predicted <- mc_est[[x]]$Mhat
  
  pointwise <- mc_est[[x]]$impact # boot$t0
  
  mean(pointwise[rownames(pointwise)%in%pub.states,][,t0:ncol(pointwise)]) # get mean post-period impact on treated
  
  pointwise.se <- matrix(apply(boot$t, 2, sd), nrow=dim(pointwise)[1], ncol=dim(pointwise)[2], byrow=FALSE, dimnames = dimnames(pointwise))

  mean(pointwise.se[rownames(pointwise.se)%in%pub.states,][,t0:ncol(pointwise.se)])
  
  m <- ncol(observed)
  n <- t0
  
  ## Plot time series 
  
  treat.status <- matrix(rownames(pointwise), nrow=nrow(pointwise), ncol=1)
  treat.status[rownames(pointwise) %in% c(pub.states)] <- "PLS"
  treat.status[rownames(pointwise) %in% state.land.states] <- "SLS"
  treat.status <- matrix(treat.status, dimnames=list(NULL, "status"))
  
  observed.mean <-  aggregate(observed, list(treat.status), mean)[-1]
  predicted.mean <-  aggregate(predicted, list(treat.status), mean)[-1]
  pointwise.mean <- aggregate(pointwise, list(treat.status), mean, na.rm=TRUE)[-1]
  pointwise.se.mean <- aggregate(pointwise.se, list(treat.status), mean)[-1]

  ts.means <- cbind(t(observed.mean), t(predicted.mean), t(pointwise.mean))
  colnames(ts.means) <- c("observed.pls","observed.sls","predicted.pls","predicted.sls","pointwise.pls","pointwise.sls")
  ts.means <- cbind(ts.means, "year"=as.numeric(rownames(ts.means)))
  ts.means.m <- melt(data.frame(ts.means), id.var=c("year"))
  
  ts.se.means <- cbind(t(pointwise.se.mean))
  colnames(ts.se.means) <- c("pointwise.pls","pointwise.sls")
  ts.se.means <- cbind(ts.se.means, "year"=as.numeric(rownames(ts.means)))
  ts.se.means.m <- melt(data.frame(ts.se.means), id.var=c("year"))
  
  ts.means.m <- merge(ts.means.m, ts.se.means.m, by=c("year","variable"), all.x=TRUE) # bind std. error
  colnames(ts.means.m) <- c("year", "variable", "value", "se")
  
  ts.means.m <- ts.means.m %>%
    mutate(upper = value + 1.96*se,
           lower = value - 1.96*se)

  # # Adjust year for plot
  ts.means.m$year <- as.Date(as.yearmon(ts.means.m$year)) 
  
  ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")
  
  # Labels
  
  ts.means.m$series <- NA
  ts.means.m$series[grep("observed.", ts.means.m$variable)] <- "Time-series"
  ts.means.m$series[grep("predicted.", ts.means.m$variable)] <- "Time-series"
  ts.means.m$series[grep("pointwise.", ts.means.m$variable)] <- "Per-period impact"

  ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series", "Per-period impact")) # reverse order
  
  ts.plot <- TsPlot(df=ts.means.m,y.title=y.title,limits=limits, breaks=breaks)
  
  return(ts.plot)
}

for(imp in c("mice")){ # c("mice","rf","ma","locf","linear")

  capacity.outcomes <- readRDS(paste0(data.directory,"capacity-outcomes-",imp,".rds"))
  
  mc.rev.pc <- PlotMCCapacity(x='rev.pc',y.title="Log per-capita state government revenue (1982$)\n",limits=c(as.POSIXct("1809-01-01 01:00:00"), as.POSIXct("1982-01-01 01:00:00")), breaks=seq(as.POSIXct("1809-1-31 00:00:00",tz="UTC"),
                                                                                                                                                                                                as.POSIXct("1982-1-31 00:00:00",tz="UTC"), "20 years"),imp=imp) 
  mc.exp.pc <- PlotMCCapacity(x='exp.pc',y.title="Log per-capita state government expenditure (1982$)\n",
                              limits=c(as.POSIXct("1809-01-01 01:00:00"), as.POSIXct("1982-01-01 01:00:00")), breaks=seq(as.POSIXct("1809-1-31 00:00:00",tz="UTC"),
                                                                                                                         as.POSIXct("1982-1-31 00:00:00",tz="UTC"), "20 years"),imp=imp)
  
  ggsave(paste0(results.directory,"plots/mc-rev-",imp,".png"), mc.rev.pc, scale=1.25)
  ggsave(paste0(results.directory,"plots/mc-exp-",imp,".png"), mc.exp.pc, scale=1.25)
}