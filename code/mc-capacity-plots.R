# Plot time-series and causal impacts

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)

source(paste0(code.directory,"TsPlot.R"))

capacity.outcomes <- readRDS(paste0(data.directory,"capacity-outcomes.rds"))

PlotMCCapacity <- function(x,y.title,limits,t0=which(colnames(capacity.outcomes[["rev.pc"]]$M)=="1869")){
  ## Create time series data
  
  boot <- readRDS(paste0(results.directory, "mc/", gsub('\\.', '-', x), "-boot.rds"))
  
  observed <- capacity.outcomes[[x]]$M
  
  mc_est <- readRDS(paste0(results.directory, "mc/mc_est.rds"))
  
  predicted <- mc_est[[x]]$Mhat
  
  pointwise <- mc_est[[x]]$impact # boot$t0
  
  pointwise.se <- matrix(apply(boot$t, 2, sd), nrow=dim(pointwise)[1], ncol=dim(pointwise)[2], byrow=FALSE)

  m <- ncol(observed)
  n <- t0
  
  cumulative <- sapply((n+1):m, function(t){
    (1/(t-n))*rowSums(pointwise[,n:t], na.rm=TRUE)
  })
  
  cumulative <- cbind(matrix(0, ncol=ncol(pointwise)-ncol(cumulative), nrow = nrow(pointwise)), cumulative)
  
  cumulative.se <- sapply((n+1):m, function(t){
    (1/(t-n))*rowSums(pointwise.se[,n:t])
  })
  
  cumulative.se <- cbind(matrix(0, ncol=ncol(pointwise.se)-ncol(cumulative.se), nrow = nrow(pointwise.se)), cumulative.se)
  
  ## Plot time series 
  
  treat.status <- matrix(rownames(pointwise), nrow=nrow(pointwise), ncol=1)
  treat.status[rownames(pointwise) %in% c(southern.pub,western.pub)] <- "PLS"
  treat.status[rownames(pointwise) %in% state.land.states] <- "SLS"
  treat.status <- matrix(treat.status, dimnames=list(NULL, "status"))
  
  observed.mean <-  aggregate(observed, list(treat.status), mean)[-1]
  predicted.mean <-  aggregate(predicted, list(treat.status), mean)[-1]
  pointwise.mean <- aggregate(pointwise, list(treat.status), mean, na.rm=TRUE)[-1]
  pointwise.se.mean <- aggregate(pointwise.se, list(treat.status), mean)[-1]
  cumulative.mean <- aggregate(cumulative, list(treat.status), mean, na.rm=TRUE)[-1]
  cumulative.se.mean <- aggregate(cumulative.se, list(treat.status), mean)[-1]
  
  # ts.means <- cbind(t(observed.mean), t(predicted.mean), t(pointwise.mean), t(cumulative.mean))
  # colnames(ts.means) <- c("observed.pls","observed.sls","predicted.pls","predicted.sls","pointwise.pls","pointwise.sls","cumulative.pls","cumulative.sls")
  ts.means <- cbind(t(observed.mean), t(predicted.mean), t(pointwise.mean))
  colnames(ts.means) <- c("observed.pls","observed.sls","predicted.pls","predicted.sls","pointwise.pls","pointwise.sls")
  ts.means <- cbind(ts.means, "year"=as.numeric(rownames(ts.means)))
  ts.means.m <- melt(data.frame(ts.means), id.var=c("year"))
  
  # ts.se.means <- cbind(t(pointwise.se.mean), t(cumulative.se.mean))
  # colnames(ts.se.means) <- c("pointwise.pls","pointwise.sls","cumulative.pls","cumulative.sls")
  ts.se.means <- cbind(t(pointwise.se.mean))
  colnames(ts.se.means) <- c("pointwise.pls","pointwise.sls")
  ts.se.means <- cbind(ts.se.means, "year"=as.numeric(rownames(ts.means)))
  ts.se.means.m <- melt(data.frame(ts.se.means), id.var=c("year"))
  
  ts.means.m <- merge(ts.means.m, ts.se.means.m, by=c("year","variable"), all.x=TRUE) # bind std. error
  colnames(ts.means.m) <- c("year", "variable", "value", "se")
  
  ts.means.m <- ts.means.m %>%
    mutate(upper = value + (se),
           lower = value - (se))

  # # Adjust year for plot
 # ts.means.m$year <- as.Date(as.yearmon(ts.means.m$year) + 11/12, frac = 1) # end of year
  ts.means.m$year <- as.Date(as.yearmon(ts.means.m$year)) 
  
  ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")
  
  # Labels
  
  ts.means.m$series <- NA
  ts.means.m$series[grep("observed.", ts.means.m$variable)] <- "Time-series"
  ts.means.m$series[grep("predicted.", ts.means.m$variable)] <- "Time-series"
  ts.means.m$series[grep("pointwise.", ts.means.m$variable)] <- "Per-period impact"
#  ts.means.m$series[grep("cumulative.", ts.means.m$variable)] <- "Cumulative impact"
  
  #ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series", "Per-period impact", "Cumulative impact")) # reverse order
  ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series", "Per-period impact")) # reverse order
  
  ts.plot <- TsPlot(df=ts.means.m,y.title,limits=limits)
  
  return(ts.plot)
}

mc.rev.pc <- PlotMCCapacity(x='rev.pc',y.title="Log per-capita state government revenue (1982$)",limits=c(as.POSIXct("1783-01-01 01:00:00"), as.POSIXct("1982-01-01 01:00:00")))
mc.exp.pc <- PlotMCCapacity(x='exp.pc',y.title="Log per-capita state government expenditure (1982$)", limits=c(as.POSIXct("1783-01-01 01:00:00"), as.POSIXct("1982-01-01 01:00:00")))
mc.educ.pc <- PlotMCCapacity(x='educ.pc',y.title="Log per-capita state government education spending (1942$)", limits=c(as.POSIXct("1783-01-01 01:00:00"), as.POSIXct("1942-01-01 01:00:00")))

ggsave(paste0(results.directory,"plots/mc-rev-pc.png"), mc.rev.pc, width=11, height=8.5)
ggsave(paste0(results.directory,"plots/mc-exp-pc.png"), mc.exp.pc, width=11, height=8.5)
ggsave(paste0(results.directory,"plots/mc-educ-pc.png"), mc.educ.pc, width=11, height=8.5)