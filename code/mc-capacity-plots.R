# Plot time-series and causal impacts

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)

source(paste0(code.directory,"TsPlot.R"))


mc_est <- readRDS(paste0(results.directory, "mc/mc_est.rds"))

iid <- readRDS(paste0(results.directory, "mc/iid.rds"))

iid_block <- readRDS(paste0(results.directory, "mc/iid_block.rds"))

moving_block <- readRDS(paste0(results.directory, "mc/moving_block.rds"))

PlotMCCapacity <- function(x,permtype){
  ## Create time series data
  
  pointwise.ci <- t(readRDS(paste0(results.directory, "mc/", permtype,".rds"))[[x]])
  
  observed <- capacity.outcomes[[x]]$M
  
  predicted <- mc.est[[x]]$Mhat
  
  pointwise <- mc.est[[x]]$impact 

  m <- ncol(observed)
  n <- t0
  
  cumulative <- sapply((n+1):m, function(t){
    (1/(t-n))*rowSums(pointwise[,n:t], na.rm=TRUE)
  })
  
  cumulative <- cbind(matrix(0, ncol=ncol(pointwise)-ncol(cumulative), nrow = nrow(pointwise)), cumulative)
  
  cumulative.ci <- t(sapply(1:nrow(pointwise.ci), function(i){
    t <- ((n+1):m)[i]
    if(i==1){
      pointwise.ci[1,]
    }else{
      (1/(t-n))*colSums(pointwise.ci[1:i,])   
    }
  }))
  
  cumulative.ci <- cbind(matrix(0, ncol=ncol(cumulative)-ncol(cumulative.ci), nrow = nrow(pointwise.ci)), cumulative.ci)
  
  pointwise.ci <- cbind(matrix(0, ncol=ncol(pointwise)-ncol(pointwise.ci), nrow = nrow(pointwise.ci)), pointwise.ci)
  
  ## Plot time series 
  
  treat.status <- matrix(rownames(pointwise), nrow=nrow(pointwise), ncol=1)
  treat.status[rownames(pointwise) %in% c(southern.pub,western.pub)] <- "PLS"
  treat.status[rownames(pointwise) %in% state.land.states] <- "SLS"
  treat.status <- matrix(treat.status, dimnames=list(NULL, "status"))
  
  observed.mean <-  aggregate(observed, list(treat.status), mean)[-1]
  predicted.mean <-  aggregate(predicted, list(treat.status), mean)[-1]
  pointwise.mean <- aggregate(pointwise, list(treat.status), mean, na.rm=TRUE)[-1]
  cumulative.mean <- aggregate(cumulative, list(treat.status), mean, na.rm=TRUE)[-1]
  
  ts.means <- cbind(t(observed.mean), t(predicted.mean), t(pointwise.mean), t(cumulative.mean))
  colnames(ts.means) <- c("observed.pls","observed.sls","predicted.pls","predicted.sls","pointwise.pls","pointwise.sls","cumulative.pls","cumulative.sls")
  ts.means <- cbind(ts.means, "year"=as.numeric(rownames(ts.means)))
  ts.means.m <- melt(data.frame(ts.means), id.var=c("year"))
  
  ts.ci.means <- cbind(t(pointwise.ci), t(cumulative.ci))
  colnames(ts.ci.means) <- c("pointwise.lo","pointwise.hi","cumulative.lo","cumulative.hi")
  ts.ci.means <- cbind(ts.ci.means, "year"=as.numeric(rownames(ts.means)))
  
  ts.means.m <- merge(ts.means.m, ts.ci.means, by=c("year"), all.x=TRUE) # bind cis

  # # Adjust year for plot
  ts.means.m$year <- as.Date(as.yearmon(ts.means.m$year) + 11/12, frac = 1) # end of year
  
  ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")
  
  # Labels
  
  ts.means.m$series <- NA
  ts.means.m$series[grep("observed.", ts.means.m$variable)] <- "Time-series"
  ts.means.m$series[grep("predicted.", ts.means.m$variable)] <- "Time-series"
  ts.means.m$series[grep("pointwise.", ts.means.m$variable)] <- "Per-period impact"
  ts.means.m$series[grep("cumulative.", ts.means.m$variable)] <- "Cumulative impact"
  
  ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series", "Per-period impact", "Cumulative impact")) # reverse order
  
  ts.plot <- TsPlot(df=ts.means.m,y.title)
  
  return(ts.plot)
}

mc.rev.pc.iid <- PlotMCCapacity(x='rev.pc',y.title="Log per-capita state government revenue (1982$)")

mc.plots <- mclapply(list("rev.pc", "exp.pc", "educ.pc"), PlotMCCapacity, mc.cores=8)

ggsave(paste0(results.directory,"plots/mc-rev-pc.png"), mc.plots[[1]], width=11, height=8.5)
ggsave(paste0(results.directory,"plots/mc-exp-pc.png"), mc.plots[[2]], width=11, height=8.5)
ggsave(paste0(results.directory,"plots/mc-educ-pc.png"), mc.plots[[3]], width=11, height=8.5)