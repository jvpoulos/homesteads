# Plot time-series and causal impacts

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)
require(caTools)

source(paste0(code.directory,"TsPlot.R"))

PlotMCCapacity <- function(x){
  ## Create time series data
  
  boot <- readRDS(paste0(results.directory, "mc/", gsub('\\.', '-', x), "-boot.rds"))

  observed <- dfList[[x]]$M
  
  predicted <- mc.est[[x]]$Mhat
  
  pointwise <- boot$t0
  pointwise.se <- matrix(apply(boot$t, 2, sd), nrow=49, ncol=159, byrow=FALSE)
  
  m <- which(colnames(observed)=="1982")
  n <- which(colnames(observed)=="1869")-1
  
  cumulative <- sapply((n+1):m, function(t){
    (1/(t-n))*rowSums(pointwise[,n:t])
  })
  
  cumulative <- cbind(matrix(0, ncol=ncol(pointwise)-ncol(cumulative), nrow = nrow(pointwise)), cumulative)
  
  cumulative.se <- sapply((n+1):m, function(t){
    (1/(t-n))*rowSums(pointwise.se[,n:t])
  })
  
  cumulative.se <- cbind(matrix(0, ncol=ncol(pointwise.se)-ncol(cumulative.se), nrow = nrow(pointwise.se)), cumulative.se)
  
  ## Plot time series 
  
  treat.status <- matrix(rownames(pointwise), nrow=nrow(pointwise), ncol=1)
  treat.status[rownames(pointwise) %in% western.pub] <- 1 #"WPL"
  treat.status[rownames(pointwise) %in% southern.pub] <- 2 # "SPL"
  treat.status[rownames(pointwise) %in% setdiff(state.land.states, southern.state)] <- 3 # "WSL"
  treat.status[rownames(pointwise) %in% southern.state] <- 4 # "SSL"
  treat.status <- matrix(treat.status, dimnames=list(NULL, "status"))
  
  observed.mean <-  aggregate(observed, list(treat.status), mean)[-1]
  predicted.mean <-  aggregate(predicted, list(treat.status), mean)[-1]
  pointwise.mean <- aggregate(pointwise, list(treat.status), mean)[-1]
  pointwise.se.mean <- aggregate(pointwise.se, list(treat.status), mean)[-1]
  cumulative.mean <- aggregate(cumulative, list(treat.status), mean)[-1]
  cumulative.se.mean <- aggregate(cumulative.se, list(treat.status), mean)[-1]
  
  ts.means <- cbind(t(observed.mean), t(predicted.mean), t(pointwise.mean), t(cumulative.mean))
  colnames(ts.means) <- c("observed.wpl","observed.spl","observed.wsl","observed.ssl","predicted.wpl","predicted.spl","predicted.wsl","predicted.ssl","pointwise.wpl","pointwise.spl","pointwise.wsl","pointwise.ssl","cumulative.wpl","cumulative.spl","cumulative.wsl","cumulative.ssl")
  ts.means <- cbind(ts.means, "year"=as.numeric(rownames(ts.means)))
  ts.means.m <- melt(data.frame(ts.means), id.var=c("year"))
  
  ts.se.means <- cbind(t(pointwise.se.mean), t(cumulative.se.mean))
  colnames(ts.se.means) <- c("pointwise.wpl","pointwise.spl","pointwise.wsl","pointwise.ssl","cumulative.wpl","cumulative.spl","cumulative.wsl","cumulative.ssl")
  ts.se.means <- cbind(ts.se.means, "year"=as.numeric(rownames(ts.means)))
  ts.se.means.m <- melt(data.frame(ts.se.means), id.var=c("year"))
  
  ts.means.m <- merge(ts.means.m, ts.se.means.m, by=c("year","variable"), all.x=TRUE) # bind std. error
  colnames(ts.means.m) <- c("year", "variable", "value", "se")
  
  ts.means.m <- ts.means.m %>%
    mutate(upper = value + se*1.96,
           lower = value - se*1.96)
  
  # # Adjust year for plot
  ts.means.m$year <- as.Date(as.yearmon(ts.means.m$year) + 11/12, frac = 1) # end of year
  
  ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")
  
  # Labels
  
  ts.means.m$series <- NA
  ts.means.m$series[grep("observed.", ts.means.m$variable)] <- "Time-series"
  ts.means.m$series[grep("predicted.", ts.means.m$variable)] <- "Time-series"
  ts.means.m$series[grep("pointwise.", ts.means.m$variable)] <- "Pointwise impact"
  ts.means.m$series[grep("cumulative.", ts.means.m$variable)] <- "Cumulative impact"
  
  ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series", "Pointwise impact", "Cumulative impact")) # reverse order
  
  ts.plot <- TsPlot(ts.means.m)
  
  return(ts.plot)
}

mc.plots <- mclapply(list("rev.pc", "exp.pc", "educ.pc"), PlotMCCapacity, mc.cores=8)

ggsave(paste0(results.directory,"plots/mc-rev-pc.png"), mc.plots[[1]], width=11, height=8.5)
ggsave(paste0(results.directory,"plots/mc-exp-pc.png"), mc.plots[[2]], width=11, height=8.5)
ggsave(paste0(results.directory,"plots/mc-educ-pc.png"), mc.plots[[3]], width=11, height=8.5)