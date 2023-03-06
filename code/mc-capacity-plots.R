######################################################################
# Plot Time-series and causal impacts, and estimated latent trends #
######################################################################

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)
require(ggplot2)
library(latex2exp)
library(wesanderson)

source(paste0(code.directory,"TsPlot.R"))

PlotMCCapacity <- function(estimates, boot, x,y.title,limits,breaks,imp,alpha=0.05){
  
  ## load observed data
  
  capacity.outcomes <- readRDS(paste0(data.directory,"capacity-outcomes-",imp,".rds"))
  t0 <- which(colnames(capacity.outcomes[[x]]$M)=="1869")
  
  ## Create time series data

  observed <- capacity.outcomes[[x]]$M
  
  predicted <- estimates$Mhat
  
  pointwise <- boot$boot_tau$t0
  
  pointwise.se <- matrix(apply(boot$boot_tau$t, 2, sd), nrow=dim(pointwise)[1], ncol=dim(pointwise)[2], byrow=FALSE, dimnames = dimnames(pointwise))

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
    mutate(upper = value + qnorm(1-(alpha/2))*se,
           lower = value - qnorm(1-(alpha/2))*se)
  
  # Adjust year for plot
  ts.means.m$year <- as.Date(as.yearmon(ts.means.m$year)) 
  
  ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")
  
  # Labels
  
  ts.means.m$series <- NA
  ts.means.m$series[grep("observed.", ts.means.m$variable)] <- "Time-series\n"
  ts.means.m$series[grep("predicted.", ts.means.m$variable)] <- "Time-series\n"
  ts.means.m$series[grep("pointwise.", ts.means.m$variable)] <- "Per-period ATT\n" #"Per-period impact"

  ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series\n", "Per-period ATT\n")) # reverse order
  
  ts.plot <- TsPlot(df=ts.means.m,y.title=y.title,limits=limits, breaks=breaks,hline=ts.means.m$hline)
  
  return(ts.plot)
}

# Load data and estimates

mc_capacity_results_data_rev.pc_estimator_mc_weights <- readRDS("./outputs/20230222/mc_capacity_results_data_rev.pc_estimator_mc_weights.rds")
mc_capacity_results_data_exp.pc_estimator_mc_weights <- readRDS("./outputs/20230212/mc_capacity_results_data_exp.pc_estimator_mc_weights.rds")

mc_capacity_point_estimates_data_rev.pc_estimator_mc_weights <- readRDS("./outputs/20230226/mc_capacity_point_estimates_data_rev.pc_estimator_mc_weights.rds")
mc_capacity_point_estimates_data_exp.pc_estimator_mc_weights <- readRDS("./outputs/20230226/mc_capacity_point_estimates_data_exp.pc_estimator_mc_weights.rds")

imp <- "mice-pmm"
imp.loc <- which(c("mice-cart","mice-pmm","mtsdi")==imp)

# Plot estimates
mc.rev.pc <- PlotMCCapacity(estimates = mc_capacity_point_estimates_data_rev.pc_estimator_mc_weights, boot = mc_capacity_results_data_rev.pc_estimator_mc_weights[,imp.loc], x='rev.pc',y.title="Log per-capita state government revenue (1982$)\n",limits=c(as.POSIXct("1809-01-01 01:00:00"), as.POSIXct("1982-01-01 01:00:00")), breaks=seq(as.POSIXct("1809-1-31 00:00:00",tz="UTC"),
                                                                                                                                                                                              as.POSIXct("1982-1-31 00:00:00",tz="UTC"), "20 years"),imp=imp) 
mc.exp.pc <- PlotMCCapacity(estimates = mc_capacity_point_estimates_data_exp.pc_estimator_mc_weights, boot = mc_capacity_results_data_exp.pc_estimator_mc_weights[,imp.loc], x='exp.pc',y.title="Log per-capita state government expenditure (1982$)\n",
                            limits=c(as.POSIXct("1809-01-01 01:00:00"), as.POSIXct("1982-01-01 01:00:00")), breaks=seq(as.POSIXct("1809-1-31 00:00:00",tz="UTC"),
                                                                                                                       as.POSIXct("1982-1-31 00:00:00",tz="UTC"), "20 years"),imp=imp)

ggsave(paste0(results.directory,"plots/mc-rev-",imp,".png"), mc.rev.pc, scale=1.25)
ggsave(paste0(results.directory,"plots/mc-exp-",imp,".png"), mc.exp.pc, scale=1.25)