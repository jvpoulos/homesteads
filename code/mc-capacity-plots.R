# Plot time-series and causal impacts

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)
require(caTools)

source(paste0(code.directory,"TsPlot.R"))

## Create time series data

rev.pc.boot <- readRDS(paste0(results.directory, "mc/rev-pc-boot.rds"))
exp.pc.boot <- readRDS(paste0(results.directory, "mc/exp-pc-boot.rds"))

pointwise <- rev.pc.boot$t0
pointwise.se <- matrix(apply(rev.pc.boot$t, 2, sd), nrow=49, ncol=159, byrow=TRUE)

cumulative <-t(rollmeanr(t(pointwise), 3, fill=0))
cumulative.se <-t(rollmeanr(t(pointwise.se), 3, fill=0))

## Plot time series 

treat.status <- matrix(rownames(pointwise), nrow=nrow(pointwise), ncol=1)
treat.status[rownames(pointwise) %in% western.pub] <- 1 #"WPL"
treat.status[rownames(pointwise) %in% southern.pub] <- 2 # "SPL"
treat.status[rownames(pointwise) %in% setdiff(state.land.states, southern.state)] <- 3 # "WSL"
treat.status[rownames(pointwise) %in% southern.state] <- 4 # "SSL"
treat.status <- matrix(treat.status, dimnames=list(NULL, "status"))

observed.mean <-  aggregate(dfList$rev.pc$M, list(treat.status), mean)[-1]
predicted.mean <-  aggregate(mc.est$rev.pc$Mhat, list(treat.status), mean)[-1]
pointwise.mean <- aggregate(pointwise, list(treat.status), mean)[-1]
pointwise.se.mean <- aggregate(pointwise.se, list(treat.status), mean)[-1]
cumulative.mean <- aggregate(cumulative, list(treat.status), mean)[-1]
cumulative.se.mean <- aggregate(cumulative.se, list(treat.status), mean)[-1]

ts.means <- cbind(t(observed.mean), t(predicted.mean), t(pointwise.mean), t(cumulative.mean))
colnames(ts.means) <- c("observed.wpl","observed.spl","observed.wsl","observed.ssl","predicted.wpl","predicted.spl","predicted.wsl","predicted.ssl","pointwise.wpl","pointwise.spl","pointwise.wsl","pointwise.ssl","cumulative.wpl","cumulative.spl","cumulative.wsl","cumulative.ssl")
ts.means <- cbind(ts.means, "year"=as.numeric(rownames(ts.means)))

ts.se.means <- cbind(t(pointwise.se.mean), t(cumulative.se.mean))
colnames(ts.se.means) <- c("pointwise.se.wpl","pointwise.se.spl","pointwise.se.wsl","pointwise.se.ssl","cumulative.se.wpl","cumulative.se.spl","cumulative.se.wsl","cumulative.se.ssl")
  
ts.means.m <- melt(data.frame(cbind(ts.means, ts.se.means)), id.var=c("year"))

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

# levels(ts.means.m$variable) <- c("Observed time-series","Predicted time-series", 
#                                  "Pointwise impact", "Cumulative impact")

# # se
# 
# se <- ts.dat  %>%
#   mutate(pred.min = pred - se*1.96,
#          pred.max = pred + se*1.96,
#          pointwise.min = 1-pred.max,
#          pointwise.max = 1-pred.min,
#          cumulative.min = cumsum(pointwise.min),
#          cumulative.max = cumsum(pointwise.max))
# 
# se <- se[with(se, order(year)), ] # sort by year
# 
# pred.vars <- c("pred", "se", "pred.min", "pred.max", "pointwise.min", "pointwise.max", "cumulative.min", "cumulative.max")
# ts.means.m <- cbind(ts.means.m, se[pred.vars])
# ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

ts.plot <- TsPlot(ts.means.m)
ggsave(paste0(results.directory,"plots/mc-capacity-impact.png"), ts.plot, width=11, height=8.5)

# Calculate avg. pointwise impact during post-period: >= 2005

# 2005
ts.means$pointwise.impact[ts.means$year%in%c(2005)]

se$pointwise.min[se$year%in%c(2005)]
se$pointwise.max[se$year%in%c(2005)]