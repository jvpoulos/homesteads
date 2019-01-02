# Plot time-series and causal impacts

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)

source(paste0(code.directory,"TsPlot.R"))

## Create time series data

rev.pc.boot <- readRDS(rev.pc.boot, "rev-pc-boot.rds")
exp.pc.boot <- readRDS(exp.pc.boot, "exp-pc-boot.rds")

pointwise <- rev.pc.boot$t0
pointwise.se <- matrix(apply(rev.pc.boot$t, 2, sd), nrow=49, ncol=159, byrow=TRUE)

cumulative <-rowCumsums(pointwise)
cumulative.se <-rowCumsums(pointwise.se)

## Plot time series 

treat.status <- matrix(rownames(pointwise), nrow=nrow(pointwise), ncol=1)
treat.status[rownames(pointwise) %in% western.pub] <- "WPL"
treat.status[rownames(pointwise) %in% southern.pub] <- "SPL"
treat.status[rownames(pointwise) %in% setdiff(state.land.states, southern.state)] <- "WSL"
treat.status[rownames(pointwise) %in% southern.state] <- "SSL"
treat.status <- matrix(treat.status, dimnames=list(NULL, "status"))

pointwise.mean <- aggregate(pointwise, list(treat.status), mean)
cumulative.mean <- aggregate(cumulative, list(treat.status), mean)

ts.means.m <- melt(as.data.frame(ts.means)[!colnames(ts.means) %in% c("se")], id.var=c("year"))

# # Adjust year for plot
ts.means.m$year <- as.Date(as.yearmon(ts.means.m$year) + 11/12, frac = 1) # end of year

ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")

# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="1" | ts.means.m$variable=="pred"] <- "Winner margin time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.impact"] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.impact"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Winner margin time-series", "Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed votediff","Predicted votediff", 
                                 "Pointwise votediff", "Cumulative votediff")

# se

se <- ts.dat  %>%
  mutate(pred.min = pred - se*1.96,
         pred.max = pred + se*1.96,
         pointwise.min = 1-pred.max,
         pointwise.max = 1-pred.min,
         cumulative.min = cumsum(pointwise.min),
         cumulative.max = cumsum(pointwise.max))

se <- se[with(se, order(year)), ] # sort by year

# for (i in 1:nrow(se)){
#   se$cumulative.min[i] <- rollmean(se$pointwise.min,i, align='right')
# }
# for (i in 1:nrow(se)){
#   se$cumulative.max[i] <- rollmean(se$pointwise.max,i, align='right')
# }

pred.vars <- c("pred", "se", "pred.min", "pred.max", "pointwise.min", "pointwise.max", "cumulative.min", "cumulative.max")
ts.means.m <- cbind(ts.means.m, se[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

ts.plot <- TsPlotElections(ts.means.m)
ggsave(paste0(results.directory,"plots/mc-capacity-impact.png"), ts.plot, width=11, height=8.5)

# Calculate avg. pointwise impact during post-period: >= 2005

# 2005
ts.means$pointwise.impact[ts.means$year%in%c(2005)]

se$pointwise.min[se$year%in%c(2005)]
se$pointwise.max[se$year%in%c(2005)]