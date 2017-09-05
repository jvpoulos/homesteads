# Plot time-series and estimate causal impacts
# Uses train/test sets from capacity-state.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)

source(paste0(code.directory,"ts-plot-ed.R"))

analysis <- "analysis-12"

type <- "treated"

## Education data
setwd(paste0(results.directory, "predictions/","/edpc/",analysis,"/",type)) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

ed.pc.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                header=FALSE,
                                col.names="ed.pc.pred"))
ed.pc.test.mean <-rowMeans(ed.pc.test.pred)
ed.pc.test.sd <- matrixStats::rowSds(as.matrix(ed.pc.test.pred))

# Import val results

val.files <- list.files(pattern = "*val.csv")

ed.pc.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                        header=FALSE,
                                        col.names="ed.pc.pred"))
ed.pc.val.mean <-rowMeans(ed.pc.val.pred)
ed.pc.val.sd <- matrixStats::rowSds(as.matrix(ed.pc.val.pred))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

ed.pc.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                        header=FALSE,
                                        col.names="ed.pc.pred"))
ed.pc.train.mean <- rowMeans(as.matrix(ed.pc.train.pred))
ed.pc.train.sd <- matrixStats::rowSds(as.matrix(ed.pc.train.pred))


# Bind to splits
ed.pc.test <- cbind(ed.pc.y.test, 
                         "ed.pc.mean"= ed.pc.test.mean, 
                         "ed.pc.sd"= ed.pc.test.sd)
ed.pc.val <- cbind(ed.pc.y.val, 
                    "ed.pc.mean"= ed.pc.val.mean, 
                    "ed.pc.sd"= ed.pc.val.sd) [-11,] # dont' double count 1899
ed.pc.train <- cbind(ed.pc.y.train, 
                          "ed.pc.mean"=ed.pc.train.mean, 
                          "ed.pc.sd"=ed.pc.train.sd) 


## Create time series data
setwd(code.directory)

ts.dat <- rbind(rbind(ed.pc.train,ed.pc.test),ed.pc.val)

## Plot time series 

time.vars <- c("year","ed.pc.Treated","ed.pc.mean")

ts.means <- ts.dat[time.vars]  %>%
  mutate(pointwise.ed.pc = ed.pc.Treated-ed.pc.mean,
         cumulative.ed.pc = NA)

ts.means <- ts.means[with(ts.means, order(year)), ] # sort by year

for (i in 1:nrow(ts.means)){
  ts.means$cumulative.ed.pc[i] <- rollmean(ts.means$pointwise.ed.pc,i, align='right')
}

ts.means.m <- melt(as.data.frame(ts.means), id.var=c("year"))

# Adjust year for plot
ts.means.m$year <- as.yearmon(ts.means.m$year, "%Y-%m",tz="UTC") # convert year to year class

ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")
                         
# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="ed.pc.Treated" | ts.means.m$variable=="ed.pc.mean"] <- "Time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.ed.pc" ] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.ed.pc" ] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed ed.pc","Predicted ed.pc", 
                                 "Pointwise ed.pc", "Cumulative ed.pc")

# SDs

sds <- ts.dat  %>%
  mutate(pred.ed.pc.min = ed.pc.mean - ed.pc.sd*1.96,
         pred.ed.pc.max = ed.pc.mean + ed.pc.sd*1.96,
         pointwise.ed.pc.min = ed.pc.Treated-pred.ed.pc.min,
         pointwise.ed.pc.max = ed.pc.Treated-pred.ed.pc.max,
         cumulative.ed.pc.min = NA,
         cumulative.ed.pc.max = NA)

sds <- sds[with(sds, order(year)), ] # sort by year

for (i in 1:nrow(sds)){
  sds$cumulative.ed.pc.min[i] <- rollmean(sds$pointwise.ed.pc.min,i, align='right')
}
for (i in 1:nrow(sds)){
  sds$cumulative.ed.pc.max[i] <- rollmean(sds$pointwise.ed.pc.max,i, align='right')
}

pred.vars <- c("ed.pc.mean", "ed.pc.sd", "pred.ed.pc.min", "pred.ed.pc.max", "pointwise.ed.pc.min", "pointwise.ed.pc.max", "cumulative.ed.pc.min", "cumulative.ed.pc.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

if(analysis=="analysis-12"){
  ts.plot <- TsPlotEd(ts.means.m[ts.means.m$year >"1830-12-31 19:03:58",], analysis)
  ggsave(paste0(results.directory,"plots/ed-south-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-34"){
  ts.plot <- TsPlotEd(ts.means.m[ts.means.m$year >= "1853-12-31 19:03:58",], analysis)
  ggsave(paste0(results.directory,"plots/ed-public-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-12"){

# Calculate avg. pointwise impact during intervention/post-period: >= 1866 & <=1928

#ed.pc
edpc.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise ed.pc" & (ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])
edpc.mu

edpc.mu - (mean(sds$ed.pc.sd[(sds$year>=1866 & sds$year<=1914)])*1.96)
edpc.mu + (mean(sds$ed.pc.sd[(sds$year>=1866 & sds$year<=1914)])*1.96)

# Calculate cumulative impact during intervention/post-period: >= 1866 <=1928

#ed.pc
ed.pc.mu <- ts.means.m$value[ts.means.m$variable=="Cumulative ed.pc" & ts.means.m$year=="1914-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative ed.pc" & ts.means.m$year=="1866-12-31 19:03:58"]
ed.pc.mu

sds$cumulative.ed.pc.max[(sds$year==1914)] -sds$cumulative.ed.pc.max[(sds$year==1866)]
sds$cumulative.ed.pc.min[(sds$year==1914)] -sds$cumulative.ed.pc.min[(sds$year==1866)]
}

if(analysis=="analysis-34"){
  # Calculate avg. pointwise impact during intervention/post-period: >= 1889 & <= 1928
  
  #ed.pc
  ed.pc.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise ed.pc" & (ts.means.m$year>="1889-12-31 19:00:00") & (ts.means.m$year<="1916-12-31 19:00:00")])
  ed.pc.mu
  
  ed.pc.mu - (mean(sds$ed.pc.sd[(sds$year>=1889 & sds$year<=1917)])*1.96)
  ed.pc.mu + (mean(sds$ed.pc.sd[(sds$year>=1889 & sds$year<=1917)])*1.96)
  
  # Calculate cumulative impact during intervention/post-period: >= 1889 & <= 1928
  
  #ed.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative ed.pc" & ts.means.m$year=="1916-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative ed.pc" & ts.means.m$year=="1889-12-31 19:00:00"]
  
  sds$cumulative.ed.pc.max[(sds$year==1917)] -sds$cumulative.ed.pc.max[(sds$year==1889)]
  sds$cumulative.ed.pc.min[(sds$year==1917)] -sds$cumulative.ed.pc.min[(sds$year==1889)]
}