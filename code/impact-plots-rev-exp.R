# Plot time-series and estimate causal impacts
# Uses train/test sets from capacity-state.R

require(reshape2)
require(dplyr)
require(zoo)
require("matrixStats")

analysis <- "analysis-12"

type <- "treated"

source(paste0(code.directory,"ts-plot-rev-exp.R"))

## Revenues data
setwd(paste0(results.directory, "predictions/","/revpc/",analysis,"/",type)) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

rev.pc.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                header=FALSE,
                                col.names="rev.pc.pred"))
rev.pc.test.mean <-rowMeans(rev.pc.test.pred)
rev.pc.test.sd <- matrixStats::rowSds(as.matrix(rev.pc.test.pred))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

rev.pc.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                        header=FALSE,
                                        col.names="rev.pc.pred"))
rev.pc.train.mean <- rowMeans(as.matrix(rev.pc.train.pred))
rev.pc.train.sd <- matrixStats::rowSds(as.matrix(rev.pc.train.pred))

# Import validation fit

val.files <- list.files(pattern = "*val.csv")

rev.pc.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                          header=FALSE,
                                          col.names="rev.pc.pred"))
rev.pc.val.mean <- rowMeans(as.matrix(rev.pc.val.pred))
rev.pc.val.sd <- matrixStats::rowSds(as.matrix(rev.pc.val.pred))


# Bind to splits
rev.pc.test <- cbind(rev.pc.y.test, 
                         "rev.pc.mean"= rev.pc.test.mean, 
                         "rev.pc.sd"= rev.pc.test.sd) 

rev.pc.val <- cbind(rev.pc.y.val, 
                     "rev.pc.mean"= rev.pc.val.mean, 
                     "rev.pc.sd"= rev.pc.val.sd) 

rev.pc.train <- cbind(rev.pc.y.train, 
                          "rev.pc.mean"=rev.pc.train.mean, 
                          "rev.pc.sd"=rev.pc.train.sd) 

## Expenditures data 
setwd(paste0(results.directory, "predictions/","/exppc/",analysis,"/",type)) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

exp.pc.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                             header=FALSE,
                                             col.names="exp.pc.pred"))
exp.pc.test.mean <-rowMeans(exp.pc.test.pred)
exp.pc.test.sd <- matrixStats::rowSds(as.matrix(exp.pc.test.pred))

# Import val results

val.files <- list.files(pattern = "*val.csv")

exp.pc.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                         header=FALSE,
                                         col.names="exp.pc.pred"))
exp.pc.val.mean <-rowMeans(exp.pc.val.pred)
exp.pc.val.sd <- matrixStats::rowSds(as.matrix(exp.pc.val.pred))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

exp.pc.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                              header=FALSE,
                                              col.names="exp.pc.pred"))
exp.pc.train.mean <- rowMeans(as.matrix(exp.pc.train.pred))
exp.pc.train.sd <- matrixStats::rowSds(as.matrix(exp.pc.train.pred))


# Bind to splits
exp.pc.test <- cbind(exp.pc.y.test, 
                         "exp.pc.mean"= exp.pc.test.mean, 
                         "exp.pc.sd"= exp.pc.test.sd) 

exp.pc.val <- cbind(exp.pc.y.val, 
                     "exp.pc.mean"= exp.pc.val.mean, 
                     "exp.pc.sd"= exp.pc.val.sd) 

exp.pc.train <- cbind(exp.pc.y.train, 
                          "exp.pc.mean"=exp.pc.train.mean, 
                          "exp.pc.sd"=exp.pc.train.sd) 

## Create time series data
setwd(code.directory)

ts.dat <- merge(rbind(rbind(rev.pc.train,rev.pc.test),rev.pc.val), rbind(rbind(exp.pc.train,exp.pc.test),exp.pc.val), by="year")

## Plot time series 

time.vars <- c("year","rev.pc.Treated","rev.pc.mean","exp.pc.Treated", "exp.pc.mean")

ts.means <- ts.dat[time.vars]  %>%
  mutate(pointwise.rev.pc = rev.pc.Treated-rev.pc.mean,
         cumulative.rev.pc = rollmean(pointwise.rev.pc,2,fill=NA, align='right'),
         pointwise.exp.pc = exp.pc.Treated-exp.pc.mean,
         cumulative.exp.pc = rollmean(pointwise.exp.pc,2,fill=NA, align='right')) 

ts.means.m <- melt(as.data.frame(ts.means), id.var=c("year"))

# Adjust year for plot
ts.means.m$year <- as.yearmon(ts.means.m$year, "%Y-%m",tz="UTC") # convert year to year class

ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")
                         
# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="rev.pc.Treated" | ts.means.m$variable=="rev.pc.mean" | ts.means.m$variable=="exp.pc.Treated" | ts.means.m$variable=="exp.pc.mean"] <- "Time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.rev.pc" | ts.means.m$variable=="pointwise.exp.pc"] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.rev.pc" | ts.means.m$variable=="cumulative.exp.pc"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed rev.pc","Predicted rev.pc", "Observed exp.pc", "Predicted exp.pc",
                                 "Pointwise rev.pc", "Cumulative rev.pc", 
                                 "Pointwise exp.pc", "Cumulative exp.pc")

# SDs

sds <- ts.dat  %>%
  mutate(pred.rev.pc.min = rev.pc.mean - rev.pc.sd*1.96,
         pred.rev.pc.max = rev.pc.mean + rev.pc.sd*1.96,
         pointwise.rev.pc.min = rev.pc.Treated-pred.rev.pc.min,
         pointwise.rev.pc.max = rev.pc.Treated-pred.rev.pc.max,
         cumulative.rev.pc.min = rollmean(pointwise.rev.pc.min,2,fill=NA, align='right'),
         cumulative.rev.pc.max = rollmean(pointwise.rev.pc.max,2,fill=NA, align='right'),
         pred.exp.pc.min = exp.pc.mean - exp.pc.sd*1.96,
         pred.exp.pc.max = exp.pc.mean + exp.pc.sd*1.96,
         pointwise.exp.pc.min = exp.pc.Treated-pred.exp.pc.min,
         pointwise.exp.pc.max = exp.pc.Treated-pred.exp.pc.max,
         cumulative.exp.pc.min = rollmean(pointwise.exp.pc.min,2,fill=NA, align='right'),
         cumulative.exp.pc.max = rollmean(pointwise.exp.pc.max,2,fill=NA, align='right'))

pred.vars <- c("rev.pc.mean", "rev.pc.sd", "pred.rev.pc.min", "pred.rev.pc.max", "pointwise.rev.pc.min", "pointwise.rev.pc.max", "cumulative.rev.pc.min", "cumulative.rev.pc.max",
               "exp.pc.mean", "exp.pc.sd", "pred.exp.pc.min", "pred.exp.pc.max", "pointwise.exp.pc.min", "pointwise.exp.pc.max", "cumulative.exp.pc.min", "cumulative.exp.pc.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

if(analysis=="analysis-12"){
  ts.plot <- TsPlotRevExp(ts.means.m[ts.means.m$year >"1832-12-31 19:03:58",], analysis)
  ggsave(paste0(results.directory,"plots/rev-exp-south-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-34"){
  ts.plot <- TsPlotRevExp(ts.means.m[ts.means.m$year > "1834-12-31 19:03:58" & ts.means.m$year <="1971-12-31 19:00:00",], analysis)
  ggsave(paste0(results.directory,"plots/rev-exp-public-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-12"){
# Calculate avg. pointwise impact during intervention/post-period: >= 1866 & <= 1928

#rev.pc
rev.pc.mu <-mean(ts.means.m$value[ts.means.m$variable=="Pointwise rev.pc" & (ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])
rev.pc.mu

rev.pc.mu - (mean(sds$rev.pc.sd[(sds$year>=1866 & sds$year<=1915)])*1.96)
rev.pc.mu + (mean(sds$rev.pc.sd[(sds$year>=1866 & sds$year<=1915)])*1.96)

#exp.pc
exp.pc.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise exp.pc" & (ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])
exp.pc.mu

exp.pc.mu-(mean(sds$exp.pc.sd[(sds$year>=1866 & sds$year<=1915)])*1.96)
exp.pc.mu+(mean(sds$exp.pc.sd[(sds$year>=1866 & sds$year<=1915)])*1.96)

# Calculate cumulative impact during intervention/post-period: >= 1866 & <= 1928

#rev.pc
ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1914-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1866-12-31 19:03:58"]

ts.means.m$cumulative.rev.pc.max[ts.means.m$year=="1914-12-31 19:00:00"] - ts.means.m$cumulative.rev.pc.max[ts.means.m$year=="1866-12-31 19:03:58"]
ts.means.m$cumulative.rev.pc.min[ts.means.m$year=="1914-12-31 19:00:00"] - ts.means.m$cumulative.rev.pc.min[ts.means.m$year=="1866-12-31 19:03:58"]

#exp.pc
ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1914-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1866-12-31 19:03:58"]

sds$cumulative.rev.pc.max[(sds$year==1917)] - sds$cumulative.rev.pc.max[(sds$year==1889)]ts.means.m$cumulative.exp.pc.min[ts.means.m$year=="1914-12-31 19:00:00"] - ts.means.m$cumulative.exp.pc.min[ts.means.m$year=="1866-12-31 19:03:58"]
}

if(analysis=="analysis-34"){
  # Calculate avg. pointwise impact during intervention/post-period: >= 1889 & <= 1928
  
  #rev.pc
  rev.pc.mu <-mean(ts.means.m$value[ts.means.m$variable=="Pointwise rev.pc" & (ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year <= "1916-12-31 19:00:00")])
  rev.pc.mu
  
  rev.pc.mu - (mean(sds$rev.pc.sd[(sds$year>=1889 & sds$year<=1928)])*1.96)
  rev.pc.mu + (mean(sds$rev.pc.sd[(sds$year>=1889 & sds$year<=1928)])*1.96)
  
  #exp.pc
  exp.pc.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise exp.pc" & (ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year <= "1916-12-31 19:00:00")])
  exp.pc.mu
  
  exp.pc.mu-(mean(sds$exp.pc.sd[(sds$year>=1889 & sds$year<=1928)])*1.96)
  exp.pc.mu+(mean(sds$exp.pc.sd[(sds$year>=1889 & sds$year<=1928)])*1.96)
  
  # Calculate cumulative impact during intervention/post-period: >= 1889 & <= 1928
  
  #rev.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1916-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1889-12-31 19:00:00"]
  
  sds$cumulative.rev.pc.max[(sds$year==1928)] - sds$cumulative.rev.pc.max[(sds$year==1889)]
  sds$cumulative.rev.pc.min[(sds$year==1928)] - sds$cumulative.rev.pc.min[(sds$year==1889)]
  
  #exp.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1916-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1889-12-31 19:00:00"]
  
  sds$cumulative.exp.pc.max[(sds$year==1917)] - sds$cumulative.exp.pc.max[(sds$year==1889)]
  sds$cumulative.exp.pc.min[(sds$year==1917)] - sds$cumulative.exp.pc.min[(sds$year==1889)]
  
}