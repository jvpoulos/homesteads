# Plot time-series and estimate causal impacts
# Uses train/test sets from capacity-state.R

analysis <- "analysis-34"

type <- "treated"

source(paste0(code.directory,"ts-plot-rev-exp.R"))

## Revenues data
setwd(paste0(results.directory, "predictions/",analysis,"/",type,"/revpc")) # prediction files loc

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


# Bind to splits
rev.pc.test <- cbind(rev.pc.y.test, 
                         "rev.pc.mean"= rev.pc.test.mean, 
                         "rev.pc.sd"= rev.pc.test.sd) 
rev.pc.train <- cbind(rev.pc.y.train, 
                          "rev.pc.mean"=rev.pc.train.mean, 
                          "rev.pc.sd"=rev.pc.train.sd) 

## Expenditures data 
setwd(paste0(results.directory, "predictions/",analysis,"/",type,"/exppc")) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

exp.pc.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                             header=FALSE,
                                             col.names="exp.pc.pred"))
exp.pc.test.mean <-rowMeans(exp.pc.test.pred)
exp.pc.test.sd <- matrixStats::rowSds(as.matrix(exp.pc.test.pred))

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
exp.pc.train <- cbind(exp.pc.y.train, 
                          "exp.pc.mean"=exp.pc.train.mean, 
                          "exp.pc.sd"=exp.pc.train.sd) 

## Create time series data
setwd(code.directory)

ts.dat <- merge(rbind(rev.pc.train,rev.pc.test), rbind(exp.pc.train,exp.pc.test), by="year")

## Plot time series 

time.vars <- c("year","rev.pc.Treated","rev.pc.mean","exp.pc.Treated", "exp.pc.mean")

ts.means <- ts.dat[time.vars]  %>%
  mutate(pointwise.rev.pc = rev.pc.Treated-rev.pc.mean,
         cumulative.rev.pc = cumsum(pointwise.rev.pc),
         pointwise.exp.pc = exp.pc.Treated-exp.pc.mean,
         cumulative.exp.pc = cumsum(pointwise.exp.pc)) 

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
         cumulative.rev.pc.min = cumsum(pointwise.rev.pc.min),
         cumulative.rev.pc.max = cumsum(pointwise.rev.pc.max),
         pred.exp.pc.min = exp.pc.mean - exp.pc.sd*1.96,
         pred.exp.pc.max = exp.pc.mean + exp.pc.sd*1.96,
         pointwise.exp.pc.min = exp.pc.Treated-pred.exp.pc.min,
         pointwise.exp.pc.max = exp.pc.Treated-pred.exp.pc.max,
         cumulative.exp.pc.min = cumsum(pointwise.exp.pc.min),
         cumulative.exp.pc.max = cumsum(pointwise.exp.pc.max))

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
# Calculate Avg. pointwise impact during pre-period: < 1866

# rev.pc
rev.pc.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise rev.pc" & (ts.means.m$year<"1866-12-31 19:03:58")])
rev.pc.mu

rev.pc.mu - (mean(sds$rev.pc.sd[(sds$year< 1866)])*1.96)
rev.pc.mu + (mean(sds$rev.pc.sd[(sds$year< 1866)])*1.96)

# exp.pc
exp.pc.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise exp.pc" & (ts.means.m$year<"1866-12-31 19:03:58")])
exp.pc.mu

exp.pc.mu- (mean(sds$exp.pc.sd[(sds$year< 1866)])*1.96)
exp.pc.mu+ (mean(sds$exp.pc.sd[(sds$year< 1866)])*1.96)

# Calculate Avg. cumulative impact during pre-period:< 1866

#rev.pc
ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1865-12-31 19:03:58"]

sds$cumulative.rev.pc.max[(sds$year==1865)]
sds$cumulative.rev.pc.min[(sds$year==1865)]

#exp.pc
ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1865-12-31 19:03:58"]

sds$cumulative.exp.pc.max[(sds$year==1865)]
sds$cumulative.exp.pc.min[(sds$year==1865)]

# Calculate avg. pointwise impact during intervention/post-period: >= 1866 & <= 1976

#rev.pc
rev.pc.mu <-mean(ts.means.m$value[ts.means.m$variable=="Pointwise rev.pc" & (ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1971-12-31 19:00:00")])
rev.pc.mu

rev.pc.mu - (mean(sds$rev.pc.sd[(sds$year>=1866 & sds$year<=1976)])*1.96)
rev.pc.mu + (mean(sds$rev.pc.sd[(sds$year>=1866 & sds$year<=1976)])*1.96)

#exp.pc
exp.pc.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise exp.pc" & (ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1971-12-31 19:00:00")])
exp.pc.mu

exp.pc.mu-(mean(sds$exp.pc.sd[(sds$year>=1866 & sds$year<=1976)])*1.96)
exp.pc.mu+(mean(sds$exp.pc.sd[(sds$year>=1866 & sds$year<=1976)])*1.96)

# Calculate cumulative impact during intervention/post-period: >= 1866 & <= 1976

#rev.pc
ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1971-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1866-12-31 19:03:58"]

sds$cumulative.rev.pc.max[(sds$year==1972)] - sds$cumulative.rev.pc.max[(sds$year==1866)]
sds$cumulative.rev.pc.min[(sds$year==1972)] - sds$cumulative.rev.pc.min[(sds$year==1866)]

#exp.pc
ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1971-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1866-12-31 19:03:58"]

sds$cumulative.exp.pc.max[(sds$year==1972)] - sds$cumulative.exp.pc.max[(sds$year==1866)]
sds$cumulative.exp.pc.min[(sds$year==1972)] - sds$cumulative.exp.pc.min[(sds$year==1866)]

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

sds$cumulative.rev.pc.max[(sds$year==1915)] - sds$cumulative.rev.pc.max[(sds$year==1866)]
sds$cumulative.rev.pc.min[(sds$year==1915)] - sds$cumulative.rev.pc.min[(sds$year==1866)]

#exp.pc
ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1914-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1866-12-31 19:03:58"]

sds$cumulative.exp.pc.max[(sds$year==1915)] - sds$cumulative.exp.pc.max[(sds$year==1866)]
sds$cumulative.exp.pc.min[(sds$year==1915)] - sds$cumulative.exp.pc.min[(sds$year==1866)]
}

if(analysis=="analysis-34"){
  # Calculate Avg. pointwise impact during pre-period: < 1889
  
  # rev.pc
  rev.pc.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise rev.pc" & (ts.means.m$year<"1889-12-31 19:03:58")])
  rev.pc.mu
  
  rev.pc.mu - (mean(sds$rev.pc.sd[(sds$year< 1889)])*1.96)
  rev.pc.mu + (mean(sds$rev.pc.sd[(sds$year< 1889)])*1.96)
  
  # exp.pc
  exp.pc.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise exp.pc" & (ts.means.m$year<"1889-12-31 19:03:58")])
  exp.pc.mu
  
  exp.pc.mu- (mean(sds$exp.pc.sd[(sds$year< 1889)])*1.96)
  exp.pc.mu+ (mean(sds$exp.pc.sd[(sds$year< 1889)])*1.96)
  
  # Calculate Avg. cumulative impact during pre-period:< 1889
  
  #rev.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1888-12-31 19:00:00"]
  
  sds$cumulative.rev.pc.max[(sds$year==1888)]
  sds$cumulative.rev.pc.min[(sds$year==1888)]
  
  #exp.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1888-12-31 19:00:00"]
  
  sds$cumulative.exp.pc.max[(sds$year==1888)]
  sds$cumulative.exp.pc.min[(sds$year==1888)]
  
  # Calculate avg. pointwise impact during intervention/post-period: >= 1889 & <= 1976
  
  #rev.pc
  rev.pc.mu <-mean(ts.means.m$value[ts.means.m$variable=="Pointwise rev.pc" & (ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year <= "1971-12-31 19:00:00")])
  rev.pc.mu
  
  rev.pc.mu - (mean(sds$rev.pc.sd[(sds$year>=1889 & sds$year<=1976)])*1.96)
  rev.pc.mu + (mean(sds$rev.pc.sd[(sds$year>=1889 & sds$year<=1976)])*1.96)
  
  #exp.pc
  exp.pc.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise exp.pc" & (ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year <= "1971-12-31 19:00:00")])
  exp.pc.mu
  
  exp.pc.mu-(mean(sds$exp.pc.sd[(sds$year>=1889 & sds$year<=1976)])*1.96)
  exp.pc.mu+(mean(sds$exp.pc.sd[(sds$year>=1889 & sds$year<=1976)])*1.96)
  
  # Calculate cumulative impact during intervention/post-period: >= 1889 & <= 1976
  
  #rev.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1971-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1889-12-31 19:00:00"]
  
  sds$cumulative.rev.pc.max[(sds$year==1972)] - sds$cumulative.rev.pc.max[(sds$year==1889)]
  sds$cumulative.rev.pc.min[(sds$year==1972)] - sds$cumulative.rev.pc.min[(sds$year==1889)]
  
  #exp.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1971-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1889-12-31 19:00:00"]
  
  sds$cumulative.exp.pc.max[(sds$year==1972)] - sds$cumulative.exp.pc.max[(sds$year==1889)]
  sds$cumulative.exp.pc.min[(sds$year==1972)] - sds$cumulative.exp.pc.min[(sds$year==1889)]
  
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
  
  sds$cumulative.rev.pc.max[(sds$year==1917)] - sds$cumulative.rev.pc.max[(sds$year==1889)]
  sds$cumulative.rev.pc.min[(sds$year==1917)] - sds$cumulative.rev.pc.min[(sds$year==1889)]
  
  #exp.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1916-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1889-12-31 19:00:00"]
  
  sds$cumulative.exp.pc.max[(sds$year==1917)] - sds$cumulative.exp.pc.max[(sds$year==1889)]
  sds$cumulative.exp.pc.min[(sds$year==1917)] - sds$cumulative.exp.pc.min[(sds$year==1889)]
  
}