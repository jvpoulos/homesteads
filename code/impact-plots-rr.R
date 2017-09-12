# Plot time-series and estimate causal impacts
# Uses train/test sets from railroads.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)

source(paste0(code.directory,"ts-plot-rr.R"))

analysis <- "analysis-34"

type <- "treated"

## Railroad data
setwd(paste0(results.directory, "predictions/","/track2/",analysis,"/",type)) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")[1:15] # use first 15

track2.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                header=FALSE,
                                col.names="track2.pred"))
track2.test.mean <-rowMeans(track2.test.pred)
track2.test.sd <- matrixStats::rowSds(as.matrix(track2.test.pred))

# Import val results

val.files <- list.files(pattern = "*val.csv")[1:15] # use first 15

track2.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                        header=FALSE,
                                        col.names="track2.pred"))
track2.val.mean <-rowMeans(track2.val.pred)
track2.val.sd <- matrixStats::rowSds(as.matrix(track2.val.pred))

# Import training fit

train.files <- list.files(pattern = "*train.csv")[1:15] # use first 15

track2.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                        header=FALSE,
                                        col.names="track2.pred"))
track2.train.mean <- rowMeans(as.matrix(track2.train.pred))
track2.train.sd <- matrixStats::rowSds(as.matrix(track2.train.pred))


# Bind to splits

if(analysis=="analysis-12"){
  track2.test <- cbind(track2.1.y.test, 
                       "track2.mean"= track2.test.mean, 
                       "track2.sd"= track2.test.sd)
  track2.val <- cbind(track2.1.y.val, 
                      "track2.mean"= track2.val.mean, 
                      "track2.sd"= track2.val.sd)
  track2.train <- cbind(track2.1.y.train, 
                        "track2.mean"=track2.train.mean, 
                        "track2.sd"=track2.train.sd) 
}

if(analysis=="analysis-34"){
track2.test <- cbind(track2.3.y.test, 
                       "track2.mean"= track2.test.mean, 
                       "track2.sd"= track2.test.sd)
track2.val <- cbind(track2.3.y.val, 
                    "track2.mean"= track2.val.mean, 
                    "track2.sd"= track2.val.sd) 
track2.train <- cbind(track2.3.y.train, 
                      "track2.mean"=track2.train.mean, 
                      "track2.sd"=track2.train.sd) 
}

## Create time series data
setwd(code.directory)

ts.dat <- rbind(rbind(track2.train,track2.test),track2.val)

## Plot time series 

time.vars <- c("year","track2.Treated","track2.mean")

ts.means <- ts.dat[time.vars]  %>%
  mutate(pointwise.track2 = track2.Treated-track2.mean,
         cumulative.track2 = NA)

ts.means <- ts.means[with(ts.means, order(year)), ] # sort by year

for (i in 1:nrow(ts.means)){
  ts.means$cumulative.track2[i] <- rollmean(ts.means$pointwise.track2,i, align='right')
}

ts.means.m <- melt(as.data.frame(ts.means), id.var=c("year"))

# Adjust year for plot
ts.means.m$year <- as.yearmon(ts.means.m$year, "%Y-%m",tz="UTC") # convert year to year class

ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")
                         
# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="track2.Treated" | ts.means.m$variable=="track2.mean"] <- "Time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.track2" ] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.track2" ] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed track2","Predicted track2", 
                                 "Pointwise track2", "Cumulative track2")

# SDs

sds <- ts.dat  %>%
  mutate(pred.track2.min = track2.mean - track2.sd*1.96,
         pred.track2.max = track2.mean + track2.sd*1.96,
         pointwise.track2.min = track2.Treated-pred.track2.min,
         pointwise.track2.max = track2.Treated-pred.track2.max,
         cumulative.track2.min = NA,
         cumulative.track2.max = NA)

sds <- sds[with(sds, order(year)), ] # sort by year

for (i in 1:nrow(sds)){
  sds$cumulative.track2.min[i] <- rollmean(sds$pointwise.track2.min,i, align='right')
}
for (i in 1:nrow(sds)){
  sds$cumulative.track2.max[i] <- rollmean(sds$pointwise.track2.max,i, align='right')
}

pred.vars <- c("track2.mean", "track2.sd", "pred.track2.min", "pred.track2.max", "pointwise.track2.min", "pointwise.track2.max", "cumulative.track2.min", "cumulative.track2.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

if(analysis=="analysis-12"){
  ts.plot <- TsPlotRR(ts.means.m, analysis)
  ggsave(paste0(results.directory,"plots/track-south-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-34"){
  ts.plot <- TsPlotRR(ts.means.m, analysis)
  ggsave(paste0(results.directory,"plots/track-public-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-12"){

# Calculate avg. pointwise impact during intervention/post-period: >= 1866 & <=1928

#track2
track2.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise track2" & (ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1910-12-31 19:00:00")])
track2.mu

track2.mu - (mean(sds$track2.sd[(sds$year>=1866 & sds$year<=1911)])*1.96)
track2.mu + (mean(sds$track2.sd[(sds$year>=1866 & sds$year<=1911)])*1.96)

# Calculate cumulative impact during intervention/post-period: >= 1866 <=1928

#track2
track2.mu <- ts.means.m$value[ts.means.m$variable=="Cumulative track2" & ts.means.m$year=="1910-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative track2" & ts.means.m$year=="1866-12-31 19:03:58"]
track2.mu

sds$cumulative.track2.max[(sds$year==1911)] -sds$cumulative.track2.max[(sds$year==1866)]
sds$cumulative.track2.min[(sds$year==1911)] -sds$cumulative.track2.min[(sds$year==1866)]
}

if(analysis=="analysis-34"){
  # Calculate avg. pointwise impact during intervention/post-period: >= 1889 & <= 1928
  
  #track2
  track2.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise track2" & (ts.means.m$year>="1889-12-31 19:00:00") & (ts.means.m$year<="1916-12-31 19:00:00")])
  track2.mu
  
  track2.mu - (mean(sds$track2.sd[(sds$year>=1889 & sds$year<=1917)])*1.96)
  track2.mu + (mean(sds$track2.sd[(sds$year>=1889 & sds$year<=1917)])*1.96)
  
  # Calculate cumulative impact during intervention/post-period: >= 1889 & <= 1928
  
  #track2
  ts.means.m$value[ts.means.m$variable=="Cumulative track2" & ts.means.m$year=="1910-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative track2" & ts.means.m$year=="1889-12-31 19:00:00"]
  
  sds$cumulative.track2.max[(sds$year==1911)] -sds$cumulative.track2.max[(sds$year==1889)]
  sds$cumulative.track2.min[(sds$year==1911)] -sds$cumulative.track2.min[(sds$year==1889)]
}