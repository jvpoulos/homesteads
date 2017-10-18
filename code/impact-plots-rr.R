# Plot time-series and estimate causal impacts
# Uses train/test sets from railroads.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)

analysis <- "analysis-41"

type <- "treated"

source(paste0(code.directory,"ts-plot-rr.R"))
source(paste0(code.directory,"PolitisWhite.R"))

## Railroad data
setwd(paste0(results.directory, "predictions/","/access/",analysis,"/",type)) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

access.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                          header=FALSE,
                                          col.names="access.pred"))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

access.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                           header=FALSE,
                                           col.names="access.pred"))

# Import validation fit

val.files <- list.files(pattern = "*val.csv")

access.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                         header=FALSE,
                                         col.names="access.pred"))


if(analysis=="analysis-01"){
# Bind to splits
access.test <- cbind(access.0.y.test, access.test.pred) 

access.val <- cbind(access.0.y.val, access.val.pred) 

access.train <- cbind(access.0.y.train, access.train.pred)
}

if(analysis=="analysis-12"){
  # Bind to splits
  access.test <- cbind(access.1.y.test, access.test.pred) 
  
  access.val <- cbind(access.1.y.val, access.val.pred) 
  
  access.train <- cbind(access.1.y.train, access.train.pred)
}

if(analysis=="analysis-34"){
  # Bind to splits
  access.test <- cbind(access.3.y.test, access.test.pred) 
  
  access.val <- cbind(access.3.y.val, access.val.pred) 
  
  access.train <- cbind(access.3.y.train, access.train.pred)
}

if(analysis=="analysis-41"){
  # Bind to splits
  access.test <- cbind(access.4.y.test, access.test.pred) 
  
  access.val <- cbind(access.4.y.val, access.val.pred) 
  
  access.train <- cbind(access.4.y.train, access.train.pred)
}

# Bootstrap estimate for prediction

access.bind <- rbind(rbind(access.train,access.val),access.test)

access.bopt <- b.star(access.bind["access.pred"][[1]],round=TRUE)[[1]]  # get optimal bootstrap lengths

access.boot <- tsbootstrap(access.bind["access.pred"][[1]], nb=1000, b= access.bopt, type="block")
access.sd <- matrixStats::rowSds(as.matrix(access.boot))

## Create time series data
setwd(code.directory)

ts.dat <- cbind(access.bind, access.sd)

## Plot time series 

ts.means <- ts.dat  %>%
  mutate(pointwise.access = access.Treated-access.pred,
         cumulative.access = cumsum(pointwise.access))

ts.means <- ts.means[with(ts.means, order(year)), ] # sort by year

# ts.means$cumulative.access <- NA
# 
# for (i in 1:nrow(ts.means)){
#   ts.means$cumulative.access[i] <- rollmean(ts.means$pointwise.access,i, align='right')
# }

ts.means.m <- melt(as.data.frame(ts.means)[!colnames(ts.means) %in% c("access.sd")], id.var=c("year"))

# # Adjust year for plot
ts.means.m$year <- as.yearmon(ts.means.m$year, "%Y-%m",tz="UTC") # convert year to year class

ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")

# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="access.Treated" | ts.means.m$variable=="access.pred"] <- "Time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.access" ] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.access"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed access","Predicted access",
                                 "Pointwise access", "Cumulative access")

# SDs

sds <- ts.dat  %>%
  mutate(pred.access.min = access.pred - access.sd*1.96,
         pred.access.max = access.pred + access.sd*1.96,
         pointwise.access.min = access.Treated-pred.access.max,
         pointwise.access.max = access.Treated-pred.access.min,
         cumulative.access.min = cumsum(pointwise.access.min),
         cumulative.access.max = cumsum(pointwise.access.max))

sds <- sds[with(sds, order(year)), ] # sort by year

# for (i in 1:nrow(sds)){
#   sds$cumulative.access.min[i] <- rollmean(sds$pointwise.access.min,i, align='right')
# }
# for (i in 1:nrow(sds)){
#   sds$cumulative.access.max[i] <- rollmean(sds$pointwise.access.max,i, align='right')
# }

pred.vars <- c("access.pred", "access.sd", "pred.access.min", "pred.access.max", "pointwise.access.min", "pointwise.access.max", "cumulative.access.min", "cumulative.access.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

if(analysis=="analysis-01"){
  ts.plot <- TsPlotRR(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/rr-hsa-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-12"){
  ts.plot <- TsPlotRR(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/rr-sha-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-34"){
  ts.plot <- TsPlotRR(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/rr-89-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-41"){
  ts.plot <- TsPlotRR(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/rr-89-treat-west.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-01"){
  
  # Calculate avg. pointwise impact during intervention/post-period: >= 1862
  
  #access
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise access" & (ts.means.m$year>="1862-12-31 19:03:58")]) 
  
  mean(ts.means.m$pointwise.access.min[(ts.means.m$year>="1862-12-31 19:03:58")]) 
  mean(ts.means.m$pointwise.access.max[(ts.means.m$year>="1862-12-31 19:03:58")]) 
  
  # Calculate cumulative impact during intervention/post-period: >= 1862
  
  #access
  ts.means.m$value[ts.means.m$variable=="Cumulative access" & (ts.means.m$year=="1910-12-31 19:00:00")] - ts.means.m$value[ts.means.m$variable=="Cumulative access" & (ts.means.m$year=="1862-12-31 19:03:58")]
  
  ts.means.m$cumulative.access.min[(ts.means.m$year=="1910-12-31 19:00:00")][[1]] -ts.means.m$cumulative.access.min[(ts.means.m$year=="1862-12-31 19:03:58")][[1]]
  ts.means.m$cumulative.access.max[(ts.means.m$year=="1910-12-31 19:00:00")][[1]]- ts.means.m$cumulative.access.max[(ts.means.m$year=="1862-12-31 19:03:58")][[1]]
  
  
}

if(analysis=="analysis-12"){
  
  # Calculate avg. pointwise impact during intervention period: >= 1866
  
  #access
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise access" & (ts.means.m$year>="1866-12-31 19:03:58")])
  
  mean(ts.means.m$pointwise.access.min[(ts.means.m$year>="1866-12-31 19:03:58")])
  mean(ts.means.m$pointwise.access.max[(ts.means.m$year>="1866-12-31 19:03:58")])
  
  # Calculate cumulative impact during intervention/post-period: >= 1866
  
  #access
  ts.means.m$value[ts.means.m$variable=="Cumulative access" & (ts.means.m$year=="1910-12-31 19:00:00")] - ts.means.m$value[ts.means.m$variable=="Cumulative access" & (ts.means.m$year=="1866-12-31 19:03:58")]
  
  ts.means.m$cumulative.access.min[(ts.means.m$year=="1910-12-31 19:00:00")][[1]] -ts.means.m$cumulative.access.min[(ts.means.m$year=="1866-12-31 19:03:58")][[1]]
  ts.means.m$cumulative.access.max[(ts.means.m$year=="1910-12-31 19:00:00")][[1]]- ts.means.m$cumulative.access.max[(ts.means.m$year=="1866-12-31 19:03:58")][[1]]
  

  
}

if(analysis=="analysis-34" | analysis=="analysis-41"){
  # Calculate avg. pointwise impact during intervention/post-period: >= 1889
  
  #access
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise access" & (ts.means.m$year>="1889-12-31 19:00:00")])
  
  mean(ts.means.m$pointwise.access.min[(ts.means.m$year>="1889-12-31 19:00:00")])
  mean(ts.means.m$pointwise.access.max[(ts.means.m$year>="1889-12-31 19:00:00")])
  
  # Calculate cumulative impact during intervention/post-period: >= 1889
  
  #access
  ts.means.m$value[ts.means.m$variable=="Cumulative access" & (ts.means.m$year=="1910-12-31 19:00:00")] - ts.means.m$value[ts.means.m$variable=="Cumulative access" & (ts.means.m$year=="1889-12-31 19:00:00")]
  
  ts.means.m$cumulative.access.min[(ts.means.m$year=="1910-12-31 19:00:00")][[1]] -ts.means.m$cumulative.access.min[(ts.means.m$year=="1889-12-31 19:00:00")][[1]]
  ts.means.m$cumulative.access.max[(ts.means.m$year=="1910-12-31 19:00:00")][[1]]- ts.means.m$cumulative.access.max[(ts.means.m$year=="1889-12-31 19:00:00")][[1]]
  
  

}