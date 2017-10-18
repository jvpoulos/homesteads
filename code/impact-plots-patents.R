# Plot time-series and estimate causal impacts
# Uses train/test sets from patents.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)

analysis <- "analysis-01"

type <- "treated"

source(paste0(code.directory,"ts-plot-patents.R"))
source(paste0(code.directory,"PolitisWhite.R"))

## Sales data
setwd(paste0(results.directory, "predictions/","/sales/",analysis,"/",type)) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

sales.pc.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                         header=FALSE,
                                         col.names="sales.pc.pred"))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

sales.pc.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                          header=FALSE,
                                          col.names="sales.pc.pred"))

# Import validation fit

val.files <- list.files(pattern = "*val.csv")

sales.pc.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                        header=FALSE,
                                        col.names="sales.pc.pred"))


# Bind to splits
sales.pc.test <- cbind(sales.pc.y.test, sales.pc.test.pred) 

sales.pc.val <- cbind(sales.pc.y.val, sales.pc.val.pred) 

sales.pc.train <- cbind(sales.pc.y.train, sales.pc.train.pred) 

# Bootstrap estimate for prediction

sales.pc.bind <- rbind(rbind(sales.pc.train,sales.pc.val),sales.pc.test)

sales.pc.bopt <- b.star(sales.pc.bind["sales.pc.pred"][[1]],round=TRUE)[[1]]  # get optimal bootstrap lengths

sales.pc.boot <- tsbootstrap(sales.pc.bind["sales.pc.pred"][[1]], nb=1000, b= sales.pc.bopt, type="block") 
sales.pc.sd <- matrixStats::rowSds(as.matrix(sales.pc.boot))

## Homesteads data 
setwd(paste0(results.directory, "predictions/","/homesteads/",analysis,"/",type)) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

homesteads.pc.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                         header=FALSE,
                                         col.names="homesteads.pc.pred"))

# Import val results

val.files <- list.files(pattern = "*val.csv")

homesteads.pc.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                        header=FALSE,
                                        col.names="homesteads.pc.pred"))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

homesteads.pc.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                          header=FALSE,
                                          col.names="homesteads.pc.pred"))

# Bind to splits
homesteads.pc.test <- cbind(homesteads.pc.y.test, homesteads.pc.test.pred) 

homesteads.pc.val <- cbind(homesteads.pc.y.val, homesteads.pc.val.pred) 

homesteads.pc.train <- cbind(homesteads.pc.y.train, homesteads.pc.train.pred) 

# Bootstrap estimate for prediction

homesteads.pc.bind <- rbind(rbind(homesteads.pc.train,homesteads.pc.test),homesteads.pc.val)

homesteads.pc.bopt <- b.star(homesteads.pc.bind["homesteads.pc.pred"][[1]],round=TRUE)[[1]]

homesteads.pc.boot <- tsbootstrap(homesteads.pc.bind["homesteads.pc.pred"][[1]], nb=1000, b=homesteads.pc.bopt, type="stationary") # stationary bootstrap with mean block length b according to Politis and Romano (1994)
homesteads.pc.sd <- matrixStats::rowSds(as.matrix(homesteads.pc.boot))

## Create time series data
setwd(code.directory)

ts.dat <- merge(cbind(sales.pc.bind, sales.pc.sd), cbind(homesteads.pc.bind, homesteads.pc.sd), by="year")

## Plot time series 

ts.means <- ts.dat  %>%
  mutate(pointwise.sales.pc = sales.pc.Treated-sales.pc.pred,
         pointwise.homesteads.pc = homesteads.pc.Treated-homesteads.pc.pred,
         cumulative.sales.pc = cumsum(pointwise.sales.pc),
         cumulative.homesteads.pc = cumsum(pointwise.homesteads.pc)) 

ts.means <- ts.means[with(ts.means, order(year)), ] # sort by year

# ts.means$cumulative.sales.pc <- NA
# ts.means$cumulative.homesteads.pc <- NA
# 
# for (i in 1:nrow(ts.means)){
#   ts.means$cumulative.sales.pc[i] <- rollmean(ts.means$pointwise.sales.pc,i, align='right')
# }
# 
# for (i in 1:nrow(ts.means)){
#   ts.means$cumulative.homesteads.pc[i] <- rollmean(ts.means$pointwise.homesteads.pc,i, align='right')
# }         

ts.means.m <- melt(as.data.frame(ts.means)[!colnames(ts.means) %in% c("sales.pc.sd" , "homesteads.pc.sd" )], id.var=c("year"))

# # Adjust year for plot
ts.means.m$year <- as.yearmon(ts.means.m$year, "%Y-%m",tz="UTC") # convert year to year class

ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")

# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="sales.pc.Treated" | ts.means.m$variable=="sales.pc.pred"] <- "Sales time-series"
ts.means.m$series[ts.means.m$variable=="homesteads.pc.Treated" | ts.means.m$variable=="homesteads.pc.pred"] <- "Homesteads time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.sales.pc" | ts.means.m$variable=="pointwise.homesteads.pc"] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.sales.pc" | ts.means.m$variable=="cumulative.homesteads.pc"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Sales time-series","Homesteads time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed sales.pc","Predicted sales.pc", "Observed homesteads.pc", "Predicted homesteads.pc",
                                 "Pointwise sales.pc", "Pointwise homesteads.pc", "Cumulative sales.pc",  "Cumulative homesteads.pc")

# SDs

sds <- ts.dat  %>%
  mutate(pred.sales.pc.min = sales.pc.pred - sales.pc.sd*1.96,
         pred.sales.pc.max = sales.pc.pred + sales.pc.sd*1.96,
         pointwise.sales.pc.min = sales.pc.Treated-pred.sales.pc.max,
         pointwise.sales.pc.max = sales.pc.Treated-pred.sales.pc.min,
         cumulative.sales.pc.min = cumsum(pointwise.sales.pc.min),
         cumulative.sales.pc.max = cumsum(pointwise.sales.pc.max),
         pred.homesteads.pc.min = homesteads.pc.pred - homesteads.pc.sd*1.96,
         pred.homesteads.pc.max = homesteads.pc.pred + homesteads.pc.sd*1.96,
         pointwise.homesteads.pc.min = homesteads.pc.Treated-pred.homesteads.pc.max,
         pointwise.homesteads.pc.max = homesteads.pc.Treated-pred.homesteads.pc.min,
         cumulative.homesteads.pc.min = cumsum(pointwise.homesteads.pc.min),
         cumulative.homesteads.pc.max = cumsum(pointwise.homesteads.pc.max))

sds <- sds[with(sds, order(year)), ] # sort by year

# for (i in 1:nrow(sds)){
#   sds$cumulative.sales.pc.min[i] <- rollmean(sds$pointwise.sales.pc.min,i, align='right')
# }
# for (i in 1:nrow(sds)){
#   sds$cumulative.sales.pc.max[i] <- rollmean(sds$pointwise.sales.pc.max,i, align='right')
# }
# 
# for (i in 1:nrow(sds)){
#   sds$cumulative.homesteads.pc.min[i] <- rollmean(sds$pointwise.homesteads.pc.min,i, align='right')
# }
# for (i in 1:nrow(sds)){
#   sds$cumulative.homesteads.pc.max[i] <- rollmean(sds$pointwise.homesteads.pc.max,i, align='right')
# }

pred.vars <- c("sales.pc.pred", "sales.pc.sd", "pred.sales.pc.min", "pred.sales.pc.max", "pointwise.sales.pc.min", "pointwise.sales.pc.max", "cumulative.sales.pc.min", "cumulative.sales.pc.max",
               "homesteads.pc.pred", "homesteads.pc.sd", "pred.homesteads.pc.min", "pred.homesteads.pc.max", "pointwise.homesteads.pc.min", "pointwise.homesteads.pc.max", "cumulative.homesteads.pc.min", "cumulative.homesteads.pc.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

if(analysis=="analysis-01"){
  ts.plot <- TsPlotPatents(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/sales-homesteads-hsa-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-12"){
  ts.plot <- TsPlotPatents(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/sales-homesteads-sha-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-34"){
  ts.plot <- TsPlotPatents(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/sales-homesteads-89-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-41"){
  ts.plot <- TsPlotPatents(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/sales-homesteads-89-treat-west.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-01"){
  
  # Calculate avg. pointwise impact during intervention/post-period: >= 1862 <=1976
  
  #sales.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise sales.pc" & (ts.means.m$year>="1862-12-31 19:03:58" & ts.means.m$year<="1976-12-31 19:03:58")]) 
  
  mean(ts.means.m$pointwise.sales.pc.min[(ts.means.m$year>="1862-12-31 19:03:58" & ts.means.m$year<="1976-12-31 19:03:58")]) 
  mean(ts.means.m$pointwise.sales.pc.max[(ts.means.m$year>="1862-12-31 19:03:58" & ts.means.m$year<="1976-12-31 19:03:58")]) 
  
  #homesteads.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise homesteads.pc" & (ts.means.m$year>="1862-12-31 19:03:58" & ts.means.m$year<="1976-12-31 19:03:58")])
  
  mean(ts.means.m$pointwise.homesteads.pc.min[(ts.means.m$year>="1862-12-31 19:03:58" & ts.means.m$year<="1976-12-31 19:03:58")])
  mean(ts.means.m$pointwise.homesteads.pc.max[(ts.means.m$year>="1862-12-31 19:03:58" & ts.means.m$year<="1976-12-31 19:03:58")])
  
  
  # Calculate cumulative impact during intervention/post-period: >= 1862 <=1976
  
  #sales.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative sales.pc" & (ts.means.m$year=="1976-12-31 19:00:00")] - ts.means.m$value[ts.means.m$variable=="Cumulative sales.pc" & (ts.means.m$year=="1862-12-31 19:03:58")]
  
  ts.means.m$cumulative.sales.pc.min[(ts.means.m$year=="1976-12-31 19:00:00")][[1]] - ts.means.m$cumulative.sales.pc.min[(ts.means.m$year=="1862-12-31 19:03:58")][[1]]
  ts.means.m$cumulative.sales.pc.max[(ts.means.m$year=="1976-12-31 19:00:00")][[1]] - ts.means.m$cumulative.sales.pc.max[(ts.means.m$year=="1862-12-31 19:03:58")][[1]] 
  
  #homesteads.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative homesteads.pc" & (ts.means.m$year=="1976-12-31 19:00:00")] - ts.means.m$value[ts.means.m$variable=="Cumulative homesteads.pc" & (ts.means.m$year=="1862-12-31 19:03:58")]
  
  ts.means.m$cumulative.homesteads.pc.min[(ts.means.m$year=="1976-12-31 19:00:00")][[1]] - ts.means.m$cumulative.homesteads.pc.min[(ts.means.m$year=="1862-12-31 19:03:58")][[1]]
  ts.means.m$cumulative.homesteads.pc.max[(ts.means.m$year=="1976-12-31 19:00:00")][[1]] - ts.means.m$cumulative.homesteads.pc.max[(ts.means.m$year=="1862-12-31 19:03:58")][[1]] 
  
}

if(analysis=="analysis-12"){
  
  # Calculate avg. pointwise impact during intervention period: >= 1866
  
  #sales.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise sales.pc" & (ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year<="1976-12-31 19:00:00")])
  
  mean(ts.means.m$pointwise.sales.pc.min[(ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year<="1976-12-31 19:00:00")])
  mean(ts.means.m$pointwise.sales.pc.max[(ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year<="1976-12-31 19:00:00")])
  
  #homesteads.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise homesteads.pc" & (ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year<="1976-12-31 19:00:00")])
  
  mean(ts.means.m$pointwise.homesteads.pc.min[(ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year<="1976-12-31 19:00:00")])
  mean(ts.means.m$pointwise.homesteads.pc.max[(ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year<="1976-12-31 19:00:00")])
  
  # Calculate cumulative impact during intervention/post-period: >= 1866 <=1976
  
  #sales.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative sales.pc" & (ts.means.m$year=="1976-12-31 19:00:00")] - ts.means.m$value[ts.means.m$variable=="Cumulative sales.pc" & (ts.means.m$year=="1866-12-31 19:03:58")]
  
  ts.means.m$cumulative.sales.pc.min[(ts.means.m$year=="1976-12-31 19:00:00")][[1]] - ts.means.m$cumulative.sales.pc.min[(ts.means.m$year=="1866-12-31 19:03:58")][[1]]
  ts.means.m$cumulative.sales.pc.max[(ts.means.m$year=="1976-12-31 19:00:00")][[1]] - ts.means.m$cumulative.sales.pc.max[(ts.means.m$year=="1866-12-31 19:03:58")][[1]] 
  
  #homesteads.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative homesteads.pc" & (ts.means.m$year=="1976-12-31 19:00:00")] - ts.means.m$value[ts.means.m$variable=="Cumulative homesteads.pc" & (ts.means.m$year=="1866-12-31 19:03:58")]
  
  ts.means.m$cumulative.homesteads.pc.min[(ts.means.m$year=="1976-12-31 19:00:00")][[1]] - ts.means.m$cumulative.homesteads.pc.min[(ts.means.m$year=="1866-12-31 19:03:58")][[1]]
  ts.means.m$cumulative.homesteads.pc.max[(ts.means.m$year=="1976-12-31 19:00:00")][[1]] - ts.means.m$cumulative.homesteads.pc.max[(ts.means.m$year=="1866-12-31 19:03:58")][[1]] 
  
  
}

if(analysis=="analysis-34" | analysis=="41"){
  # Calculate avg. pointwise impact during intervention/post-period: >= 1889
  
  #sales.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise sales.pc" & (ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year<="1976-12-31 19:00:00")])
  
  mean(ts.means.m$pointwise.sales.pc.min[(ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year<="1976-12-31 19:00:00")])
  mean(ts.means.m$pointwise.sales.pc.max[(ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year<="1976-12-31 19:00:00")])
  
  #homesteads.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise homesteads.pc" & (ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year<="1976-12-31 19:00:00")])
  
  mean(ts.means.m$pointwise.homesteads.pc.min[(ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year<="1976-12-31 19:00:00")])
  mean(ts.means.m$pointwise.homesteads.pc.max[(ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year<="1976-12-31 19:00:00")])
  
  # Calculate cumulative impact during intervention/post-period: >= 1889 <=1976
  
  #sales.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative sales.pc" & (ts.means.m$year=="1976-12-31 19:00:00")] - ts.means.m$value[ts.means.m$variable=="Cumulative sales.pc" & (ts.means.m$year=="1889-12-31 19:00:00")]
  
  ts.means.m$cumulative.sales.pc.min[(ts.means.m$year=="1976-12-31 19:00:00")][[1]] - ts.means.m$cumulative.sales.pc.min[(ts.means.m$year=="1889-12-31 19:00:00")][[1]]
  ts.means.m$cumulative.sales.pc.max[(ts.means.m$year=="1976-12-31 19:00:00")][[1]] - ts.means.m$cumulative.sales.pc.max[(ts.means.m$year=="1889-12-31 19:00:00")][[1]] 
  
  #homesteads.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative homesteads.pc" & (ts.means.m$year=="1976-12-31 19:00:00")] - ts.means.m$value[ts.means.m$variable=="Cumulative homesteads.pc" & (ts.means.m$year=="1889-12-31 19:00:00")]
  
  ts.means.m$cumulative.homesteads.pc.min[(ts.means.m$year=="1976-12-31 19:00:00")][[1]] - ts.means.m$cumulative.homesteads.pc.min[(ts.means.m$year=="1889-12-31 19:00:00")][[1]]
  ts.means.m$cumulative.homesteads.pc.max[(ts.means.m$year=="1976-12-31 19:00:00")][[1]] - ts.means.m$cumulative.homesteads.pc.max[(ts.means.m$year=="1889-12-31 19:00:00")][[1]] 
  
}