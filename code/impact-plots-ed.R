# Plot time-series and estimate causal impacts
# Uses train/test sets from capacity-state.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)

analysis <- "analysis-01"

type <- "treated"

source(paste0(code.directory,"ts-plot-ed.R"))
source(paste0(code.directory,"PolitisWhite.R"))

## Education data
setwd(paste0(results.directory, "predictions/","/educpc/",analysis,"/",type)) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

educ.pc.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                         header=FALSE,
                                         col.names="educ.pc.pred"))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

educ.pc.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                          header=FALSE,
                                          col.names="educ.pc.pred"))

# Import validation fit

val.files <- list.files(pattern = "*val.csv")

educ.pc.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                        header=FALSE,
                                        col.names="educ.pc.pred"))


# Bind to splits
educ.pc.test <- cbind(educ.pc.y.test, educ.pc.test.pred) 

educ.pc.val <- cbind(educ.pc.y.val, educ.pc.val.pred) 

educ.pc.train <- cbind(educ.pc.y.train, educ.pc.train.pred) 

# Bootstrap estimate for prediction

educ.pc.bind <- rbind(rbind(educ.pc.train,educ.pc.val),educ.pc.test)

educ.pc.bopt <- b.star(educ.pc.bind["educ.pc.pred"][[1]],round=TRUE)[[1]]  # get optimal bootstrap lengths

educ.pc.boot <- tsbootstrap(educ.pc.bind["educ.pc.pred"][[1]], nb=1000, b= educ.pc.bopt, type="block")
educ.pc.sd <- matrixStats::rowSds(as.matrix(educ.pc.boot))

## Create time series data
setwd(code.directory)

ts.dat <- cbind(educ.pc.bind, educ.pc.sd)

## Plot time series 

ts.means <- ts.dat  %>%
  mutate(pointwise.educ.pc = educ.pc.Treated-educ.pc.pred,
         cumulative.educ.pc = cumsum(pointwise.educ.pc))

# cumulative.educ.pc = cumsum(pointwise.educ.pc),

ts.means <- ts.means[with(ts.means, order(year)), ] # sort by year

# ts.means$cumulative.educ.pc <- NA
# 
# for (i in 1:nrow(ts.means)){
#   ts.means$cumulative.educ.pc[i] <- rollmean(ts.means$pointwise.educ.pc,i, align='right')
# }

ts.means.m <- melt(as.data.frame(ts.means)[!colnames(ts.means) %in% c("educ.pc.sd")], id.var=c("year"))

# # Adjust year for plot
ts.means.m$year <- as.yearmon(ts.means.m$year, "%Y-%m",tz="UTC") # convert year to year class

ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")

# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="educ.pc.Treated" | ts.means.m$variable=="educ.pc.pred"] <- "Time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.educ.pc" ] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.educ.pc"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed educ.pc","Predicted educ.pc",
                                 "Pointwise educ.pc", "Cumulative educ.pc")

# SDs

sds <- ts.dat  %>%
  mutate(pred.educ.pc.min = educ.pc.pred - educ.pc.sd*1.96,
         pred.educ.pc.max = educ.pc.pred + educ.pc.sd*1.96,
         pointwise.educ.pc.min = educ.pc.Treated-pred.educ.pc.max,
         pointwise.educ.pc.max = educ.pc.Treated-pred.educ.pc.min,
         cumulative.educ.pc.min = cumsum(pointwise.educ.pc.min),
         cumulative.educ.pc.max = cumsum(pointwise.educ.pc.max))

sds <- sds[with(sds, order(year)), ] # sort by year

# for (i in 1:nrow(sds)){
#   sds$cumulative.educ.pc.min[i] <- rollmean(sds$pointwise.educ.pc.min,i, align='right')
# }
# for (i in 1:nrow(sds)){
#   sds$cumulative.educ.pc.max[i] <- rollmean(sds$pointwise.educ.pc.max,i, align='right')
# }

pred.vars <- c("educ.pc.pred", "educ.pc.sd", "pred.educ.pc.min", "pred.educ.pc.max", "pointwise.educ.pc.min", "pointwise.educ.pc.max", "cumulative.educ.pc.min", "cumulative.educ.pc.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

if(analysis=="analysis-01"){
  ts.plot <- TsPlotEd(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/ed-hsa-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-12"){
  ts.plot <- TsPlotEd(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/ed-sha-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-34"){
  ts.plot <- TsPlotEd(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/ed-89-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-41"){
  ts.plot <- TsPlotEd(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/ed-89-treat-west.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-01"){
  
  # Calculate avg. pointwise impact during intervention/post-period: >= 1862
  
  #educ.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise educ.pc" & (ts.means.m$year>="1862-12-31 19:03:58")]) 
  
  mean(ts.means.m$pointwise.educ.pc.min[(ts.means.m$year>="1862-12-31 19:03:58")]) 
  mean(ts.means.m$pointwise.educ.pc.max[(ts.means.m$year>="1862-12-31 19:03:58")]) 
  
  # Calculate avg. cumulative impact during intervention/post-period: >= 1862
  
  #educ.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative educ.pc" & (ts.means.m$year=="1941-12-31 19:00:00")] - ts.means.m$value[ts.means.m$variable=="Cumulative educ.pc" & (ts.means.m$year=="1862-12-31 19:03:58")]
  
  ts.means.m$cumulative.educ.pc.min[(ts.means.m$year=="1941-12-31 19:00:00")][[1]] - ts.means.m$cumulative.educ.pc.min[(ts.means.m$year=="1862-12-31 19:03:58")][[1]]
  ts.means.m$cumulative.educ.pc.max[(ts.means.m$year=="1941-12-31 19:00:00")][[1]] - ts.means.m$cumulative.educ.pc.max[(ts.means.m$year=="1862-12-31 19:03:58")][[1]]
  
  # Calculate MAPE
  
  mean(abs(educ.pc.val$educ.pc.Treated-educ.pc.val$educ.pc.pred)/((abs(educ.pc.val$educ.pc.Treated) + abs(educ.pc.val$educ.pc.pred))/2))*100 #SMAPE
}

if(analysis=="analysis-12"){
  
  # Calculate avg. pointwise impact during intervention period: >= 1866
  
  #educ.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise educ.pc" & (ts.means.m$year>="1866-12-31 19:03:58")])
  
  mean(ts.means.m$pointwise.educ.pc.min[(ts.means.m$year>="1866-12-31 19:03:58")])
  mean(ts.means.m$pointwise.educ.pc.max[(ts.means.m$year>="1866-12-31 19:03:58")])
  
  
  # Calculate avg. cumulative impact during intervention/post-period: >= 1866
  
  #educ.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative educ.pc" & (ts.means.m$year=="1941-12-31 19:00:00")] - ts.means.m$value[ts.means.m$variable=="Cumulative educ.pc" & (ts.means.m$year=="1866-12-31 19:03:58")]
  
  ts.means.m$cumulative.educ.pc.min[(ts.means.m$year=="1941-12-31 19:00:00")][[1]] - ts.means.m$cumulative.educ.pc.min[(ts.means.m$year=="1866-12-31 19:03:58")][[1]]
  ts.means.m$cumulative.educ.pc.max[(ts.means.m$year=="1941-12-31 19:00:00")][[1]] - ts.means.m$cumulative.educ.pc.max[(ts.means.m$year=="1866-12-31 19:03:58")][[1]]
  
  # Calculate MAPE
  mean(abs((educ.pc.val$educ.pc.Treated-educ.pc.val$educ.pc.pred)/educ.pc.val$educ.pc.Treated))*(100/3)
  
  
}

if(analysis=="analysis-34" | analysis=="analysis-41"){
  # Calculate avg. pointwise impact during intervention/post-period: >= 1889
  
  #educ.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise educ.pc" & (ts.means.m$year>="1889-12-31 19:00:00")])
  
  mean(ts.means.m$pointwise.educ.pc.min[(ts.means.m$year>="1889-12-31 19:00:00")])
  mean(ts.means.m$pointwise.educ.pc.max[(ts.means.m$year>="1889-12-31 19:00:00")])
  
  
  # Calculate avg. cumulative impact during intervention/post-period: >= 1889
  
  #educ.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative educ.pc" & (ts.means.m$year=="1941-12-31 19:00:00")] - ts.means.m$value[ts.means.m$variable=="Cumulative educ.pc" & (ts.means.m$year=="1889-12-31 19:00:00")]
  
  ts.means.m$cumulative.educ.pc.min[(ts.means.m$year=="1941-12-31 19:00:00")][[1]] - ts.means.m$cumulative.educ.pc.min[(ts.means.m$year=="1889-12-31 19:00:00")][[1]]
  ts.means.m$cumulative.educ.pc.max[(ts.means.m$year=="1941-12-31 19:00:00")][[1]] - ts.means.m$cumulative.educ.pc.max[(ts.means.m$year=="1889-12-31 19:00:00")][[1]]
  
  # Calculate MAPE
  mean(abs((educ.pc.val$educ.pc.Treated-educ.pc.val$educ.pc.pred)/educ.pc.val$educ.pc.Treated))*(100/3)
  
  
}