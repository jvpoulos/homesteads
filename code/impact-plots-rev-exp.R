# Plot time-series and estimate causal impacts
# Uses train/test sets from capacity-state.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)

analysis <- "analysis-12"

type <- "treated"

source(paste0(code.directory,"ts-plot-rev-exp.R"))
source(paste0(code.directory,"PolitisWhite.R"))

## Revenues data
setwd(paste0(results.directory, "predictions/","/revpc/",analysis,"/",type)) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

rev.pc.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                header=FALSE,
                                col.names="rev.pc.pred"))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

rev.pc.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                        header=FALSE,
                                        col.names="rev.pc.pred"))

# Import validation fit

val.files <- list.files(pattern = "*val.csv")

rev.pc.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                          header=FALSE,
                                          col.names="rev.pc.pred"))


# Bind to splits
rev.pc.test <- cbind(rev.pc.y.test, rev.pc.test.pred) 

rev.pc.val <- cbind(rev.pc.y.val, rev.pc.val.pred) 

rev.pc.train <- cbind(rev.pc.y.train, rev.pc.train.pred) 

# Bootstrap estimate for prediction

rev.pc.bind <- rbind(rbind(rev.pc.train,rev.pc.val),rev.pc.test)

rev.pc.bopt <- b.star(rev.pc.bind["rev.pc.pred"][[1]],round=TRUE)[[1]]  # get optimal bootstrap lengths

rev.pc.boot <- tsbootstrap(rev.pc.bind["rev.pc.pred"][[1]], nb=1000, b= rev.pc.bopt, type="stationary")  # stationary bootstrap with mean block length b according to Politis and Romano (1994)
rev.pc.sd <- matrixStats::rowSds(as.matrix(rev.pc.boot))

## Expenditures data 
setwd(paste0(results.directory, "predictions/","/exppc/",analysis,"/",type)) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

exp.pc.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                             header=FALSE,
                                             col.names="exp.pc.pred"))

# Import val results

val.files <- list.files(pattern = "*val.csv")

exp.pc.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                         header=FALSE,
                                         col.names="exp.pc.pred"))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

exp.pc.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                              header=FALSE,
                                              col.names="exp.pc.pred"))

# Bind to splits
exp.pc.test <- cbind(exp.pc.y.test, exp.pc.test.pred) 

exp.pc.val <- cbind(exp.pc.y.val, exp.pc.val.pred) 

exp.pc.train <- cbind(exp.pc.y.train, exp.pc.train.pred) 

# Bootstrap estimate for prediction

exp.pc.bind <- rbind(rbind(exp.pc.train,exp.pc.test),exp.pc.val)

exp.pc.bopt <- b.star(exp.pc.bind["exp.pc.pred"][[1]],round=TRUE)[[1]]  # get optimal bootstrap lengths

exp.pc.boot <- tsbootstrap(exp.pc.bind["exp.pc.pred"][[1]], nb=1000, b=exp.pc.bopt, type="stationary") # stationary bootstrap with mean block length b according to Politis and Romano (1994)
exp.pc.sd <- matrixStats::rowSds(as.matrix(exp.pc.boot))

## Create time series data
setwd(code.directory)

ts.dat <- merge(cbind(rev.pc.bind, rev.pc.sd), cbind(exp.pc.bind, exp.pc.sd), by="year")

## Plot time series 

ts.means <- ts.dat  %>%
  mutate(pointwise.rev.pc = rev.pc.Treated-rev.pc.pred,
         pointwise.exp.pc = exp.pc.Treated-exp.pc.pred,
         cumulative.rev.pc = NA,
         cumulative.exp.pc = NA)
         
         # cumulative.rev.pc = cumsum(pointwise.rev.pc),
         # cumulative.exp.pc = cumsum(pointwise.exp.pc)) 
         
ts.means <- ts.means[with(ts.means, order(year)), ] # sort by year
         
ts.means$cumulative.rev.pc <- NA
ts.means$cumulative.exp.pc <- NA
         
for (i in 1:nrow(ts.means)){
  ts.means$cumulative.rev.pc[i] <- rollmean(ts.means$pointwise.rev.pc,i, align='right')
}
         
for (i in 1:nrow(ts.means)){
  ts.means$cumulative.exp.pc[i] <- rollmean(ts.means$pointwise.exp.pc,i, align='right')
}         

ts.means.m <- melt(as.data.frame(ts.means)[!colnames(ts.means) %in% c("rev.pc.sd" , "exp.pc.sd" )], id.var=c("year"))

# # Adjust year for plot
ts.means.m$year <- as.yearmon(ts.means.m$year, "%Y-%m",tz="UTC") # convert year to year class
 
ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")
                         
# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="rev.pc.Treated" | ts.means.m$variable=="rev.pc.pred" | ts.means.m$variable=="exp.pc.Treated" | ts.means.m$variable=="exp.pc.pred"] <- "Time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.rev.pc" | ts.means.m$variable=="pointwise.exp.pc"] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.rev.pc" | ts.means.m$variable=="cumulative.exp.pc"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed rev.pc","Predicted rev.pc", "Observed exp.pc", "Predicted exp.pc",
                                 "Pointwise rev.pc", "Pointwise exp.pc", "Cumulative rev.pc",  "Cumulative exp.pc")

# SDs

sds <- ts.dat  %>%
  mutate(pred.rev.pc.min = rev.pc.pred - rev.pc.sd*1.96,
         pred.rev.pc.max = rev.pc.pred + rev.pc.sd*1.96,
         pointwise.rev.pc.min = rev.pc.Treated-pred.rev.pc.max,
         pointwise.rev.pc.max = rev.pc.Treated-pred.rev.pc.min,
         cumulative.rev.pc.min = cumsum(pointwise.rev.pc.min),
         cumulative.rev.pc.max = cumsum(pointwise.rev.pc.max),
         pred.exp.pc.min = exp.pc.pred - exp.pc.sd*1.96,
         pred.exp.pc.max = exp.pc.pred + exp.pc.sd*1.96,
         pointwise.exp.pc.min = exp.pc.Treated-pred.exp.pc.max,
         pointwise.exp.pc.max = exp.pc.Treated-pred.exp.pc.min,
         cumulative.exp.pc.min = NA,
         cumulative.exp.pc.max = NA)
         # cumulative.exp.pc.min = cumsum(pointwise.exp.pc.min),
         # cumulative.exp.pc.max = cumsum(pointwise.exp.pc.max))

sds <- sds[with(sds, order(year)), ] # sort by year

for (i in 1:nrow(sds)){
  sds$cumulative.rev.pc.min[i] <- rollmean(sds$pointwise.rev.pc.min,i, align='right')
}
for (i in 1:nrow(sds)){
  sds$cumulative.rev.pc.max[i] <- rollmean(sds$pointwise.rev.pc.max,i, align='right')
}

for (i in 1:nrow(sds)){
  sds$cumulative.exp.pc.min[i] <- rollmean(sds$pointwise.exp.pc.min,i, align='right')
}
for (i in 1:nrow(sds)){
  sds$cumulative.exp.pc.max[i] <- rollmean(sds$pointwise.exp.pc.max,i, align='right')
}

pred.vars <- c("rev.pc.pred", "rev.pc.sd", "pred.rev.pc.min", "pred.rev.pc.max", "pointwise.rev.pc.min", "pointwise.rev.pc.max", "cumulative.rev.pc.min", "cumulative.rev.pc.max",
               "exp.pc.pred", "exp.pc.sd", "pred.exp.pc.min", "pred.exp.pc.max", "pointwise.exp.pc.min", "pointwise.exp.pc.max", "cumulative.exp.pc.min", "cumulative.exp.pc.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

if(analysis=="analysis-12"){
  ts.plot <- TsPlotRevExp(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/rev-exp-south-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-34"){
  ts.plot <- TsPlotRevExp(ts.means.m[ts.means.m$series != "Cumulative impact",], analysis)
  ggsave(paste0(results.directory,"plots/rev-exp-public-treat.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="analysis-12"){
# Calculate avg. pointwise impact during intervention/post-period: >= 1866 & <= 1928

#rev.pc
mean(ts.means.m$value[ts.means.m$variable=="Pointwise rev.pc" & (ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])

mean(ts.means.m$pointwise.rev.pc.min[(ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])
mean(ts.means.m$pointwise.rev.pc.max[(ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])

#exp.pc
mean(ts.means.m$value[ts.means.m$variable=="Pointwise exp.pc" & (ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])

mean(ts.means.m$pointwise.exp.pc.min[(ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])
mean(ts.means.m$pointwise.exp.pc.max[(ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])

# Calculate cumulative impact during intervention/post-period: >= 1866 & <= 1928

#rev.pc
ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1914-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1866-12-31 19:03:58"]

mean(ts.means.m$cumulative.rev.pc.min[(ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])
mean(ts.means.m$cumulative.rev.pc.max[(ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])

#exp.pc
ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1914-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1866-12-31 19:03:58"]

mean(ts.means.m$cumulative.exp.pc.min[(ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])
mean(ts.means.m$cumulative.exp.pc.max[(ts.means.m$year>="1866-12-31 19:03:58" & ts.means.m$year <= "1914-12-31 19:00:00")])

}

if(analysis=="analysis-34"){
  # Calculate avg. pointwise impact during intervention/post-period: >= 1889 & <= 1928
  
  #rev.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise rev.pc" & (ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year <= "1916-12-31 19:00:00")])
  
  mean(ts.means.m$pointwise.rev.pc.min[(ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year <= "1916-12-31 19:00:00")])
  mean(ts.means.m$pointwise.rev.pc.max[(ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year <= "1916-12-31 19:00:00")])
  
  #exp.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise exp.pc" & (ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year <= "1916-12-31 19:00:00")])
  
  mean(ts.means.m$pointwise.exp.pc.min[(ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year <= "1916-12-31 19:00:00")])
  mean(ts.means.m$pointwise.exp.pc.max[(ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year <= "1916-12-31 19:00:00")])
  
  # Calculate cumulative impact during intervention/post-period: >= 1889 & <= 1928
  
  #rev.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1916-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative rev.pc" & ts.means.m$year=="1889-12-31 19:00:00"]
  
  mean(ts.means.m$cumulative.rev.pc.min[(ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year <= "1916-12-31 19:00:00")])
  mean(ts.means.m$cumulative.rev.pc.max[(ts.means.m$year>="1889-12-31 19:00:00" & ts.means.m$year <= "1916-12-31 19:00:00")])
  
  #exp.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1916-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative exp.pc" & ts.means.m$year=="1889-12-31 19:00:00"]
  
  sds$cumulative.exp.pc.max[(sds$year==1916)] - sds$cumulative.exp.pc.max[(sds$year==1889)]
  sds$cumulative.exp.pc.min[(sds$year==1916)] - sds$cumulative.exp.pc.min[(sds$year==1889)]
  
}