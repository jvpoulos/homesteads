# Plot time-series and estimate causal impacts
# Uses train/test sets from patents.R

source(paste0(code.directory,"ts-plot-patents.R"))

analysis <- "analysis-34"

type <- "treated"

## Sales data
setwd(paste0(results.directory, "predictions/",analysis,"/",type,"/sales")) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

sales.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                header=FALSE,
                                col.names="sales.pred"))
sales.test.mean <-rowMeans(sales.test.pred)
sales.test.sd <- matrixStats::rowSds(as.matrix(sales.test.pred))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

sales.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                        header=FALSE,
                                        col.names="sales.pred"))
sales.train.mean <- rowMeans(as.matrix(sales.train.pred))
sales.train.sd <- matrixStats::rowSds(as.matrix(sales.train.pred))


# Bind to splits
sales.test <- cbind(sales.y.test, 
                         "sales.mean"= sales.test.mean, 
                         "sales.sd"= sales.test.sd) 
sales.train <- cbind(sales.y.train, 
                          "sales.mean"=sales.train.mean, 
                          "sales.sd"=sales.train.sd) 

## Homesteads data 
setwd(paste0(results.directory, "predictions/",analysis,"/",type,"/homesteads")) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

homesteads.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                             header=FALSE,
                                             col.names="homesteads.pred"))
homesteads.test.mean <-rowMeans(homesteads.test.pred)
homesteads.test.sd <- matrixStats::rowSds(as.matrix(homesteads.test.pred))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

homesteads.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                              header=FALSE,
                                              col.names="homesteads.pred"))
homesteads.train.mean <- rowMeans(as.matrix(homesteads.train.pred))
homesteads.train.sd <- matrixStats::rowSds(as.matrix(homesteads.train.pred))


# Bind to splits
homesteads.test <- cbind(homesteads.y.test, 
                         "homesteads.mean"= homesteads.test.mean, 
                         "homesteads.sd"= homesteads.test.sd) 
homesteads.train <- cbind(homesteads.y.train, 
                          "homesteads.mean"=homesteads.train.mean, 
                          "homesteads.sd"=homesteads.train.sd) 

## Create time series data
setwd(code.directory)

ts.dat <- merge(rbind(sales.train,sales.test), rbind(homesteads.train,homesteads.test), by="date")

## Plot time series 

time.vars <- c("date","sales.Treated","sales.mean","homesteads.Treated", "homesteads.mean")

ts.means <- ts.dat[time.vars]  %>%
  mutate(pointwise.sales = sales.Treated-sales.mean,
         cumulative.sales = cumsum(pointwise.sales),
         pointwise.homesteads = homesteads.Treated-homesteads.mean,
         cumulative.homesteads = cumsum(pointwise.homesteads)) 

ts.means.m <- melt(as.data.frame(ts.means), id.var=c("date"))

# Adjust date for plot
ts.means.m$date <- as.yearmon(ts.means.m$date, "%Y-%m",tz="UTC") # convert date to date class

ts.means.m$date <- as.POSIXct(ts.means.m$date, tz="UTC")
                         
# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="sales.Treated" | ts.means.m$variable=="sales.mean" | ts.means.m$variable=="homesteads.Treated" | ts.means.m$variable=="homesteads.mean"] <- "Time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.sales" | ts.means.m$variable=="pointwise.homesteads"] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.sales" | ts.means.m$variable=="cumulative.homesteads"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed sales","Predicted sales", "Observed homesteads", "Predicted homesteads",
                                 "Pointwise sales", "Cumulative sales", 
                                 "Pointwise homesteads", "Cumulative homesteads")

# SDs

sds <- ts.dat  %>%
  mutate(pred.sales.min = sales.mean - sales.sd*1.96,
         pred.sales.max = sales.mean + sales.sd*1.96,
         pointwise.sales.min = sales.Treated-pred.sales.min,
         pointwise.sales.max = sales.Treated-pred.sales.max,
         cumulative.sales.min = cumsum(pointwise.sales.min),
         cumulative.sales.max = cumsum(pointwise.sales.max),
         pred.homesteads.min = homesteads.mean - homesteads.sd*1.96,
         pred.homesteads.max = homesteads.mean + homesteads.sd*1.96,
         pointwise.homesteads.min = homesteads.Treated-pred.homesteads.min,
         pointwise.homesteads.max = homesteads.Treated-pred.homesteads.max,
         cumulative.homesteads.min = cumsum(pointwise.homesteads.min),
         cumulative.homesteads.max = cumsum(pointwise.homesteads.max))

pred.vars <- c("sales.mean", "sales.sd", "pred.sales.min", "pred.sales.max", "pointwise.sales.min", "pointwise.sales.max", "cumulative.sales.min", "cumulative.sales.max",
               "homesteads.mean", "homesteads.sd", "pred.homesteads.min", "pred.homesteads.max", "pointwise.homesteads.min", "pointwise.homesteads.max", "cumulative.homesteads.min", "cumulative.homesteads.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

# Plot
ts.plot <- TsPlotPatents(ts.means.m)

ggsave(paste0(results.directory,"plots/patents-south-treat.png"), ts.plot, width=11, height=8.5)

# Calculate Avg. pointwise impact during pre-period: <= "Feb 1889"

# sales
sales.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise sales" & (ts.means.m$date<="1889-02-28 19:00:00")])
sales.mu

sales.mu - (mean(sds$sales.sd[(sds$date<= "Feb 1889")])*1.96)
sales.mu + (mean(sds$sales.sd[(sds$date<= "Feb 1889")])*1.96)

# homesteads
homesteads.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise homesteads" & (ts.means.m$date<="1889-02-28 19:00:00")])
homesteads.mu

homesteads.mu - (mean(sds$homesteads.sd[(sds$date<= "Feb 1889")])*1.96)
homesteads.mu + (mean(sds$homesteads.sd[(sds$date<= "Feb 1889")])*1.96)

# Calculate Avg. cumulative impact during pre-period: <= "Feb 1889"

#sales
ts.means.m$value[ts.means.m$variable=="Cumulative sales" & ts.means.m$date=="1889-02-28 19:00:00"]

sds$cumulative.sales.max[(sds$date=="Feb 1889")]
sds$cumulative.sales.min[(sds$date=="Feb 1889")]

#homesteads
ts.means.m$value[ts.means.m$variable=="Cumulative homesteads" & ts.means.m$date=="1889-02-28 19:00:00"]

sds$cumulative.homesteads.max[(sds$date=="Feb 1889")]
sds$cumulative.homesteads.min[(sds$date=="Feb 1889")]


# Calculate avg. pointwise impact during intervention/post-period: >= "Mar 1889"

#sales
sales.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise sales" & (ts.means.m$date>="1889-03-01 19:00:00")])
sales.mu

sales.mu - (mean(sds$sales.sd[(sds$date>="Mar 1889")])*1.96)
sales.mu + (mean(sds$sales.sd[(sds$date>="Mar 1889")])*1.96)

#homesteads
homesteads.mu <- mean(ts.means.m$value[ts.means.m$variable=="Pointwise homesteads" & (ts.means.m$date>="1889-03-01 19:00:00")])
homesteads.mu

homesteads.mu - (mean(sds$homesteads.sd[(sds$date>="Mar 1889")])*1.96)
homesteads.mu + (mean(sds$homesteads.sd[(sds$date>="Mar 1889")])*1.96)

# Calculate cumulative impact during intervention/post-period: >= "Mar 1889"

#sales
ts.means.m$value[ts.means.m$variable=="Cumulative sales" & ts.means.m$date=="1976-09-30 20:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative sales" & ts.means.m$date=="1889-02-28 19:00:00"]

sds$cumulative.sales.max[(sds$date=="Oct 1976")] -sds$cumulative.sales.max[(sds$date=="Mar 1889")]
sds$cumulative.sales.min[(sds$date=="Oct 1976")] -sds$cumulative.sales.min[(sds$date=="Mar 1889")]

#homesteads
ts.means.m$value[ts.means.m$variable=="Cumulative homesteads" & ts.means.m$date=="1976-09-30 20:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative homesteads" & ts.means.m$date=="1889-02-28 19:00:00"]

sds$cumulative.homesteads.max[(sds$date=="Oct 1976")] -sds$cumulative.homesteads.max[(sds$date=="Mar 1889")]
sds$cumulative.homesteads.min[(sds$date=="Oct 1976")] -sds$cumulative.homesteads.min[(sds$date=="Mar 1889")]