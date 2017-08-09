# Plot time-series and estimate causal impacts
# Uses train/test sets from patents.R

source("ts-plot-sales.R")

analysis <- "analysis-12"

type <- "control"

## Sales data
setwd(paste0(results.directory, "predictions/",analysis,"/",type)) # prediction files loc

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

## Create time series data
setwd(code.directory)

ts.dat <- rbind(sales.train,sales.test)

## Plot time series 

if (type=='treated') {
time.vars <- c("date","sales.Treated","sales.mean")

ts.means <- ts.dat[time.vars]  %>%
  mutate(pointwise.sales = sales.Treated-sales.mean,
         cumulative.sales = cumsum(pointwise.sales)) 
}

if (type=='control') {
  time.vars <- c("date","sales.Control","sales.mean")
  
  ts.means <- ts.dat[time.vars]  %>%
    mutate(pointwise.sales = sales.Control-sales.mean,
           cumulative.sales = cumsum(pointwise.sales)) 
}

ts.means.m <- melt(as.data.frame(ts.means), id.var=c("date"))

# Adjust date for plot
ts.means.m$date <- as.yearmon(ts.means.m$date, "%Y-%m",tz="UTC") # convert date to date class

ts.means.m$date <- as.POSIXct(ts.means.m$date, tz="UTC")
                         
# Labels

ts.means.m$series <- NA
if (type=='treated') {
  ts.means.m$series[ts.means.m$variable=="sales.Treated" | ts.means.m$variable=="sales.mean"] <- "Time-series"
}
if (type=='control') {
  ts.means.m$series[ts.means.m$variable=="sales.Control" | ts.means.m$variable=="sales.mean"] <- "Time-series"
}
ts.means.m$series[ts.means.m$variable=="pointwise.sales" ] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.sales" ] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed sales","Predicted sales",
                                 "Pointwise sales", "Cumulative sales")

# SDs

if (type=='treated') {
  sds <- ts.dat  %>%
  mutate(pred.sales.min = sales.mean - sales.sd,
         pred.sales.max = sales.mean + sales.sd,
         pointwise.sales.min = sales.Treated-pred.sales.min,
         pointwise.sales.max = sales.Treated-pred.sales.max,
         cumulative.sales.min = cumsum(pointwise.sales.min),
         cumulative.sales.max = cumsum(pointwise.sales.max))
}

if (type=='control') {
  sds <- ts.dat  %>%
    mutate(pred.sales.min = sales.mean - sales.sd,
           pred.sales.max = sales.mean + sales.sd,
           pointwise.sales.min = sales.Control-pred.sales.min,
           pointwise.sales.max = sales.Control-pred.sales.max,
           cumulative.sales.min = cumsum(pointwise.sales.min),
           cumulative.sales.max = cumsum(pointwise.sales.max))
}

pred.vars <- c("sales.mean", "sales.sd", "pred.sales.min", "pred.sales.max", "pointwise.sales.min", "pointwise.sales.max", "cumulative.sales.min", "cumulative.sales.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

# Plot
ts.plot <- TsPlotSales(ts.means.m) 

if (type=='treated') {
  ggsave(paste0(results.directory,"plots/sales-south-treat.png"), ts.plot, width=11, height=8.5)
}

if (type=='control') {
  ggsave(paste0(results.directory,"plots/sales-south-treat.png"), ts.plot, width=11, height=8.5)
}
# Calculate Avg. pointwise impact during pre-period: <= "May 1866"

mean(ts.means.m$value[ts.means.m$variable=="Pointwise sales" & (ts.means.m$date<="1886-05-01 19:00:00")])

mean(sds$sales.sd[(sds$date<= "May 1866")])

# Calculate Avg. cumulative impact during pre-period: <= "May 1866"

ts.means.m$value[ts.means.m$variable=="Cumulative sales" & ts.means.m$date=="1866-05-31 19:03:58"]

(abs(sds$cumulative.sales.min[(sds$date=="May 1866")] -sds$cumulative.sales.max[(sds$date=="May 1866")])/2)

# Calculate avg. pointwise impact during intervention/post-period:  >= "Jun 1866" & <= "Feb 1889"

mean(ts.means.m$value[ts.means.m$variable=="Pointwise sales" & (ts.means.m$date>="1876-06-01 19:00:00")])

mean(sds$sales.sd[(sds$date>="Jun 1876")])

# Calculate cumulative impact during intervention/post-period:  >= "Jun 1866" & <= "Feb 1889"

ts.means.m$value[ts.means.m$variable=="Cumulative sales" & ts.means.m$date=="1889-01-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative sales" & ts.means.m$date=="1866-05-31 19:03:58"]

abs((abs(sds$cumulative.sales.min[(sds$date=="Jun 1866")] -sds$cumulative.sales.max[(sds$date=="Jun 1866")])/2) -(abs(sds$cumulative.sales.min[(sds$date=="Feb 1889")] -sds$cumulative.sales.max[(sds$date=="Feb 1889")])/2))