# Plot time-series and estimate causal impacts
# Uses train/test sets from capacity-state.R

source(paste0(code.directory,"ts-plot-ed.R"))

analysis <- "analysis-12"

type <- "treated"

## Education data
setwd(paste0(results.directory, "predictions/",analysis,"/",type,"/edpc")) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

ed.pc.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                header=FALSE,
                                col.names="ed.pc.pred"))
ed.pc.test.mean <-rowMeans(ed.pc.test.pred)
ed.pc.test.sd <- matrixStats::rowSds(as.matrix(ed.pc.test.pred))

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
ed.pc.train <- cbind(ed.pc.y.train, 
                          "ed.pc.mean"=ed.pc.train.mean, 
                          "ed.pc.sd"=ed.pc.train.sd) 


## Create time series data
setwd(code.directory)

ts.dat <- rbind(ed.pc.train,ed.pc.test)

## Plot time series 

time.vars <- c("year","ed.pc.Treated","ed.pc.mean")

ts.means <- ts.dat[time.vars]  %>%
  mutate(pointwise.ed.pc = ed.pc.Treated-ed.pc.mean,
         cumulative.ed.pc = cumsum(pointwise.ed.pc)) 

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
  mutate(pred.ed.pc.min = ed.pc.mean - ed.pc.sd,
         pred.ed.pc.max = ed.pc.mean + ed.pc.sd,
         pointwise.ed.pc.min = ed.pc.Treated-pred.ed.pc.min,
         pointwise.ed.pc.max = ed.pc.Treated-pred.ed.pc.max,
         cumulative.ed.pc.min = cumsum(pointwise.ed.pc.min),
         cumulative.ed.pc.max = cumsum(pointwise.ed.pc.max))

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
# Calculate Avg. pointwise impact during pre-period: < 1866

# ed.pc
mean(ts.means.m$value[ts.means.m$variable=="Pointwise ed.pc" & (ts.means.m$year<"1866-12-31 19:03:58")])

mean(sds$ed.pc.sd[(sds$year< 1866)])

# Calculate Avg. cumulative impact during pre-period:< 1866

#ed.pc
ts.means.m$value[ts.means.m$variable=="Cumulative ed.pc" & ts.means.m$year=="1865-12-31 19:03:58"]

(abs(sds$cumulative.ed.pc.min[(sds$year==1865)] -sds$cumulative.ed.pc.max[(sds$year==1865)])/2)

# Calculate avg. pointwise impact during intervention/post-period: >= 1866

#ed.pc
mean(ts.means.m$value[ts.means.m$variable=="Pointwise ed.pc" & (ts.means.m$year>="1866-12-31 19:03:58")])

mean(sds$ed.pc.sd[(sds$year>=1866 & sds$year<=1914)])

# Calculate cumulative impact during intervention/post-period: >= 1866

#ed.pc
ts.means.m$value[ts.means.m$variable=="Cumulative ed.pc" & ts.means.m$year=="1941-12-31 19:00:00"] - ts.means.m$value[ts.means.m$variable=="Cumulative ed.pc" & ts.means.m$year=="1866-12-31 19:03:58"]

abs((abs(sds$cumulative.ed.pc.min[(sds$year==1942)] -sds$cumulative.ed.pc.max[(sds$year==1942)])/2) -(abs(sds$cumulative.ed.pc.min[(sds$year==1866)] -sds$cumulative.ed.pc.max[(sds$year==1866)])/2))
}

if(analysis=="analysis-34"){
  # Calculate Avg. pointwise impact during pre-period: < 1889
  
  # ed.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise ed.pc" & (ts.means.m$year<"1889-12-31 19:00:00")])
  
  mean(sds$ed.pc.sd[(sds$year< 1889)])
  
  # Calculate Avg. cumulative impact during pre-period:< 1889
  
  #ed.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative ed.pc" & ts.means.m$year=="1888-12-31 19:00:00"]
  
  (abs(sds$cumulative.ed.pc.min[(sds$year==1888)] -sds$cumulative.ed.pc.max[(sds$year==1888)])/2)
  
  # Calculate avg. pointwise impact during intervention/post-period: >= 1889 & <= 1914
  
  #ed.pc
  mean(ts.means.m$value[ts.means.m$variable=="Pointwise ed.pc" & (ts.means.m$year>="1889-12-31 19:00:00")])
  
  mean(sds$ed.pc.sd[(sds$year>=1889)])
  
  # Calculate cumulative impact during intervention/post-period: >= 1889
  
  #ed.pc
  ts.means.m$value[ts.means.m$variable=="Cumulative ed.pc" & ts.means.m$year=="1941-12-31 19:00:0"] - ts.means.m$value[ts.means.m$variable=="Cumulative ed.pc" & ts.means.m$year=="1889-12-31 19:00:00"]
  
  abs((abs(sds$cumulative.ed.pc.min[(sds$year==1942)] -sds$cumulative.ed.pc.max[(sds$year==1942)])/2) -(abs(sds$cumulative.ed.pc.min[(sds$year==1889)] -sds$cumulative.ed.pc.max[(sds$year==1889)])/2))
}