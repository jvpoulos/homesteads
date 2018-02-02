###################################
#  GBR Regression                 #
###################################

library(boot)
library(tidyr)
library(zoo)
library(readxl)
library(dplyr)
library(plm)
library(ggplot2)

# Two-stage estimates

source(paste0(homestead.code.directory,"Run2Stage.R"))

## Southern public land states

gbr.south <- homestead.rr.wide[homestead.rr.wide$state.abb %in% southern.pub,]

# Test: 1890

# rr
f1 <- formula(year2rr1890 ~ year2rr1880 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1890 ~ year2x1880)

rr.90 <- boot(data=gbr.south,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.90.delta <- rr.90$t0[['delta']]
rr.90.delta

rr.90.CI <- boot.ci(rr.90, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.90.CI

# Test: 1900

# rr
f1 <- formula(year2rr1900 ~ year2rr1890 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1900 ~ year2x1890)

rr.00 <- boot(data=gbr.south,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.00.delta <- rr.00$t0[['delta']]
rr.00.delta

rr.00.CI <- boot.ci(rr.00, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.00.CI

# Test: 1910

# rr
f1 <- formula(year2rr1910 ~ year2rr1900 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1910 ~ year2x1900)

rr.10 <- boot(data=gbr.south,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.10.delta <- rr.10$t0[['delta']]
rr.10.delta

rr.10.CI <- boot.ci(rr.10, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.10.CI

# Test: 1920

# rr
f1 <- formula(year2rr1920 ~ year2rr1910 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1920 ~ year2x1880)

rr.20 <- boot(data=gbr.south,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 parallel="multicore", ncpus = cores)

rr.20.delta <- rr.20$t0[['delta']]
rr.20.delta

rr.20.CI <- boot.ci(rr.20, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.20.CI

# Test: pooled

gbr.south.pooled <- homestead.rr.long[homestead.rr.long$state.abb %in% southern.pub,]

# Create lags
TLag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}

gbr.south.pooled <- gbr.south.pooled %>% 
  group_by(state.abb,county) %>% 
  mutate(access.lag = TLag(access.mean, 10, time = year),
         access.1870 = access.mean[year==1870])

#rr
f1 <- formula(access.mean ~ access.lag + delta, weights=1/access.1870)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

rr.south.pooled <- boot(data=gbr.south.pooled,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      R=1000,
                      parallel="multicore", ncpus = cores)

rr.south.pooled.delta <- rr.south.pooled$t0[['delta']]
rr.south.pooled.delta

rr.south.pooled.CI <- boot.ci(rr.south.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
rr.south.pooled.CI

## Western public land states

gbr.west <- homestead.rr.wide[homestead.rr.wide$state.abb %in% western.pub,]

# Test: 1880

# rr
f1 <- formula(year2rr1880 ~ year2rr1870 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1880 ~ year2x1870)

rr.80.west <- boot(data=gbr.west,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.80.west.delta <- rr.80.west$t0[['delta']]
rr.80.west.delta

rr.80.west.CI <- boot.ci(rr.80.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.80.west.CI

# Test: 1890

# rr
f1 <- formula(year2rr1890 ~ year2rr1880 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1890 ~ year2x1880)

rr.90.west <- boot(data=gbr.west,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.90.west.delta <- rr.90.west$t0[['delta']]
rr.90.west.delta

rr.90.west.CI <- boot.ci(rr.90.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.90.west.CI

# Test: 1900

# rr
f1 <- formula(year2rr1900 ~ year2rr1890 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1900 ~ year2x1890)

rr.00.west <- boot(data=gbr.west,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.00.west.delta <- rr.00.west$t0[['delta']]
rr.00.west.delta

rr.00.west.CI <- boot.ci(rr.00.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.00.west.CI

# Test: 1910

# rr
f1 <- formula(year2rr1910 ~ year2rr1900 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1910 ~ year2x1900)

rr.10.west <- boot(data=gbr.west,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.10.west.delta <- rr.10.west$t0[['delta']]
rr.10.west.delta

rr.10.west.CI <- boot.ci(rr.10.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.10.west.CI

# Test: 1920

# rr
f1 <- formula(year2rr1920 ~ year2rr1910 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1920 ~ year2x1880)

rr.20.west <- boot(data=gbr.west,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.20.west.delta <- rr.20.west$t0[['delta']]
rr.20.west.delta

rr.20.west.CI <- boot.ci(rr.20.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.20.west.CI

# Test: pooled

gbr.west.pooled <- homestead.rr.long[homestead.rr.long$state.abb %in% western.pub,]

gbr.west.pooled <- merge(gbr.west.pooled, gbr.west[c("state.abb", "county", "year2rr1870")],
                         by=c("state.abb","county")) 

# Create lags
TLag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}

gbr.west.pooled <- gbr.west.pooled %>% 
  group_by(fips) %>% 
  mutate(access.lag = TLag(access.mean, 10, time = year))

#rr
f1 <- formula(access.mean ~ access.lag + delta, weights=1/year2rr1870)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

rr.west.pooled <- boot(data=gbr.west.pooled,
                        statistic=Run2Stage,
                        f1=f1, f2=f2,
                        R=1000,
                        parallel="multicore", ncpus = cores)

rr.west.pooled.delta <- rr.west.pooled$t0[['delta']]
rr.west.pooled.delta

rr.west.pooled.CI <- boot.ci(rr.west.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
rr.west.pooled.CI

## All public land states

gbr.all <- homestead.rr.wide

# Test: 1880

# rr
f1 <- formula(year2rr1880 ~ year2rr1870 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1880 ~ year2x1870)

rr.80.all <- boot(data=gbr.all,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.80.all.delta <- rr.80.all$t0[['delta']]
rr.80.all.delta

rr.80.all.CI <- boot.ci(rr.80, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.80.all.CI

# Test: 1890

# rr
f1 <- formula(year2rr1890 ~ year2rr1880 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1890 ~ year2x1880)

rr.90.all <- boot(data=gbr.all,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.90.all.delta <- rr.90.all$t0[['delta']]
rr.90.all.delta

rr.90.all.CI <- boot.ci(rr.90.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.90.all.CI

# Test: 1900

# rr
f1 <- formula(year2rr1900 ~ year2rr1890 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1900 ~ year2x1890)

rr.00.all <- boot(data=gbr.all,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.00.all.delta <- rr.00.all$t0[['delta']]
rr.00.all.delta

rr.00.all.CI <- boot.ci(rr.00.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.00.all.CI

# Test: 1910

# rr
f1 <- formula(year2rr1910 ~ year2rr1900 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1910 ~ year2x1900)

rr.10.all <- boot(data=gbr.all,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.10.all.delta <- rr.10.all$t0[['delta']]
rr.10.all.delta

rr.10.all.CI <- boot.ci(rr.10.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.10.all.CI

# Test: 1920

# rr
f1 <- formula(year2rr1920 ~ year2rr1910 + delta, weights=1/year2rr1870)
f2 <- formula(year2x1920 ~ year2x1880)

rr.20.all <- boot(data=gbr.all,
              statistic=Run2Stage,
              f1=f1, f2=f2,
              R=1000,
              parallel="multicore", ncpus = cores)

rr.20.all.delta <- rr.20.all$t0[['delta']]
rr.20.all.delta

rr.20.all.CI <- boot.ci(rr.20.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.20.all.CI

# Test: pooled

gbr.all.pooled <- homestead.rr.long[homestead.rr.long$state.abb %in% pub.states,]

gbr.all.pooled <- merge(gbr.all.pooled, gbr.all[c("state.abb", "county", "year2rr1870")],
                         by=c("state.abb","county")) 

# Create lags
TLag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}

gbr.all.pooled <- gbr.all.pooled %>% 
  group_by(fips) %>% 
  mutate(access.lag = TLag(access.mean, 10, time = year))

#rr
f1 <- formula(access.mean ~ access.lag + delta, weights=1/year2rr1870)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

rr.all.pooled <- boot(data=gbr.all.pooled,
                       statistic=Run2Stage,
                       f1=f1, f2=f2,
                       R=1000,
                       parallel="multicore", ncpus = cores)

rr.all.pooled.delta <- rr.all.pooled$t0[['delta']]
rr.all.pooled.delta

rr.all.pooled.CI <- boot.ci(rr.all.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
rr.all.pooled.CI

## Plot results

ForestPlot2 <- function(d, xlab, ylab, title="", leglab1, ylim=NULL){
  p <- ggplot(d, aes(x=x, y = y, ymin=y.lo, ymax=y.hi,colour=region)) + 
    geom_pointrange(size=1, alpha=0.6) + 
    #  coord_flip() +
    geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0), colour="black", lty=2) +
  #  scale_y_continuous(labels = scales::percent) +
    labs(colour = leglab1) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(ylim=ylim) +
    ylab(ylab) +
    xlab(xlab) #switch because of the coord_flip() above
  return(p)
}

# Create data for plot 
test.years <- seq(1880,1920,10)

plot.rr.year.gbr <- data.frame(region= rep(c("South","West", "All"),each=length(test.years), times=2),
                             y = c(NA, rr.90.delta, rr.00.delta, rr.20.delta, rr.20.delta,
                                   rr.80.west.delta, rr.90.west.delta, rr.00.west.delta, rr.20.west.delta, rr.20.west.delta,
                                   rr.80.all.delta, rr.90.all.delta, rr.00.all.delta, rr.10.all.delta, rr.20.all.delta),
                             y.lo = c(NA, rr.90.CI[1], rr.00.CI[1], rr.20.CI[1], rr.20.CI[1],
                                      rr.80.west.CI[1], rr.90.west.CI[1], rr.00.west.CI[1], rr.20.west.CI[1], rr.20.west.CI[1],
                                      rr.80.all.CI[1], rr.90.all.CI[1], rr.00.all.CI[1], rr.10.all.CI[1], rr.20.all.CI[1]),
                             y.hi = c(NA, rr.90.CI[2], rr.00.CI[2], rr.20.CI[2], rr.20.CI[2],
                                      rr.80.west.CI[2], rr.90.west.CI[2], rr.00.west.CI[2], rr.20.west.CI[2], rr.20.west.CI[2],
                                      rr.80.all.CI[2], rr.90.all.CI[2], rr.00.all.CI[2], rr.10.all.CI[2], rr.20.all.CI[2]))

plot.rr.year.gbr$x <- rep(test.years,3)

plot.rr.year.gbr <- plot.rr.year.gbr[rowSums(is.na(plot.rr.year.gbr[c("y", "y.lo", "y.hi")]))!=ncol(plot.rr.year.gbr[c("y", "y.lo", "y.hi")]), ] #rm NA years

# Plot forest plots
plot.rr.year.gbr$x <- as.factor(plot.rr.year.gbr$x) 
rr.plot.year.gbr <- ForestPlot2(plot.rr.year.gbr,ylab="Estimated effect of lagged per-capita homesteads",xlab="",title="GBR: Railroad access",leglab1="Region")

ggsave(paste0(results.directory,"gbr-rr.png"), rr.plot.year.gbr, width=11, height=8.5)  
