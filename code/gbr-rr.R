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
f1 <- formula(year2y1880 ~ year2y1870 + delta, weights=1/year2y1870)
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

# tax2
f1 <- formula(year3y1880 ~ year3y1870 + delta, weights=1/year3y1870)
f2 <- formula(year2x1880 ~ year2x1870)

tax2.80.west <- boot(data=gbr.west,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     parallel="multicore", ncpus = cores)

tax2.80.west.delta <- tax2.80.west$t0[['delta']]
tax2.80.west.delta

tax2.80.west.CI <- boot.ci(tax2.80.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.80.west.CI

# Test: 1920

# rr
f1 <- formula(year2y1920 ~ year2y1880 + delta, weights=1/year2y1870)
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

# Test: 1930

# rr
f1 <- formula(year2y1930 ~ year2y1920 + delta, weights=1/year2y1870)
f2 <- formula(year2x1930 ~ year2x1920)

rr.30.west <- boot(data=gbr.west,
                statistic=Run2Stage,
                f1=f1, f2=f2,
                R=1000,
                parallel="multicore", ncpus = cores)

rr.30.west.delta <- rr.30.west$t0[['delta']]
rr.30.west.delta

rr.30.west.CI <- boot.ci(rr.30.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.30.west.CI

# tax2
f1 <- formula(year3y1930 ~ year3y1880 + delta, weights=1/year3y1870)
f2 <- formula(year2x1930 ~ year2x1880)

tax2.30.west <- boot(data=gbr.west,
                statistic=Run2Stage,
                f1=f1, f2=f2,
                R=1000,
                parallel="multicore", ncpus = cores)

tax2.30.west.delta <- tax2.30.west$t0[['delta']]
tax2.30.west.delta

tax2.30.west.CI <- boot.ci(tax2.30.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.30.west.CI

# Test: 1940

# rr
f1 <- formula(year2y1940 ~ year2y1930 + delta, weights=1/year2y1870)
f2 <- formula(year2x1940 ~ year2x1930)

rr.40.west <- boot(data=gbr.west,
                statistic=Run2Stage,
                f1=f1, f2=f2,
                R=1000,
                parallel="multicore", ncpus = cores)

rr.40.west.delta <- rr.40.west$t0[['delta']]
rr.40.west.delta

rr.40.west.CI <- boot.ci(rr.40.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
rr.40.west.CI

# Test: pooled

gbr.west.pooled <- homestead.rr.long[homestead.rr.long$state.abb %in% western.pub,]

gbr.west.pooled <- merge(gbr.west.pooled, gbr.west[c("state.abb", "county", "year2y1870", "year3y1870")],
                         by=c("state.abb","county")) 
# Create lags
gbr.west.pooled <- gbr.west.pooled %>% 
  group_by(state.abb,county) %>% 
  mutate(taxpc1.lag = TLag(taxpc1, 10, time = year),
         taxpc2.lag = TLag(taxpc2, 10, time = year))

# Fix lag mis-matches
gbr.west.pooled$taxpc1.lag[gbr.west.pooled$year==1920] <- ifelse(is.na(gbr.west.pooled$taxpc1[gbr.west.pooled$year==1920]), 
                                                                 gbr.west.pooled$taxpc1[gbr.west.pooled$year==1880], 
                                                                 gbr.west.pooled$taxpc1[gbr.west.pooled$year==1920])
  
gbr.west.pooled$taxpc2.lag[gbr.west.pooled$year==1930] <- ifelse(is.na(gbr.west.pooled$taxpc2[gbr.west.pooled$year==1930]), 
                                                                 gbr.west.pooled$taxpc2[gbr.west.pooled$year==1880], 
                                                                 gbr.west.pooled$taxpc2[gbr.west.pooled$year==1930])
#rr
f1 <- formula(taxpc1 ~ taxpc1.lag + delta, weights=1/year2y1870)
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

#tax2
f1 <- formula(taxpc2 ~ taxpc2.lag + delta, weights=1/taxpc2.1870)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

tax2.west.pooled <- boot(data=gbr.west.pooled,
                          statistic=Run2Stage,
                          f1=f1, f2=f2,
                          R=1000,
                          parallel="multicore", ncpus = cores)

tax2.west.pooled.delta <- tax2.west.pooled$t0[['delta']]
tax2.west.pooled.delta

tax2.west.pooled.CI <- boot.ci(tax2.west.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
tax2.west.pooled.CI

## All public land states

gbr.all <- homestead.rr.wide

# Test: 1880

# rr
f1 <- formula(year2y1880 ~ year2y1870 + delta, weights=1/year2y1870)
f2 <- formula(year2x1880 ~ year2x1870)

rr.80.all <- boot(data=gbr.all,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     parallel="multicore", ncpus = cores)

rr.80.all.delta <- rr.80.all$t0[['delta']]
rr.80.all.delta

rr.80.all.CI <- boot.ci(rr.80.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.80.all.CI

# tax2
f1 <- formula(year3y1880 ~ year3y1870 + delta, weights=1/year3y1870)
f2 <- formula(year2x1880 ~ year2x1870)

tax2.80.all <- boot(data=gbr.all,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     parallel="multicore", ncpus = cores)

tax2.80.all.delta <- tax2.80.all$t0[['delta']]
tax2.80.all.delta

tax2.80.all.CI <- boot.ci(tax2.80.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.80.all.CI

# Test: 1920

# rr
f1 <- formula(year2y1920 ~ year2y1880 + delta, weights=1/year2y1870)
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

# Test: 1930

# rr
f1 <- formula(year2y1930 ~ year2y1920 + delta, weights=1/year2y1870)
f2 <- formula(year2x1930 ~ year2x1920)

rr.30.all <- boot(data=gbr.all,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     parallel="multicore", ncpus = cores)

rr.30.all.delta <- rr.30.all$t0[['delta']]
rr.30.all.delta

rr.30.all.CI <- boot.ci(rr.30.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.30.all.CI

# tax2
f1 <- formula(year3y1930 ~ year3y1880 + delta, weights=1/year3y1870)
f2 <- formula(year2x1930 ~ year2x1880)

tax2.30.all <- boot(data=gbr.all,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     parallel="multicore", ncpus = cores)

tax2.30.all.delta <- tax2.30.all$t0[['delta']]
tax2.30.all.delta

tax2.30.all.CI <- boot.ci(tax2.30.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.30.all.CI

# Test: 1940

# rr
f1 <- formula(year2y1940 ~ year2y1930 + delta, weights=1/year2y1870)
f2 <- formula(year2x1940 ~ year2x1930)

rr.40.all <- boot(data=gbr.all,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     parallel="multicore", ncpus = cores)

rr.40.all.delta <- rr.40.all$t0[['delta']]
rr.40.all.delta

rr.40.all.CI <- boot.ci(rr.40.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
rr.40.all.CI

# Test: pooled

gbr.all.pooled <- homestead.rr.long[homestead.rr.long$state.abb %in% pub.states,]

gbr.all.pooled <- merge(gbr.all.pooled, gbr.all[c("state.abb", "county", "year2y1870", "year3y1870")],
                         by=c("state.abb","county")) 
# Create lags
gbr.all.pooled <- gbr.all.pooled %>% 
  group_by(state.abb,county) %>% 
  mutate(taxpc1.lag = TLag(taxpc1, 10, time = year),
         taxpc2.lag = TLag(taxpc2, 10, time = year))

# Fix lag mis-matches
gbr.all.pooled$taxpc1.lag[gbr.all.pooled$year==1920] <- ifelse(is.na(gbr.all.pooled$taxpc1[gbr.all.pooled$year==1920]), 
                                                                 gbr.all.pooled$taxpc1[gbr.all.pooled$year==1880], 
                                                                 gbr.all.pooled$taxpc1[gbr.all.pooled$year==1920])

gbr.all.pooled$taxpc2.lag[gbr.all.pooled$year==1930] <- ifelse(is.na(gbr.all.pooled$taxpc2[gbr.all.pooled$year==1930]), 
                                                                 gbr.all.pooled$taxpc2[gbr.all.pooled$year==1880], 
                                                                 gbr.all.pooled$taxpc2[gbr.all.pooled$year==1930])
#rr
f1 <- formula(taxpc1 ~ taxpc1.lag + delta, weights=1/year2y1870)
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

#tax2
f1 <- formula(taxpc2 ~ taxpc2.lag + delta, weights=1/taxpc2.1870)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

tax2.all.pooled <- boot(data=gbr.all.pooled,
                         statistic=Run2Stage,
                         f1=f1, f2=f2,
                         R=1000,
                         parallel="multicore", ncpus = cores)

tax2.all.pooled.delta <- tax2.all.pooled$t0[['delta']]
tax2.all.pooled.delta

tax2.all.pooled.CI <- boot.ci(tax2.all.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
tax2.all.pooled.CI

## Plot results

ForestPlot2 <- function(d, xlab, ylab, title="", leglab1, leglab2, ylim=NULL){
  p <- ggplot(d, aes(x=x, y = y, ymin=y.lo, ymax=y.hi,colour=region, shape=variable)) + 
    geom_pointrange(size=1, alpha=0.6) + 
    #  coord_flip() +
    geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0), colour="black", lty=2) +
  #  scale_y_continuous(labels = scales::percent) +
    labs(colour = leglab1 , shape =leglab2) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(ylim=ylim) +
    ylab(ylab) +
    xlab(xlab) #switch because of the coord_flip() above
  return(p)
}

# Create data for plot 
test.years <- seq(1880,1940,10)

plot.taxpc.year.gbr <- data.frame(region= rep(c("South","West", "All"),each=length(test.years), times=2),
                                  variable= rep(c("rr","Tax2"),each=length(test.years)*3),
                             y = c(NA, NA, NA, NA, rr.20.delta,rr.30.delta, rr.40.delta,
                                   rr.80.west.delta, NA, NA, NA, rr.20.west.delta, rr.30.west.delta, rr.40.west.delta,
                                   rr.80.all.delta, NA, NA, NA, rr.20.all.delta, rr.30.all.delta, rr.40.all.delta,
                                   NA, NA, NA, NA, NA,tax2.30.delta, NA,
                                   tax2.80.west.delta, NA, NA, NA, NA, tax2.30.west.delta, NA,
                                   tax2.80.all.delta, NA, NA, NA, NA, tax2.30.all.delta, NA),
                             y.lo = c(NA, NA, NA, NA, rr.20.CI[1],rr.30.CI[1], rr.40.CI[1],
                                      rr.80.west.CI[1], NA, NA, NA, rr.20.west.CI[1], rr.30.west.CI[1], rr.40.west.CI[1],
                                      rr.80.all.CI[1], NA, NA, NA, rr.20.all.CI[1], rr.30.all.CI[1], rr.40.all.CI[1],
                                      NA, NA, NA, NA, NA,tax2.30.CI[1], NA,
                                      tax2.80.west.CI[1], NA, NA, NA, NA, tax2.30.west.CI[1], NA,
                                      tax2.80.all.CI[1], NA, NA, NA, NA, tax2.30.all.CI[1], NA),
                             y.hi = c(NA, NA, NA, NA, rr.20.CI[2],rr.30.CI[2], rr.40.CI[2],
                                      rr.80.west.CI[2], NA, NA, NA, rr.20.west.CI[2], rr.30.west.CI[2], rr.40.west.CI[2],
                                      rr.80.all.CI[2], NA, NA, NA, rr.20.all.CI[2], rr.30.all.CI[2], rr.40.all.CI[2],
                             NA, NA, NA, NA, NA,tax2.30.CI[2], NA,
                             tax2.80.west.CI[2], NA, NA, NA, NA, tax2.30.west.CI[2], NA,
                             tax2.80.all.CI[2], NA, NA, NA, NA, tax2.30.all.CI[2], NA))

plot.taxpc.year.gbr$x <- rep(test.years,3*2)

plot.taxpc.year.gbr <- plot.taxpc.year.gbr[rowSums(is.na(plot.taxpc.year.gbr[c("y", "y.lo", "y.hi")]))!=ncol(plot.taxpc.year.gbr[c("y", "y.lo", "y.hi")]), ] #rm NA years

# Plot forest plots
plot.taxpc.year.gbr$x <- as.factor(plot.taxpc.year.gbr$x) 
taxpc.plot.year.gbr <- ForestPlot2(plot.taxpc.year.gbr,ylab="Estimated effect of lagged per-capita homesteads",xlab="",title="GBR: Fiscal capacity",leglab1="Region", leglab2="Measure")

ggsave(paste0(results.directory,"gbr-taxpc.png"), taxpc.plot.year.gbr, width=11, height=8.5)  
