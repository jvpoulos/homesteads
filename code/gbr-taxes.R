###################################
#  GBR Regression                 #
###################################

library(boot)
library(tidyr)
library(zoo)
library(readxl)
library(dplyr)
library(plm)

# Two-stage estimates

source(paste0(homestead.code.directory,"Run2Stage.R"))

## Southern public land states

gbr.south <- homestead.tax.wide[homestead.tax.wide$state.abb %in% southern.pub,]

# Test: 1920

# tax1
f1 <- formula(year2y1920 ~ year2y1880 + delta, weights=1/year2y1870)
f2 <- formula(year2x1920 ~ year2x1880)

tax1.20 <- boot(data=gbr.south,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.south$county, # stratify by county
                 parallel="multicore", ncpus = cores)

tax1.20.delta <- tax1.20$t0[['delta']]
tax1.20.delta

tax1.20.CI <- boot.ci(tax1.20, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.20.CI

# Test: 1930

# tax1
f1 <- formula(year2y1930 ~ year2y1920 + delta, weights=1/year2y1870)
f2 <- formula(year2x1930 ~ year2x1920)

tax1.30 <- boot(data=gbr.south,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.south$county,
                 parallel="multicore", ncpus = cores)

tax1.30.delta <- tax1.30$t0[['delta']]
tax1.30.delta

tax1.30.CI <- boot.ci(tax1.30, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.30.CI

# tax2
f1 <- formula(year3y1930 ~ year3y1880 + delta, weights=1/year3y1870)
f2 <- formula(year2x1930 ~ year2x1880)

tax2.30 <- boot(data=gbr.south,
                statistic=Run2Stage,
                f1=f1, f2=f2,
                R=1000,
                strata=gbr.south$county,
                parallel="multicore", ncpus = cores)

tax2.30.delta <- tax2.30$t0[['delta']]
tax2.30.delta

tax2.30.CI <- boot.ci(tax2.30, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.30.CI

# Test: 1940

# tax1
f1 <- formula(year2y1940 ~ year2y1930 + delta, weights=1/year2y1870)
f2 <- formula(year2x1940 ~ year2x1930)

tax1.40 <- boot(data=gbr.south,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.south$county,
                 parallel="multicore", ncpus = cores)

tax1.40.delta <- tax1.40$t0[['delta']]
tax1.40.delta

tax1.40.CI <- boot.ci(tax1.40, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
tax1.40.CI

# Test: pooled

gbr.south.pooled<- homestead.tax.long[homestead.tax.long$state.abb %in% southern.pub,]

# Create lags
TLag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}

gbr.south.pooled <- gbr.south.pooled %>% 
  group_by(state.abb,county) %>% 
  mutate(taxpc1.lag = TLag(taxpc1, 10, time = year),
         taxpc2.lag = TLag(taxpc2, 10, time = year),
         taxpc1.1870 = taxpc1[year==1870],
         taxpc2.1870 = taxpc2[year==1870])

# Fix lag mis-matches
gbr.south.pooled$taxpc1.lag[gbr.south.pooled$year==1920] <- gbr.south.pooled$taxpc1[gbr.south.pooled$year==1880] #1920/1880
gbr.south.pooled$taxpc2.lag[gbr.south.pooled$year==1930] <- gbr.south.pooled$taxpc2[gbr.south.pooled$year==1880] #1930/1880

#tax1
f1 <- formula(taxpc1 ~ taxpc1.lag + delta, weights=1/taxpc1.1870)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

tax1.south.pooled <- boot(data=gbr.south.pooled,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      R=1000,
                      strata=gbr.south.pooled$county,
                      parallel="multicore", ncpus = cores)

tax1.south.pooled.delta <- tax1.south.pooled$t0[['delta']]
tax1.south.pooled.delta

tax1.south.pooled.CI <- boot.ci(tax1.south.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
tax1.south.pooled.CI

#tax2
f1 <- formula(taxpc2 ~ taxpc2.lag + delta, weights=1/taxpc2.1870)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

tax2.south.pooled <- boot(data=gbr.south.pooled,
                          statistic=Run2Stage,
                          f1=f1, f2=f2,
                          R=1000,
                          strata=gbr.south.pooled$county,
                          parallel="multicore", ncpus = cores)

tax2.south.pooled.delta <- tax2.south.pooled$t0[['delta']]
tax2.south.pooled.delta

tax2.south.pooled.CI <- boot.ci(tax2.south.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
tax2.south.pooled.CI

## Western public land states

gbr.west <- homestead.tax.wide[homestead.tax.wide$state.abb %in% western.pub,]

# Test: 1880

# tax1
f1 <- formula(year2y1880 ~ year2y1870 + delta, weights=1/year2y1870)
f2 <- formula(year2x1880 ~ year2x1870)

tax1.80.west <- boot(data=gbr.west,
                statistic=Run2Stage,
                f1=f1, f2=f2,
                R=1000,
                strata=gbr.west$county, # stratify by county
                parallel="multicore", ncpus = cores)

tax1.80.west.delta <- tax1.80.west$t0[['delta']]
tax1.80.west.delta

tax1.80.west.CI <- boot.ci(tax1.80.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.80.west.CI

# tax2
f1 <- formula(year3y1880 ~ year3y1870 + delta, weights=1/year3y1870)
f2 <- formula(year2x1880 ~ year2x1870)

tax2.80.west <- boot(data=gbr.west,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     strata=gbr.west$county, # stratify by county
                     parallel="multicore", ncpus = cores)

tax2.80.west.delta <- tax2.80.west$t0[['delta']]
tax2.80.west.delta

tax2.80.west.CI <- boot.ci(tax2.80.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.80.west.CI

# Test: 1920

# tax1
f1 <- formula(year2y1920 ~ year2y1880 + delta, weights=1/year2y1870)
f2 <- formula(year2x1920 ~ year2x1880)

tax1.20.west <- boot(data=gbr.west,
                statistic=Run2Stage,
                f1=f1, f2=f2,
                R=1000,
                strata=gbr.west$county, # stratify by county
                parallel="multicore", ncpus = cores)

tax1.20.west.delta <- tax1.20.west$t0[['delta']]
tax1.20.west.delta

tax1.20.west.CI <- boot.ci(tax1.20.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.20.west.CI

# Test: 1930

# tax1
f1 <- formula(year2y1930 ~ year2y1920 + delta, weights=1/year2y1870)
f2 <- formula(year2x1930 ~ year2x1920)

tax1.30.west <- boot(data=gbr.west,
                statistic=Run2Stage,
                f1=f1, f2=f2,
                R=1000,
                strata=gbr.west$county,
                parallel="multicore", ncpus = cores)

tax1.30.west.delta <- tax1.30.west$t0[['delta']]
tax1.30.west.delta

tax1.30.west.CI <- boot.ci(tax1.30.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.30.west.CI

# tax2
f1 <- formula(year3y1930 ~ year3y1880 + delta, weights=1/year3y1870)
f2 <- formula(year2x1930 ~ year2x1880)

tax2.30.west <- boot(data=gbr.west,
                statistic=Run2Stage,
                f1=f1, f2=f2,
                R=1000,
                strata=gbr.west$county,
                parallel="multicore", ncpus = cores)

tax2.30.west.delta <- tax2.30.west$t0[['delta']]
tax2.30.west.delta

tax2.30.west.CI <- boot.ci(tax2.30.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.30.west.CI

# Test: 1940

# tax1
f1 <- formula(year2y1940 ~ year2y1930 + delta, weights=1/year2y1870)
f2 <- formula(year2x1940 ~ year2x1930)

tax1.40.west <- boot(data=gbr.west,
                statistic=Run2Stage,
                f1=f1, f2=f2,
                R=1000,
                strata=gbr.west$county,
                parallel="multicore", ncpus = cores)

tax1.40.west.delta <- tax1.40.west$t0[['delta']]
tax1.40.west.delta

tax1.40.west.CI <- boot.ci(tax1.40.west, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
tax1.40.west.CI

# Test: pooled

gbr.west.pooled <- homestead.tax.long[homestead.tax.long$state.abb %in% western.pub,]

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
#tax1
f1 <- formula(taxpc1 ~ taxpc1.lag + delta, weights=1/year2y1870)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

tax1.west.pooled <- boot(data=gbr.west.pooled,
                          statistic=Run2Stage,
                          f1=f1, f2=f2,
                          R=1000,
                          strata=gbr.west.pooled$county,
                          parallel="multicore", ncpus = cores)

tax1.west.pooled.delta <- tax1.west.pooled$t0[['delta']]
tax1.west.pooled.delta

tax1.west.pooled.CI <- boot.ci(tax1.west.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
tax1.west.pooled.CI

#tax2
f1 <- formula(taxpc2 ~ taxpc2.lag + delta, weights=1/taxpc2.1870)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

tax2.west.pooled <- boot(data=gbr.west.pooled,
                          statistic=Run2Stage,
                          f1=f1, f2=f2,
                          R=1000,
                          strata=gbr.west.pooled$county,
                          parallel="multicore", ncpus = cores)

tax2.west.pooled.delta <- tax2.west.pooled$t0[['delta']]
tax2.west.pooled.delta

tax2.west.pooled.CI <- boot.ci(tax2.west.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
tax2.west.pooled.CI

## All public land states

gbr.all <- homestead.tax.wide

# Test: 1880

# tax1
f1 <- formula(year2y1880 ~ year2y1870 + delta, weights=1/year2y1870)
f2 <- formula(year2x1880 ~ year2x1870)

tax1.80.all <- boot(data=gbr.all,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     strata=gbr.all$county, # stratify by county
                     parallel="multicore", ncpus = cores)

tax1.80.all.delta <- tax1.80.all$t0[['delta']]
tax1.80.all.delta

tax1.80.all.CI <- boot.ci(tax1.80.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.80.all.CI

# tax2
f1 <- formula(year3y1880 ~ year3y1870 + delta, weights=1/year3y1870)
f2 <- formula(year2x1880 ~ year2x1870)

tax2.80.all <- boot(data=gbr.all,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     strata=gbr.all$county, # stratify by county
                     parallel="multicore", ncpus = cores)

tax2.80.all.delta <- tax2.80.all$t0[['delta']]
tax2.80.all.delta

tax2.80.all.CI <- boot.ci(tax2.80.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.80.all.CI

# Test: 1920

# tax1
f1 <- formula(year2y1920 ~ year2y1880 + delta, weights=1/year2y1870)
f2 <- formula(year2x1920 ~ year2x1880)

tax1.20.all <- boot(data=gbr.all,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     strata=gbr.all$county, # stratify by county
                     parallel="multicore", ncpus = cores)

tax1.20.all.delta <- tax1.20.all$t0[['delta']]
tax1.20.all.delta

tax1.20.all.CI <- boot.ci(tax1.20.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.20.all.CI

# Test: 1930

# tax1
f1 <- formula(year2y1930 ~ year2y1920 + delta, weights=1/year2y1870)
f2 <- formula(year2x1930 ~ year2x1920)

tax1.30.all <- boot(data=gbr.all,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     strata=gbr.all$county,
                     parallel="multicore", ncpus = cores)

tax1.30.all.delta <- tax1.30.all$t0[['delta']]
tax1.30.all.delta

tax1.30.all.CI <- boot.ci(tax1.30.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.30.all.CI

# tax2
f1 <- formula(year3y1930 ~ year3y1880 + delta, weights=1/year3y1870)
f2 <- formula(year2x1930 ~ year2x1880)

tax2.30.all <- boot(data=gbr.all,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     strata=gbr.all$county,
                     parallel="multicore", ncpus = cores)

tax2.30.all.delta <- tax2.30.all$t0[['delta']]
tax2.30.all.delta

tax2.30.all.CI <- boot.ci(tax2.30.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.30.all.CI

# Test: 1940

# tax1
f1 <- formula(year2y1940 ~ year2y1930 + delta, weights=1/year2y1870)
f2 <- formula(year2x1940 ~ year2x1930)

tax1.40.all <- boot(data=gbr.all,
                     statistic=Run2Stage,
                     f1=f1, f2=f2,
                     R=1000,
                     strata=gbr.all$county,
                     parallel="multicore", ncpus = cores)

tax1.40.all.delta <- tax1.40.all$t0[['delta']]
tax1.40.all.delta

tax1.40.all.CI <- boot.ci(tax1.40.all, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
tax1.40.all.CI

# Test: pooled

gbr.all.pooled <- homestead.tax.long[homestead.tax.long$state.abb %in% pub.states,]

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
#tax1
f1 <- formula(taxpc1 ~ taxpc1.lag + delta, weights=1/year2y1870)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

tax1.all.pooled <- boot(data=gbr.all.pooled,
                         statistic=Run2Stage,
                         f1=f1, f2=f2,
                         R=1000,
                         strata=gbr.all.pooled$county,
                         parallel="multicore", ncpus = cores)

tax1.all.pooled.delta <- tax1.all.pooled$t0[['delta']]
tax1.all.pooled.delta

tax1.all.pooled.CI <- boot.ci(tax1.all.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
tax1.all.pooled.CI

#tax2
f1 <- formula(taxpc2 ~ taxpc2.lag + delta, weights=1/taxpc2.1870)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

tax2.all.pooled <- boot(data=gbr.all.pooled,
                         statistic=Run2Stage,
                         f1=f1, f2=f2,
                         R=1000,
                         strata=gbr.all.pooled$county,
                         parallel="multicore", ncpus = cores)

tax2.all.pooled.delta <- tax2.all.pooled$t0[['delta']]
tax2.all.pooled.delta

tax2.all.pooled.CI <- boot.ci(tax2.all.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
tax2.all.pooled.CI

## Plot results

ForestPlot2 <- function(d, xlab, ylab, title="", leglab, ylim=NULL){
  p <- ggplot(d, aes(x=x, y = y, ymin=y.lo, ymax=y.hi,colour=variable)) + 
    geom_pointrange(size=1, alpha=0.6) + 
    #  coord_flip() +
    geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0), colour="black", lty=2) +
  #  scale_y_continuous(labels = scales::percent) +
    labs(colour = leglab) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(ylim=ylim) +
    ylab(ylab) +
    xlab(xlab) #switch because of the coord_flip() above
  return(p)
}

# Create data for plot 
test.years <- seq(1880,1940,10)

plot.tax1.year.gbr <- data.frame(variable= rep(c("South","West", "All"),each=length(test.years)),
                             y = c(NA, NA, NA, NA, tax1.20.delta,tax1.30.delta, tax1.40.delta,
                                   tax1.80.west.delta, NA, NA, NA, tax1.20.west.delta, tax1.30.west.delta, tax1.40.west,
                                   tax1.80.all.delta, NA, NA, NA, tax1.20.all.delta, tax1.30.all.delta, tax1.40.all),
                             y.lo = c(NA, NA, NA, NA, tax1.20.delta,tax1.30.delta, tax1.40.delta,
                                      tax1.80.west.delta, NA, NA, NA, tax1.20.west.delta, tax1.30.west.delta, tax1.40.west,
                                      tax1.80.all.delta, NA, NA, NA, tax1.20.all.delta, tax1.30.all.delta, tax1.40.all),
                             y.hi = c(NA, NA, NA, NA, tax1.20.delta,tax1.30.delta, tax1.40.delta,
                                      tax1.80.west.delta, NA, NA, NA, tax1.20.west.delta, tax1.30.west.delta, tax1.40.west,
                                      tax1.80.all.delta, NA, NA, NA, tax1.20.all.delta, tax1.30.all.delta, tax1.40.all))


plot.tax1.year.gbr$x <- rep(test.years,3)

# Plot forest plots
plot.tax1.year.gbr$x <- as.factor(plot.tax1.year.gbr$x) 
summary.plot.year <- ForestPlot2(plot.tax1.year.gbr,ylab="Estimated effect of lagged per-capita homesteads",xlab="",title="GBR: Log per-capita taxes collected by counties (Tax 1)",leglab="Region") + ylim(c(-0.03, 0.03))

ggsave(paste0(results.directory,"gbr-tax1.png"), summary.plot.year, width=11, height=8.5)  
