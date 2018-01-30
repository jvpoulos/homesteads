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

gbr.south <- census.ts.aland[census.ts.aland$state.abb %in% southern.pub,]


# # Test: 1870
# 
# f1 <- formula(yeary1870 ~ yeary1860 + delta, weights=1/yeary1860)
# f2 <- formula(year2x1870 ~ year2x1860)
# 
# aland.70 <- boot(data=gbr.south,
#                  statistic=Run2Stage,
#                  f1=f1, f2=f2,
#                  first.test=TRUE,
#                  R=1000,
#                  strata=gbr.south$county, # stratify by county
#                  parallel="multicore", ncpus = cores)
# 
# aland.70.delta <- aland.70$t0[['delta']]
# aland.70.delta
# 
# aland.70.CI <- boot.ci(aland.70, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
# aland.70.CI
# 
# # Test: 1880
# 
# f1 <- formula(yeary1880 ~ yeary1870 + delta, weights=1/yeary1860)
# f2 <- formula(year2x1880 ~ year2x1870)
# 
# aland.80 <- boot(data=gbr.south,
#                 statistic=Run2Stage,
#                 f1=f1, f2=f2,
#                 R=1000,
#                 strata=gbr.south$county, # stratify by county
#                 parallel="multicore", ncpus = cores)
# 
# aland.80.delta <- aland.80$t0[['delta']]
# aland.80.delta
# 
# aland.80.CI <- boot.ci(aland.80, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
# aland.80.CI

# Test: 1890

f1 <- formula(yeary1890 ~ yeary1880 + delta, weights=1/yeary1860)
f2 <- formula(year2x1890 ~ year2x1880)

aland.90 <- boot(data=gbr.south,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.south$county, # stratify by county
                 parallel="multicore", ncpus = cores)

aland.90.delta <- aland.90$t0[['delta']]
aland.90.delta

aland.90.CI <- boot.ci(aland.90, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.90.CI

# Test: 1900

f1 <- formula(yeary1900 ~ yeary1890 + delta, weights=1/yeary1860)
f2 <- formula(year2x1900 ~ year2x1890)

aland.00 <- boot(data=gbr.south,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.south$county,
                 parallel="multicore", ncpus = cores)

aland.00.delta <- aland.00$t0[['delta']]
aland.00.delta

aland.00.CI <- boot.ci(aland.00, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.00.CI

# Test: 1910

f1 <- formula(yeary1910 ~ yeary1900 + delta, weights=1/yeary1860)
f2 <- formula(year2x1910 ~ year2x1900)

aland.10 <- boot(data=gbr.south,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.south$county,
                 parallel="multicore", ncpus = cores)

aland.10.delta <- aland.10$t0[['delta']]
aland.10.delta

aland.10.CI <- boot.ci(aland.10, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.10.CI

# Test: 1920

f1 <- formula(yeary1920 ~ yeary1910 + delta, weights=1/yeary1860)
f2 <- formula(year2x1920 ~ year2x1910)

aland.20 <- boot(data=gbr.south,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.south$county,
                 parallel="multicore", ncpus = cores)

aland.20.delta <- aland.20$t0[['delta']]
aland.20.delta

aland.20.CI <- boot.ci(aland.20, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.20.CI

# Test: 1930

f1 <- formula(yeary1930 ~ yeary1920 + delta, weights=1/yeary1860)
f2 <- formula(year2x1930 ~ year2x1920)

aland.30 <- boot(data=gbr.south,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.south$county,
                 parallel="multicore", ncpus = cores)

aland.30.delta <- aland.30$t0[['delta']]
aland.30.delta

aland.30.CI <- boot.ci(aland.30, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.30.CI

# Test: 1940

f1 <- formula(yeary1940 ~ yeary1930 + delta, weights=1/yeary1860)
f2 <- formula(year2x1940 ~ year2x1930)

aland.40 <- boot(data=gbr.south,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.south$county,
                 parallel="multicore", ncpus = cores)

aland.40.delta <- aland.40$t0[['delta']]
aland.40.delta

aland.40.CI <- boot.ci(aland.40, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.40.CI

# Test: 1950

f1 <- formula(yeary1950 ~ yeary1940 + delta, weights=1/yeary1860)
f2 <- formula(year2x1950 ~ year2x1940)

aland.50 <- boot(data=gbr.south,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.south$county,
                 parallel="multicore", ncpus = cores)

aland.50.delta <- aland.50$t0[['delta']]
aland.50.delta

aland.50.CI <- boot.ci(aland.50, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.50.CI

# Test: pooled

gbr.south.pooled<- merge(census.ts.means[census.ts.means$state.abb %in% southern.pub,], 
                                patents.decennial, by.x=c("state.abb","county","year")
                                , by.y=c("state_code","county_code","year2"), all.x=TRUE)

# Create lags
TLag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}

gbr.south.pooled<- gbr.south.pooled%>% 
  group_by(state.abb,county) %>% 
  mutate(aland.gini.lag = TLag(aland.gini, 10, time = year),
         homesteads.pc.lag = TLag(homesteads.pc, 10, time = year),
         yeary1860 = aland.gini[year==1860]) %>%
  filter(year >= 1880) # to compare with FE estimates

f1 <- formula(aland.gini ~ aland.gini.lag + delta, weights=1/yeary1860)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

aland.south.pooled <- boot(data=gbr.south.pooled,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      R=1000,
                      strata=gbr.south.pooled$county,
                      parallel="multicore", ncpus = cores)

aland.south.pooled.delta <- aland.south.pooled$t0[['delta']]
aland.south.pooled.delta

aland.south.pooled.CI <- boot.ci(aland.south.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.south.pooled.CI

## Western public land states

gbr.west <- census.ts.aland[census.ts.aland$state.abb %in% western.pub,]

# Test: 1870

f1 <- formula(yeary1870 ~ yeary1860 + delta, weights=1/yeary1860)
f2 <- formula(year2x1870 ~ year2x1860)

aland.west.70 <- boot(data=gbr.west,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 first.test=TRUE,
                 R=1000,
                 strata=gbr.west$county,
                 parallel="multicore", ncpus = cores)

aland.west.70.delta <- aland.west.70$t0[['delta']]
aland.west.70.delta

aland.west.70.CI <- boot.ci(aland.west.70, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.west.70.CI

# Test: 1880

f1 <- formula(yeary1880 ~ yeary1870 + delta, weights=1/yeary1860)
f2 <- formula(year2x1880 ~ year2x1870)

aland.west.80 <- boot(data=gbr.west,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      R=1000,
                      strata=gbr.west$county,
                      parallel="multicore", ncpus = cores)

aland.west.80.delta <- aland.west.80$t0[['delta']]
aland.west.80.delta

aland.west.80.CI <- boot.ci(aland.west.80, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.west.80.CI

# Test: 1890

f1 <- formula(yeary1890 ~ yeary1880 + delta, weights=1/yeary1860)
f2 <- formula(year2x1890 ~ year2x1880)

aland.west.90 <- boot(data=gbr.west,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.west$county,
                 parallel="multicore", ncpus = cores)

aland.west.90.delta <- aland.west.90$t0[['delta']]
aland.west.90.delta

aland.west.90.CI <- boot.ci(aland.west.90, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.west.90.CI

# Test: 1900

f1 <- formula(yeary1900 ~ yeary1890 + delta, weights=1/yeary1860)
f2 <- formula(year2x1900 ~ year2x1890)

aland.west.00 <- boot(data=gbr.west,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.west$county,
                 parallel="multicore", ncpus = cores)

aland.west.00.delta <- aland.west.00$t0[['delta']]
aland.west.00.delta

aland.west.00.CI <- boot.ci(aland.west.00, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.west.00.CI

# Test: 1910

f1 <- formula(yeary1910 ~ yeary1900 + delta, weights=1/yeary1860)
f2 <- formula(year2x1910 ~ year2x1900)

aland.west.10 <- boot(data=gbr.west,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.west$county,
                 parallel="multicore", ncpus = cores)

aland.west.10.delta <- aland.west.10$t0[['delta']]
aland.west.10.delta

aland.west.10.CI <- boot.ci(aland.west.10, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.west.10.CI

# Test: 1920

f1 <- formula(yeary1920 ~ yeary1910 + delta, weights=1/yeary1860)
f2 <- formula(year2x1920 ~ year2x1910)

aland.west.20 <- boot(data=gbr.west,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.west$county,
                 parallel="multicore", ncpus = cores)

aland.west.20.delta <- aland.west.20$t0[['delta']]
aland.west.20.delta

aland.west.20.CI <- boot.ci(aland.west.20, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.west.20.CI

# Test: 1930

f1 <- formula(yeary1930 ~ yeary1920 + delta, weights=1/yeary1860)
f2 <- formula(year2x1930 ~ year2x1920)

aland.west.30 <- boot(data=gbr.west,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.west$county,
                 parallel="multicore", ncpus = cores)

aland.west.30.delta <- aland.west.30$t0[['delta']]
aland.west.30.delta

aland.west.30.CI <- boot.ci(aland.west.30, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.west.30.CI

# Test: 1940

f1 <- formula(yeary1940 ~ yeary1930 + delta, weights=1/yeary1860)
f2 <- formula(year2x1940 ~ year2x1930)

aland.west.40 <- boot(data=gbr.west,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.west$county,
                 parallel="multicore", ncpus = cores)

aland.west.40.delta <- aland.west.40$t0[['delta']]
aland.west.40.delta

aland.west.40.CI <- boot.ci(aland.west.40, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.west.40.CI

# Test: 1950

f1 <- formula(yeary1950 ~ yeary1940 + delta, weights=1/yeary1860)
f2 <- formula(year2x1950 ~ year2x1940)

aland.west.50 <- boot(data=gbr.west,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 R=1000,
                 strata=gbr.west$county,
                 parallel="multicore", ncpus = cores)

aland.west.50.delta <- aland.50$t0[['delta']]
aland.west.50.delta

aland.west.50.CI <- boot.ci(aland.west.50, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.west.50.CI

# Test: pooled

gbr.west.pooled <- merge(census.ts.means[census.ts.means$state.abb %in% western.pub,], 
                              patents.decennial, by.x=c("state.abb","county","year")
                                      , by.y=c("state_code","county_code","year2"), all.x=TRUE)

gbr.west.pooled <- merge(gbr.west.pooled, gbr.west[c("state.abb", "county", "yeary1860")],
                         by=c("state.abb","county")) # keeps obs with 1860 ineq
# Create lags
gbr.west.pooled <- gbr.west.pooled %>% 
  group_by(state.abb,county) %>% 
  mutate(aland.gini.lag = TLag(aland.gini, 10, time = year),
         homesteads.pc.lag = TLag(homesteads.pc, 10, time = year)) %>%
  filter(year >= 1880) # to compare with FE estimates

f1 <- formula(aland.gini ~ aland.gini.lag + delta, weights=1/yeary1860)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

aland.west.pooled <- boot(data=gbr.west.pooled,
                           statistic=Run2Stage,
                           f1=f1, f2=f2,
                           R=1000,
                           strata=gbr.west.pooled$county,
                           parallel="multicore", ncpus = cores)

aland.west.pooled.delta <- aland.west.pooled$t0[['delta']]
aland.west.pooled.delta

aland.west.pooled.CI <- boot.ci(aland.west.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.west.pooled.CI

## All public land states

gbr.all <- census.ts.aland[census.ts.aland$state.abb %in% pub.states,]

# Test: 1870

f1 <- formula(yeary1870 ~ yeary1860 + delta, weights=1/yeary1860)
f2 <- formula(year2x1870 ~ year2x1860)

aland.all.70 <- boot(data=gbr.all,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      first.test=TRUE,
                      R=1000,
                      strata=gbr.all$county,
                      parallel="multicore", ncpus = cores)

aland.all.70.delta <- aland.all.70$t0[['delta']]
aland.all.70.delta

aland.all.70.CI <- boot.ci(aland.all.70, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.all.70.CI

# Test: 1880

f1 <- formula(yeary1880 ~ yeary1870 + delta, weights=1/yeary1860)
f2 <- formula(year2x1880 ~ year2x1870)

aland.all.80 <- boot(data=gbr.all,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      R=1000,
                      strata=gbr.all$county,
                      parallel="multicore", ncpus = cores)

aland.all.80.delta <- aland.all.80$t0[['delta']]
aland.all.80.delta

aland.all.80.CI <- boot.ci(aland.all.80, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.all.80.CI

# Test: 1890

f1 <- formula(yeary1890 ~ yeary1880 + delta, weights=1/yeary1860)
f2 <- formula(year2x1890 ~ year2x1880)

aland.all.90 <- boot(data=gbr.all,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      R=1000,
                      strata=gbr.all$county,
                      parallel="multicore", ncpus = cores)

aland.all.90.delta <- aland.all.90$t0[['delta']]
aland.all.90.delta

aland.all.90.CI <- boot.ci(aland.all.90, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.all.90.CI

# Test: 1900

f1 <- formula(yeary1900 ~ yeary1890 + delta, weights=1/yeary1860)
f2 <- formula(year2x1900 ~ year2x1890)

aland.all.00 <- boot(data=gbr.all,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      R=1000,
                      strata=gbr.all$county,
                      parallel="multicore", ncpus = cores)

aland.all.00.delta <- aland.all.00$t0[['delta']]
aland.all.00.delta

aland.all.00.CI <- boot.ci(aland.all.00, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.all.00.CI

# Test: 1910

f1 <- formula(yeary1910 ~ yeary1900 + delta, weights=1/yeary1860)
f2 <- formula(year2x1910 ~ year2x1900)

aland.all.10 <- boot(data=gbr.all,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      R=1000,
                      strata=gbr.all$county,
                      parallel="multicore", ncpus = cores)

aland.all.10.delta <- aland.all.10$t0[['delta']]
aland.all.10.delta

aland.all.10.CI <- boot.ci(aland.all.10, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.all.10.CI

# Test: 1920

f1 <- formula(yeary1920 ~ yeary1910 + delta, weights=1/yeary1860)
f2 <- formula(year2x1920 ~ year2x1910)

aland.all.20 <- boot(data=gbr.all,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      R=1000,
                      strata=gbr.all$county,
                      parallel="multicore", ncpus = cores)

aland.all.20.delta <- aland.all.20$t0[['delta']]
aland.all.20.delta

aland.all.20.CI <- boot.ci(aland.all.20, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.all.20.CI

# Test: 1930

f1 <- formula(yeary1930 ~ yeary1920 + delta, weights=1/yeary1860)
f2 <- formula(year2x1930 ~ year2x1920)

aland.all.30 <- boot(data=gbr.all,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      R=1000,
                      strata=gbr.all$county,
                      parallel="multicore", ncpus = cores)

aland.all.30.delta <- aland.all.30$t0[['delta']]
aland.all.30.delta

aland.all.30.CI <- boot.ci(aland.all.30, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.all.30.CI

# Test: 1940

f1 <- formula(yeary1940 ~ yeary1930 + delta, weights=1/yeary1860)
f2 <- formula(year2x1940 ~ year2x1930)

aland.all.40 <- boot(data=gbr.all,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      R=1000,
                      strata=gbr.all$county,
                      parallel="multicore", ncpus = cores)

aland.all.40.delta <- aland.all.40$t0[['delta']]
aland.all.40.delta

aland.all.40.CI <- boot.ci(aland.all.40, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.all.40.CI

# Test: 1950

f1 <- formula(yeary1950 ~ yeary1940 + delta, weights=1/yeary1860)
f2 <- formula(year2x1950 ~ year2x1940)

aland.all.50 <- boot(data=gbr.all,
                      statistic=Run2Stage,
                      f1=f1, f2=f2,
                      R=1000,
                      strata=gbr.all$county,
                      parallel="multicore", ncpus = cores)

aland.all.50.delta <- aland.50$t0[['delta']]
aland.all.50.delta

aland.all.50.CI <- boot.ci(aland.all.50, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.all.50.CI

# Test: pooled

gbr.all.pooled <- merge(census.ts.means[census.ts.means$state.abb %in% pub.states,], 
                         patents.decennial, by.x=c("state.abb","county","year")
                         , by.y=c("state_code","county_code","year2"), all.x=TRUE)

gbr.all.pooled <- merge(gbr.all.pooled, gbr.all[c("state.abb", "county", "yeary1860")],
                         by=c("state.abb","county")) # keeps obs with 1860 ineq

# Create lags
gbr.all.pooled <- gbr.all.pooled %>% 
  group_by(state.abb,county) %>% 
  mutate(aland.gini.lag = TLag(aland.gini, 10, time = year),
         homesteads.pc.lag = TLag(homesteads.pc, 10, time = year)) %>%
  filter(year >= 1880) # to compare with FE estimates

f1 <- formula(aland.gini ~ aland.gini.lag + delta, weights=1/yeary1860)
f2 <- formula(homesteads.pc ~ homesteads.pc.lag)

aland.all.pooled <- boot(data=gbr.all.pooled,
                          statistic=Run2Stage,
                          f1=f1, f2=f2,
                          R=1000,
                          strata=gbr.all.pooled$county,
                          parallel="multicore", ncpus = cores)

aland.all.pooled.delta <- aland.all.pooled$t0[['delta']]
aland.all.pooled.delta

aland.all.pooled.CI <- boot.ci(aland.all.pooled, conf=0.95, index=3, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs by first order normal approximation
aland.all.pooled.CI

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
test.years <- seq(1870,1950,10)

plot.data.year.gbr <- data.frame(variable= rep(c("South","West", "All"),each=length(test.years)),
                             y = c(NA, NA, aland.90.delta, aland.00.delta,aland.10.delta,aland.20.delta,aland.30.delta,aland.40.delta,aland.50.delta,
                                   aland.west.70.delta, aland.west.80.delta, aland.west.90.delta, aland.west.00.delta,aland.west.10.delta,aland.west.20.delta,aland.west.30.delta,aland.west.40.delta,aland.west.50.delta,
                             aland.all.70.delta, aland.all.80.delta, aland.all.90.delta, aland.all.00.delta,aland.all.10.delta,aland.all.20.delta,aland.all.30.delta,aland.all.40.delta,aland.all.50.delta),
                             y.lo = c(NA, NA, aland.90.CI[1], aland.00.CI[1],aland.10.CI[1],aland.20.CI[1],aland.30.CI[1],aland.40.CI[1],aland.50.CI[1],
                                      aland.west.70.CI[1], aland.west.80.CI[1], aland.west.90.CI[1], aland.west.00.CI[1],aland.west.10.CI[1],aland.west.20.CI[1],aland.west.30.CI[1],aland.west.40.CI[1],aland.west.50.CI[1],
                                      aland.all.70.CI[1], aland.all.80.CI[1], aland.all.90.CI[1], aland.all.00.CI[1],aland.all.10.CI[1],aland.all.20.CI[1],aland.all.30.CI[1],aland.all.40.CI[1],aland.all.50.CI[1]),
                             y.hi = c(NA, NA, aland.90.CI[2], aland.00.CI[2],aland.10.CI[2],aland.20.CI[2],aland.30.CI[2],aland.40.CI[2],aland.50.CI[2],
                                      aland.west.70.CI[2], aland.west.80.CI[2], aland.west.90.CI[2], aland.west.00.CI[2],aland.west.10.CI[2],aland.west.20.CI[2],aland.west.30.CI[2],aland.west.40.CI[2],aland.west.50.CI[2],
                             aland.all.70.CI[2], aland.all.80.CI[2], aland.all.90.CI[2], aland.all.00.CI[2],aland.all.10.CI[2],aland.all.20.CI[2],aland.all.30.CI[2],aland.all.40.CI[2],aland.all.50.CI[2]))


plot.data.year.gbr$x <- rep(test.years,3)

# Plot forest plots
plot.data.year.gbr$x <- as.factor(plot.data.year.gbr$x) 
summary.plot.year <- ForestPlot2(plot.data.year.gbr,ylab="Estimated effect of lagged per-capita homesteads",xlab="",title="GBR: land inequality",leglab="Region") + ylim(c(-0.03, 0.03))

ggsave(paste0(results.directory,"gbr-homestead.png"), summary.plot.year, width=11, height=8.5)  
