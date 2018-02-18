###################################
#  Fixed effects model  (robust)#
###################################

library(boot)
library(tidyr)
library(zoo)
library(readxl)
library(parallel)
library(doParallel)
library(foreach)

## RR access as additional control 
source(paste0(homestead.code.directory,"RunFE-robust.R"))

# Tax1 + RR access

f1 <- formula(taxpc1 ~ homesteads.pc.lag + access.mean.lag + id) 

# Southern public land state counties 

fe.south <- homestead.tax.long[homestead.tax.long$state.abb %in% c(southern.pub),]

# All years
tax1.south.fe <- boot(data=fe.south,
                          statistic=RunFERobust,
                          f1=f1,
                          R=1000,
                          parallel="multicore", ncpus = cores)

tax1.south.fe.delta <- tax1.south.fe$t0
tax1.south.fe.delta

tax1.south.fe.CI <- boot.ci(tax1.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.south.fe.CI

boot.ci(tax1.south.fe, conf=0.95, index=2, type="norm")$normal[2:3] # railroad access

# Western public land state counties

fe.west <- homestead.tax.long[homestead.tax.long$state.abb %in% c(western.pub),]

# All years

tax1.west.fe <- boot(data=fe.west,
                       statistic=RunFERobust,
                       f1=f1,
                       R=1000,
                       parallel="multicore", ncpus = cores)

tax1.west.fe.delta <- tax1.west.fe$t0
tax1.west.fe.delta

tax1.west.fe.CI <- boot.ci(tax1.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.west.fe.CI

boot.ci(tax1.west.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

# All public land states

fe.all <- homestead.tax.long[homestead.tax.long$state.abb %in% c(pub.states),]

# All years

tax1.all.fe <- boot(data=fe.all,
                      statistic=RunFERobust,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

tax1.all.fe.delta <- tax1.all.fe$t0
tax1.all.fe.delta

tax1.all.fe.CI <- boot.ci(tax1.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.all.fe.CI

boot.ci(tax1.all.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

## plot residuals

# Fit the model
d <- na.omit(fe.all[c("taxpc1","homesteads.pc.lag","access.mean.lag","id")])

fit <- lm(f1, d)

# Obtain predicted and residual values
d$predicted <- predict(fit)
d$residuals <- residuals(fit)

d <- as.data.frame(d)

names(d)[2:3] <- c("Per-capita homesteads (ln)", "Railroad access")

# plot actual/predicted

tax1.fitted <- d %>% 
  gather(key = "iv", value = "x", -taxpc1, -predicted, -residuals, -id) %>%  # Get data into shape
  ggplot(aes(x = x, y = taxpc1)) +  # Note use of `x` here and next line
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  ggtitle("Actual and fitted values from lagged fixed effects model regression") +
  ylab("Log per-capita taxes collected by counties (Tax 1)") +
  xlab("") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  facet_grid(~ iv, scales = "free") +  # Split panels here by `iv`
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(results.directory,"plots/tax1-fitted.png"), tax1.fitted, width=11, height=8.5)

# tax2 + RR access

f1 <- formula(taxpc2 ~ homesteads.pc.lag + access.mean.lag + id) 

# Southern public land state counties 

fe.south <- homestead.tax.long[homestead.tax.long$state.abb %in% c(southern.pub),]

# All years
tax2.south.fe <- boot(data=fe.south,
                      statistic=RunFERobust,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

tax2.south.fe.delta <- tax2.south.fe$t0
tax2.south.fe.delta

tax2.south.fe.CI <- boot.ci(tax2.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.south.fe.CI

boot.ci(tax2.south.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

# Western public land state counties

fe.west <- homestead.tax.long[homestead.tax.long$state.abb %in% c(western.pub),]

# All years

tax2.west.fe <- boot(data=fe.west,
                     statistic=RunFERobust,
                     f1=f1,
                     R=1000,
                     parallel="multicore", ncpus = cores)

tax2.west.fe.delta <- tax2.west.fe$t0
tax2.west.fe.delta

tax2.west.fe.CI <- boot.ci(tax2.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.west.fe.CI

boot.ci(tax2.west.fe, conf=0.95, index=2, type="norm")$normal[2:3] ## RR

# All public land states

fe.all <- homestead.tax.long[homestead.tax.long$state.abb %in% c(pub.states),]

# All years

tax2.all.fe <- boot(data=fe.all,
                    statistic=RunFERobust,
                    f1=f1,
                    R=1000,
                    parallel="multicore", ncpus = cores)

tax2.all.fe.delta <- tax2.all.fe$t0
tax2.all.fe.delta

tax2.all.fe.CI <- boot.ci(tax2.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.all.fe.CI

boot.ci(tax2.all.fe, conf=0.95, index=2, type="norm")$normal[2:3] # rr

# land inequality + RR access

f1 <- formula(aland.gini ~ homesteads.pc.lag + access.mean.lag + id) 

# Southern public land state counties 

fe.south <- homestead.tax.long[homestead.tax.long$state.abb %in% c(southern.pub),]

# All years
aland.gini.south.fe <- boot(data=fe.south,
                      statistic=RunFERobust,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

aland.gini.south.fe.delta <- aland.gini.south.fe$t0
aland.gini.south.fe.delta

aland.gini.south.fe.CI <- boot.ci(aland.gini.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.gini.south.fe.CI

boot.ci(aland.gini.south.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

# Western public land state counties

fe.west <- homestead.tax.long[homestead.tax.long$state.abb %in% c(western.pub),]

# All years

aland.gini.west.fe <- boot(data=fe.west,
                     statistic=RunFERobust,
                     f1=f1,
                     R=1000,
                     parallel="multicore", ncpus = cores)

aland.gini.west.fe.delta <- aland.gini.west.fe$t0
aland.gini.west.fe.delta

aland.gini.west.fe.CI <- boot.ci(aland.gini.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.gini.west.fe.CI

boot.ci(aland.gini.west.fe, conf=0.95, index=2, type="norm")$normal[2:3] ## RR

# All public land states

fe.all <- homestead.tax.long[homestead.tax.long$state.abb %in% c(pub.states),]

# All years

aland.gini.all.fe <- boot(data=fe.all,
                    statistic=RunFERobust,
                    f1=f1,
                    R=1000,
                    parallel="multicore", ncpus = cores)

aland.gini.all.fe.delta <- aland.gini.all.fe$t0
aland.gini.all.fe.delta

aland.gini.all.fe.CI <- boot.ci(aland.gini.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.gini.all.fe.CI

boot.ci(aland.gini.all.fe, conf=0.95, index=2, type="norm")$normal[2:3] # rr

## plot residuals

# Fit the model
d <- na.omit(fe.all[c("aland.gini","homesteads.pc.lag","access.mean.lag","id")])

fit <- lm(f1, d)

# Obtain predicted and residual values
d$predicted <- predict(fit)
d$residuals <- residuals(fit)

d <- as.data.frame(d)

names(d)[2:3] <- c("Per-capita homesteads (ln)", "Railroad access")

# plot actual/predicted

aland.gini.fitted <- d %>% 
  gather(key = "iv", value = "x", -aland.gini, -predicted, -residuals, -id) %>%  # Get data into shape
  ggplot(aes(x = x, y = aland.gini)) +  # Note use of `x` here and next line
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  ggtitle("Actual and fitted values from lagged fixed effects model regression") +
  ylab("Land inequality") +
  xlab("") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  facet_grid(~ iv, scales = "free") +  # Split panels here by `iv`
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(results.directory,"plots/aland-gini-fitted.png"), aland.gini.fitted, width=11, height=8.5)

## Farm value as additional control

source(paste0(homestead.code.directory,"RunFE-robust2.R"))

# Tax1

f1 <- formula(taxpc1 ~ homesteads.pc.lag + farmval.lag + id) 

# Southern public land state counties 

fe.south <- homestead.tax.long[homestead.tax.long$state.abb %in% c(southern.pub),]

# All years
tax1.south.fe <- boot(data=fe.south,
                      statistic=RunFERobust2,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

tax1.south.fe.delta <- tax1.south.fe$t0
tax1.south.fe.delta

tax1.south.fe.CI <- boot.ci(tax1.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.south.fe.CI

boot.ci(tax1.south.fe, conf=0.95, index=2, type="norm")$normal[2:3] # railroad access

# Western public land state counties

fe.west <- homestead.tax.long[homestead.tax.long$state.abb %in% c(western.pub),]

# All years

tax1.west.fe <- boot(data=fe.west,
                     statistic=RunFERobust2,
                     f1=f1,
                     R=1000,
                     parallel="multicore", ncpus = cores)

tax1.west.fe.delta <- tax1.west.fe$t0
tax1.west.fe.delta

tax1.west.fe.CI <- boot.ci(tax1.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.west.fe.CI

boot.ci(tax1.west.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

# All public land states

fe.all <- homestead.tax.long[homestead.tax.long$state.abb %in% c(pub.states),]

# All years

tax1.all.fe <- boot(data=fe.all,
                    statistic=RunFERobust2,
                    f1=f1,
                    R=1000,
                    parallel="multicore", ncpus = cores)

tax1.all.fe.delta <- tax1.all.fe$t0
tax1.all.fe.delta

tax1.all.fe.CI <- boot.ci(tax1.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.all.fe.CI

boot.ci(tax1.all.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

## plot residuals

# Fit the model
d <- na.omit(fe.all[c("taxpc1","homesteads.pc.lag","farmval.lag","id")])

fit <- lm(f1, d)

# Obtain predicted and residual values
d$predicted <- predict(fit)
d$residuals <- residuals(fit)

d <- as.data.frame(d)

names(d)[2:3] <- c("Homesteads (ln)", "Farm value (ln)")

# plot actual/predicted

tax1.fitted <- d %>% 
  gather(key = "iv", value = "x", -taxpc1, -predicted, -residuals, -id) %>%  # Get data into shape
  ggplot(aes(x = x, y = taxpc1)) +  # Note use of `x` here and next line
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  ggtitle("Actual and fitted values from lagged fixed effects model regression") +
  ylab("Log per-capita taxes collected by counties (Tax 1)") +
  xlab("") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  facet_grid(~ iv, scales = "free") +  # Split panels here by `iv`
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(results.directory,"plots/tax1-fitted2.png"), tax1.fitted, width=11, height=8.5)

# tax2 + RR access

f1 <- formula(taxpc2 ~ homesteads.pc.lag + farmval.lag + id) 

# Southern public land state counties 

fe.south <- homestead.tax.long[homestead.tax.long$state.abb %in% c(southern.pub),]

# All years
tax2.south.fe <- boot(data=fe.south,
                      statistic=RunFERobust2,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

tax2.south.fe.delta <- tax2.south.fe$t0
tax2.south.fe.delta

tax2.south.fe.CI <- boot.ci(tax2.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.south.fe.CI

boot.ci(tax2.south.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

# Western public land state counties

fe.west <- homestead.tax.long[homestead.tax.long$state.abb %in% c(western.pub),]

# All years

tax2.west.fe <- boot(data=fe.west,
                     statistic=RunFERobust2,
                     f1=f1,
                     R=1000,
                     parallel="multicore", ncpus = cores)

tax2.west.fe.delta <- tax2.west.fe$t0
tax2.west.fe.delta

tax2.west.fe.CI <- boot.ci(tax2.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.west.fe.CI

boot.ci(tax2.west.fe, conf=0.95, index=2, type="norm")$normal[2:3] ## RR

# All public land states

fe.all <- homestead.tax.long[homestead.tax.long$state.abb %in% c(pub.states),]

# All years

tax2.all.fe <- boot(data=fe.all,
                    statistic=RunFERobust2,
                    f1=f1,
                    R=1000,
                    parallel="multicore", ncpus = cores)

tax2.all.fe.delta <- tax2.all.fe$t0
tax2.all.fe.delta

tax2.all.fe.CI <- boot.ci(tax2.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.all.fe.CI

boot.ci(tax2.all.fe, conf=0.95, index=2, type="norm")$normal[2:3] # rr

## land inequality + farm values

f1 <- formula(aland.gini ~ homesteads.pc.lag + farmval.lag + id) 

# Southern public land state counties 

fe.south <- homestead.tax.long[homestead.tax.long$state.abb %in% c(southern.pub),]

# All years
aland.gini.south.fe <- boot(data=fe.south,
                            statistic=RunFERobust2,
                            f1=f1,
                            R=1000,
                            parallel="multicore", ncpus = cores)

aland.gini.south.fe.delta <- aland.gini.south.fe$t0
aland.gini.south.fe.delta

aland.gini.south.fe.CI <- boot.ci(aland.gini.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.gini.south.fe.CI

boot.ci(aland.gini.south.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

# Western public land state counties

fe.west <- homestead.tax.long[homestead.tax.long$state.abb %in% c(western.pub),]

# All years

aland.gini.west.fe <- boot(data=fe.west,
                           statistic=RunFERobust2,
                           f1=f1,
                           R=1000,
                           parallel="multicore", ncpus = cores)

aland.gini.west.fe.delta <- aland.gini.west.fe$t0
aland.gini.west.fe.delta

aland.gini.west.fe.CI <- boot.ci(aland.gini.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.gini.west.fe.CI

boot.ci(aland.gini.west.fe, conf=0.95, index=2, type="norm")$normal[2:3] ## RR

# All public land states

fe.all <- homestead.tax.long[homestead.tax.long$state.abb %in% c(pub.states),]

# All years

aland.gini.all.fe <- boot(data=fe.all,
                          statistic=RunFERobust2,
                          f1=f1,
                          R=1000,
                          parallel="multicore", ncpus = cores)

aland.gini.all.fe.delta <- aland.gini.all.fe$t0
aland.gini.all.fe.delta

aland.gini.all.fe.CI <- boot.ci(aland.gini.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.gini.all.fe.CI

boot.ci(aland.gini.all.fe, conf=0.95, index=2, type="norm")$normal[2:3] # rr

## plot residuals

# Fit the model
d <- na.omit(fe.all[c("aland.gini","homesteads.pc.lag","farmval.lag","id")])

fit <- lm(f1, d)

# Obtain predicted and residual values
d$predicted <- predict(fit)
d$residuals <- residuals(fit)

d <- as.data.frame(d)

names(d)[2:3] <- c("Per-capita homesteads (ln)", "Farm value (ln)")

# plot actual/predicted

aland.gini.fitted <- d %>% 
  gather(key = "iv", value = "x", -aland.gini, -predicted, -residuals, -id) %>%  # Get data into shape
  ggplot(aes(x = x, y = aland.gini)) +  # Note use of `x` here and next line
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  ggtitle("Actual and fitted values from lagged fixed effects model regression") +
  ylab("Land inequality") +
  xlab("") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  facet_grid(~ iv, scales = "free") +  # Split panels here by `iv`
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(results.directory,"plots/aland-gini-fitted2.png"), aland.gini.fitted, width=11, height=8.5)

## RR access + farm values

f1 <- formula(access.mean ~ homesteads.pc.lag + farmval.lag + id) 

# Southern public land state counties 

fe.south <- homestead.tax.long[homestead.tax.long$state.abb %in% c(southern.pub),]

# All years
access.mean.south.fe <- boot(data=fe.south,
                            statistic=RunFERobust2,
                            f1=f1,
                            R=1000,
                            parallel="multicore", ncpus = cores)

access.mean.south.fe.delta <- access.mean.south.fe$t0
access.mean.south.fe.delta

access.mean.south.fe.CI <- boot.ci(access.mean.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
access.mean.south.fe.CI

boot.ci(access.mean.south.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

# Western public land state counties

fe.west <- homestead.tax.long[homestead.tax.long$state.abb %in% c(western.pub),]

# All years

access.mean.west.fe <- boot(data=fe.west,
                           statistic=RunFERobust2,
                           f1=f1,
                           R=1000,
                           parallel="multicore", ncpus = cores)

access.mean.west.fe.delta <- access.mean.west.fe$t0
access.mean.west.fe.delta

access.mean.west.fe.CI <- boot.ci(access.mean.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
access.mean.west.fe.CI

boot.ci(access.mean.west.fe, conf=0.95, index=2, type="norm")$normal[2:3] ## RR

# All public land states

fe.all <- homestead.tax.long[homestead.tax.long$state.abb %in% c(pub.states),]

# All years

access.mean.all.fe <- boot(data=fe.all,
                          statistic=RunFERobust2,
                          f1=f1,
                          R=1000,
                          parallel="multicore", ncpus = cores)

access.mean.all.fe.delta <- access.mean.all.fe$t0
access.mean.all.fe.delta

access.mean.all.fe.CI <- boot.ci(access.mean.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
access.mean.all.fe.CI

boot.ci(access.mean.all.fe, conf=0.95, index=2, type="norm")$normal[2:3] # rr

## plot residuals

# Fit the model
d <- na.omit(fe.all[c("access.mean","homesteads.pc.lag","farmval.lag","id")])

fit <- lm(f1, d)

# Obtain predicted and residual values
d$predicted <- predict(fit)
d$residuals <- residuals(fit)

d <- as.data.frame(d)

names(d)[2:3] <- c("Per-capita homesteads (ln)", "Farm value (ln)")

# plot actual/predicted

access.mean.fitted <- d %>% 
  gather(key = "iv", value = "x", -access.mean, -predicted, -residuals, -id) %>%  # Get data into shape
  ggplot(aes(x = x, y = access.mean)) +  # Note use of `x` here and next line
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  ggtitle("Actual and fitted values from lagged fixed effects model regression") +
  ylab("Railroad access") +
  xlab("") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  facet_grid(~ iv, scales = "free") +  # Split panels here by `iv`
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(results.directory,"plots/access-fitted2.png"), access.mean.fitted, width=11, height=8.5)
