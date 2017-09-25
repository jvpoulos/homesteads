#####################################
### Descriptive statistics         ###
#####################################

library(scales)
library(ggplot2)
library(dplyr)
library(reshape)
library(reshape2)
library(tidyr)

## Plot census time-series

census.plot <- census.ts

icpsr<- read_csv(paste0(data.directory,"icpsr-state-code.csv"), col_names = FALSE)# get icpsr state code crosswalk
colnames(icpsr) <- c("state.code","state.name")

census.plot <- merge(census.plot, icpsr, all.x=TRUE, by.x="state", by.y="state.code")
census.plot <- subset(census.plot, !is.na(state.name)) # rm D.C. & territories

census.plot$cat <- NA
census.plot$cat[census.plot$state.name %in% southern.pub] <- "Southern public land"
census.plot$cat[census.plot$state.name %in% southern.state] <- "Southern state land"
census.plot$cat[census.plot$state.name %in% setdiff(pub.states,southern.pub)] <- "Non-southern public land"
census.plot$cat[census.plot$state.name %in% setdiff(state.land.states,southern.state)] <- "Non-southern state land"

cats.census.plot <- census.plot %>% 
  filter(!is.na(cat)) %>%  
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),aland.gini,output,tenancy,wages)

# inequality

aland.gini.state.time <- ggplot(cats.census.plot[!is.na(cats.census.plot$aland.gini),], aes( year, aland.gini ,color=cat )) + 
  geom_line() +
  #geom_smooth(span=0.1, se = FALSE) +
  #coord_cartesian(xlim=c(1800,1975)) +
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Adjusted land Gini") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/aland-gini-state-time.png"), aland.gini.state.time, width=11, height=8.5)

# output

output.state.time <- ggplot(cats.census.plot[!is.na(cats.census.plot$output),], aes( year, output ,color=cat )) + 
  geom_line() +
  #geom_smooth(span=0.1, se = FALSE) +
  #coord_cartesian(xlim=c(1800,1975)) +
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Farm output") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/output-state-time.png"), output.state.time, width=11, height=8.5)

wages.state.time <- ggplot(cats.census.plot[!is.na(cats.census.plot$wages),], aes( year, wages ,color=cat )) + 
  geom_line() +
  #geom_smooth(span=0.1, se = FALSE) +
  #coord_cartesian(xlim=c(1800,1975)) +
  #geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  #geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Farm wages") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/wages-state-time.png"), wages.state.time, width=11, height=8.5)

# tenancy

tenancy.state.time <- ggplot(cats.census.plot[!is.na(cats.census.plot$tenancy) & cats.census.plot$year<=1950,], aes( year, tenancy ,color=cat )) + 
  geom_line() +
  #geom_smooth(span=0.1, se = FALSE) +
 # coord_cartesian(xlim=c(1880,1950)) +
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Farm tenancy") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/tenancy-state-time.png"), tenancy.state.time, width=11, height=8.5)


## Plot capacity time-series

funds.plot <- funds

funds.plot$cat <- NA
funds.plot$cat[funds.plot$state %in% southern.pub] <- "Southern public land"
funds.plot$cat[funds.plot$state %in% southern.state] <- "Southern state land"
funds.plot$cat[funds.plot$state %in% setdiff(pub.states,southern.pub)] <- "Non-southern public land"
funds.plot$cat[funds.plot$state %in% setdiff(state.land.states,southern.state)] <- "Non-southern state land"

cats.funds.plot <- funds.plot %>% 
  filter(!is.na(cat)) %>%  # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),rev.pc,exp.pc) 

# By time x region

rev.pc.state.time <- ggplot(cats.funds.plot, aes( year, rev.pc ,color=cat )) + 
  geom_smooth(span=0.1, se = FALSE) +
  coord_cartesian(xlim=c(1800,1975)) +
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="State government total revenue, per-capita ($)") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/rev-pc-state-time.png"), rev.pc.state.time, width=11, height=8.5)

exp.pc.state.time <- ggplot(cats.funds.plot, aes( year, exp.pc ,color=cat )) + 
  geom_smooth(span=0.1, se = FALSE) +
  coord_cartesian(xlim=c(1800,1975)) +
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="State government total expenditure, per-capita ($)") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/exp-pc-state-time.png"), exp.pc.state.time, width=11, height=8.5)

## Plot railroads time-series

rr.plot <- rr.inter.m

rr.plot$cat <- NA
rr.plot$cat[rr.plot$state %in% southern.pub] <- "Southern public land"
rr.plot$cat[rr.plot$state %in% southern.state] <- "Southern state land"
rr.plot$cat[rr.plot$state %in% setdiff(pub.states,southern.pub)] <- "Non-southern public land"
rr.plot$cat[rr.plot$state %in% setdiff(state.land.states,southern.state)] <- "Non-southern state land"

rr.plot$year <- rr.plot$InOpBy

cats.rr.plot <- rr.plot %>% 
  filter(!is.na(cat)) %>%  # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),track2) 

# By time x region 

track.state.time <- ggplot(cats.rr.plot, aes( year, track2 ,color=cat )) + 
  geom_smooth(span=0.1, se = FALSE) +
  coord_cartesian(xlim=c(1826,1911)) +
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Railroad track miles per square mile") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/track-state-time.png"), track.state.time, width=11, height=8.5)
