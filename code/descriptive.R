#####################################
### Descriptive statistics         ###
#####################################

library(scales)
library(ggplot2)
library(dplyr)
library(reshape)
library(reshape2)
library(tidyr)

## Plot per-capita patents time series

# By time

patents.tab <- patents.sum %>%
  filter(state_code %in% pub.states) %>% # only public land states
  group_by(year) %>%
  summarise_each(funs(mean),sales.pc, homesteads.pc)

colnames(patents.tab) <- c("Year","Sales", "Homesteads")

patents.time <- ggplot(patents.tab, aes(x=Year,y=Sales)) + 
  geom_smooth(aes(colour='Sales'),span=0.3) +
  geom_smooth(aes(y=Homesteads,colour='Homesteads'),span=0.3) +
  geom_vline(xintercept=1862, linetype=1) +  
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
 # coord_cartesian(xlim=as.POSIXct(c("01/01/1800","12/31/1950"), format="%m/%d/%Y",tz="UTC")) +
  scale_y_continuous(name="Log per-capita land patents") +
  xlab("") +
  scale_colour_manual(name="Patent type",
                      values=c(Sales="red", Homesteads="blue"))

ggsave(paste0(results.directory,"plots/patents-time.png"), patents.time, width=11, height=8.5)

# By time x region (sales)

patents.sum$cat <- NA
patents.sum$cat[patents.sum$state_code %in% southern.pub] <- "Southern.public.land"
patents.sum$cat[patents.sum$state_code %in% southern.state] <- "Southern.state.land"
patents.sum$cat[patents.sum$state_code %in% setdiff(pub.states,southern.pub)] <- "Western.public.land"
patents.sum$cat[patents.sum$state_code %in% setdiff(state.land.states,southern.state)] <- "Northeastern.state.land"

# cats.sums.foo <- patents.sum %>%
#   filter(state_code %in% pub.states) %>% # only public land states
#   group_by(cat) %>%
#   summarise_each(funs(mean),homesteads,ns.pop)

cats.sums <- patents.sum %>%
  filter(state_code %in% pub.states) %>% # only public land states
  group_by(year,cat) %>%
  summarise_each(funs(mean),sales.pc, homesteads.pc)

sales.state.time <- ggplot(cats.sums, aes( year, sales.pc ,color=cat )) + 
  geom_smooth(span=0.1) +
#  coord_cartesian(xlim=as.POSIXct(c("01/01/1800","12/31/1930"), format="%m/%d/%Y",tz="UTC")) +
  geom_vline(xintercept=1862, linetype=1) +  
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Log per-capita sales") +
  xlab("") +
  scale_color_discrete(name="State type",
                       #  values=c(Control="red", Treated="blue"),
                       labels=c("Southern public land", "Western public land"))

ggsave(paste0(results.directory,"plots/sales-state-time.png"), sales.state.time, width=11, height=8.5)

# By time x state (homesteads)

homesteads.state.time <- ggplot(cats.sums, aes( year, homesteads.pc ,color=cat )) + 
  geom_smooth(span=0.1) +
  #  coord_cartesian(xlim=as.POSIXct(c("01/01/1800","12/31/1930"), format="%m/%d/%Y",tz="UTC")) +
  geom_vline(xintercept=1862, linetype=1) +  
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Log per-capita homesteads") +
  xlab("") +
  scale_color_discrete(name="State type",
                       #  values=c(Control="red", Treated="blue"),
                       labels=c("Southern public land", "Western public land"))

ggsave(paste0(results.directory,"plots/homesteads-state-time.png"), homesteads.state.time, width=11, height=8.5)

## Plot acres time series

# By time

acres.tab <- patents %>%
  filter(state_code %in% pub.states) %>% # only public land states
  filter(authority_code %in% c(251101, 272002)) %>% # homesteads, sales
  group_by(date,authority_code) %>%
  summarise_each(funs(sum),total_acres) 

acres.tab <- spread(acres.tab, key = authority_code, value = total_acres)

colnames(acres.tab) <- c("Date","Homesteads", "Sales")

acres.tab$Date <- as.POSIXct(acres.tab$Date,format="%m/%d/%Y",tz="UTC")

acres.tab$Homesteads[is.na(acres.tab$Homesteads)] <- 0 # NAs to 0
acres.tab$Sales[is.na(acres.tab$Sales)] <- 0 # NAs to 0

acres.time <- ggplot(acres.tab, aes(x=Date,y=cumsum(Sales))) + 
  geom_line(aes(colour='Sales')) +
  geom_line(aes(y=cumsum(Homesteads),colour='Homesteads')) +
  coord_cartesian(xlim=as.POSIXct(c("01/01/1800","12/31/1950"), format="%m/%d/%Y",tz="UTC")) +
  scale_y_continuous(name="Cumulative acres", labels = scales::comma) +
  xlab("") +
  scale_colour_manual(name="Patent type",
                      values=c(Sales="red", Homesteads="blue"))

ggsave(paste0(results.directory,"plots/acres-time.png"), acres.time, width=11, height=8.5)

# By time x region (sales)

acres.tab <- patents %>%
  filter(state_code %in% pub.states) %>% # only public land states
  filter(authority_code %in% c(251101, 272002)) %>% # homesteads, sales
  group_by(date,state_code,authority_code) %>%
  summarise_each(funs(sum),total_acres) 

acres.tab <- spread(acres.tab, key = authority_code, value = total_acres)

acres.tab$cat <- NA
acres.tab$cat[acres.tab$state_code %in% southern.pub] <- "Southern.public.land"
acres.tab$cat[acres.tab$state_code %in% southern.state] <- "Southern.state.land"
acres.tab$cat[acres.tab$state_code %in% setdiff(pub.states,southern.pub)] <- "Western.public.land"
acres.tab$cat[acres.tab$state_code %in% setdiff(state.land.states,southern.state)] <- "Northeastern.state.land"

colnames(acres.tab) <- c("Date","State", "Homesteads", "Sales", "cat")

# Create control and treated sums
cats.sums.acres <- acres.tab %>% 
  group_by(Date,cat) %>% 
  summarise_each(funs(sum),Sales,Homesteads) 

cats.sums.acres$Date <- as.POSIXct(cats.sums.acres$Date,format="%m/%d/%Y",tz="UTC")

cats.sums.acres.r <- reshape(data.frame(cats.sums.acres), idvar = "Date", timevar = "cat", direction = "wide")

cats.sums.acres.r[, 2:5][is.na(cats.sums.acres.r[, 2:5])] <- 0 # NAs to 0

sales.acres.time <- ggplot(cats.sums.acres.r, aes( Date, cumsum(Sales.Western.public.land))) + 
  geom_line(aes(colour='Western public land')) +
  geom_line(aes(y=cumsum(Sales.Southern.public.land),colour='Southern public land')) +
  coord_cartesian(xlim=as.POSIXct(c("01/01/1800","12/31/1930"), format="%m/%d/%Y",tz="UTC")) +
  geom_vline(xintercept=1862, linetype=1) +  
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Cumulative total acres by cash entry", labels = scales::comma) +
  xlab("") +
  scale_color_discrete(name="State type",
                       #  values=c(Control="red", Treated="blue"),
                       labels=c("Southern public land", "Western public land"))

ggsave(paste0(results.directory,"plots/sales-acres-time.png"), sales.acres.time, width=11, height=8.5)

# By time x state (homesteads)

homesteads.acres.time <- ggplot(cats.sums.acres.r, aes( Date, cumsum(Homesteads.Western.public.land))) + 
  geom_line(aes(colour='Western public land')) +
  geom_line(aes(y=cumsum(Homesteads.Southern.public.land),colour='Southern public land')) +
  coord_cartesian(xlim=as.POSIXct(c("01/01/1800","12/31/1930"), format="%m/%d/%Y",tz="UTC")) +
  geom_vline(xintercept=1862, linetype=1) +  
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Cumulative total acres by homestead", labels = scales::comma) +
  xlab("") +
  scale_color_discrete(name="State type",
                       #  values=c(Control="red", Treated="blue"),
                       labels=c("Southern public land", "Western public land"))

ggsave(paste0(results.directory,"plots/homesteads-acres-time.png"), homesteads.acres.time, width=11, height=8.5)

## Plot census time-series

census.plot <- census.ts

data.directory <-"~/Dropbox/github/land-reform/data/"
icpsr<- read_csv(paste0(data.directory,"icpsr-state-code.csv"), col_names = FALSE)# get icpsr state code crosswalk
colnames(icpsr) <- c("state.code","state.name")

census.plot <- merge(census.plot, icpsr, all.x=TRUE, by.x="state", by.y="state.code")
census.plot <- subset(census.plot, !is.na(state.name)) # rm D.C. & territories

census.plot$cat <- NA
census.plot$cat[census.plot$state.name %in% southern.pub] <- "Southern public land"
census.plot$cat[census.plot$state.name %in% southern.state] <- "Southern state land"
census.plot$cat[census.plot$state.name %in% setdiff(pub.states,southern.pub)] <- "Western public land"
census.plot$cat[census.plot$state.name %in% setdiff(state.land.states,southern.state)] <- "Eastern state land"

cats.census.plot <- census.plot %>% 
  filter(!is.na(cat)) %>%  
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),aland.gini,output,tenancy,wages,farmsize)

# inequality

aland.gini.state.time <- ggplot(cats.census.plot[!is.na(cats.census.plot$aland.gini),], aes( year, aland.gini ,color=cat )) + 
  geom_line() +
  #geom_smooth(span=0.1, se = FALSE) +
  #coord_cartesian(xlim=c(1800,1975)) +
  geom_vline(xintercept=1862, linetype=1) +  
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
 # geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Land inequality") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/aland-gini-state-time.png"), aland.gini.state.time, width=11, height=8.5)

# tenancy

tenancy.state.time <- ggplot(cats.census.plot[!is.na(cats.census.plot$tenancy) & cats.census.plot$year<=1950,], aes( year, tenancy ,color=cat )) + 
  geom_line() +
  #geom_smooth(span=0.1, se = FALSE) +
 # coord_cartesian(xlim=c(1880,1950)) +
  scale_y_continuous(name="Farm tenancy") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/tenancy-state-time.png"), tenancy.state.time, width=11, height=8.5)

# wages

wages.state.time <- ggplot(cats.census.plot[!is.na(cats.census.plot$wages) & cats.census.plot$year<=1950,], 
                           aes( year, wages ,color=cat )) + 
  geom_line() +
  # coord_cartesian(xlim=c(1880,1950)) +
  scale_y_continuous(name="Farm wages (ln)") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/wages-state-time.png"), wages.state.time, width=11, height=8.5)

# output

output.state.time <- ggplot(cats.census.plot[!is.na(cats.census.plot$output) & cats.census.plot$year<=1950,], aes( year, output ,color=cat )) + 
  geom_line() +
  #geom_smooth(span=0.1, se = FALSE) +
  # coord_cartesian(xlim=c(1880,1950)) +
  scale_y_continuous(name="Farm output (ln)") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/output-state-time.png"), output.state.time, width=11, height=8.5)

# farmsize

farmsize.state.time <- ggplot(cats.census.plot[!is.na(cats.census.plot$farmsize) & cats.census.plot$year<=1950,], aes( year, farmsize ,color=cat )) + 
  geom_line() +
  #geom_smooth(span=0.1, se = FALSE) +
  # coord_cartesian(xlim=c(1880,1950)) +
  scale_y_continuous(name="Farm farmsize (ln)") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/farmsize-state-time.png"), farmsize.state.time, width=11, height=8.5)

## Avg. farm values

census.plot <- homestead.tax.long

census.plot$cat <- NA
census.plot$cat[census.plot$state.abb %in% southern.pub] <- "Southern public land"
census.plot$cat[census.plot$state.abb %in% southern.state] <- "Southern state land"
census.plot$cat[census.plot$state.abb %in% setdiff(pub.states,southern.pub)] <- "Western public land"
census.plot$cat[census.plot$state.abb %in% setdiff(state.land.states,southern.state)] <- "Eastern state land"

cats.census.plot <- census.plot %>% 
  filter(!is.na(cat)) %>%  
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),farmval)

farmval.state.time <- ggplot(cats.census.plot[!is.na(cats.census.plot$farmval) & cats.census.plot$year<=1950,], aes( year, farmval ,color=cat )) + 
  geom_line() +
  #geom_smooth(span=0.1, se = FALSE) +
  # coord_cartesian(xlim=c(1880,1950)) +
  scale_y_continuous(name="Farm value (ln)") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/farmval-state-time.png"), farmval.state.time, width=11, height=8.5)

## Plot taxes time-series

taxpc.plot <- taxpc

taxpc.plot$cat <- NA
taxpc.plot$cat[taxpc.plot$state.abb %in% southern.pub] <- "Southern public land"
taxpc.plot$cat[taxpc.plot$state.abb %in% southern.state] <- "Southern state land"
taxpc.plot$cat[taxpc.plot$state.abb %in% setdiff(pub.states,southern.pub)] <- "Western public land"
taxpc.plot$cat[taxpc.plot$state.abb %in% setdiff(state.land.states,southern.state)] <- "Northeastern state land"

cats.taxpc.plot <- taxpc.plot %>% 
  filter(!is.na(cat)) %>%  
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),taxpc2,taxpc1) 

# cats.taxpc.plot.foo <- taxpc.plot %>% 
#   filter(!is.na(cat)) %>%  
#   group_by(cat) %>% 
#   summarise_each(funs(mean(., na.rm = TRUE)),taxpc2,taxpc1) 

# taxpc1

taxpc1.state.time <- ggplot(cats.taxpc.plot[!is.na(cats.taxpc.plot$taxpc1),], aes( year, taxpc1 ,color=cat )) + 
  geom_line() +
 # coord_cartesian(xlim=c(1800,1975)) +
 # geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Log per-capita taxes collected by counties (Tax 1)") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/tax1-state-time.png"), taxpc1.state.time, width=11, height=8.5)

# taxpc2

taxpc2.state.time <- ggplot(cats.taxpc.plot[!is.na(cats.taxpc.plot$taxpc2),], aes( year, taxpc2 ,color=cat )) + 
  geom_line() +
  # coord_cartesian(xlim=c(1800,1975)) +
 # geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Log per-capita taxes collected by all local governments within the county (Tax 2)") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/tax2-state-time.png"), taxpc2.state.time, width=11, height=8.5)

## Plot railroads time-series

rr.plot <- rr.inter.m

rr.plot$cat <- NA
rr.plot$cat[rr.plot$state %in% southern.pub] <- "Southern public land"
rr.plot$cat[rr.plot$state %in% southern.state] <- "Southern state land"
rr.plot$cat[rr.plot$state %in% setdiff(pub.states,southern.pub)] <- "Western public land"
rr.plot$cat[rr.plot$state %in% setdiff(state.land.states,southern.state)] <- "Northeastern state land"

rr.plot$year <- rr.plot$InOpBy

cats.rr.plot <- rr.plot %>% 
  filter(!is.na(cat)) %>%  # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),access) 

# By time x region 

track.state.time <- ggplot(cats.rr.plot, aes( year, access ,color=cat )) + 
  geom_smooth(span=0.1, se = FALSE) +
  coord_cartesian(xlim=c(1826,1911)) +
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Railroad access") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/access-state-time.png"), track.state.time, width=11, height=8.5)
