#####################################
### Descriptive statistics         ###
#####################################

library(scales)
library(ggplot2)
library(dplyr)
library(reshape)
library(reshape2)
library(tidyr)

## Plot patents time series

# By time

patents.tab <- patents %>%
  filter(state_code %in% pub.states) %>% # only public land states
  group_by(date) %>%
  summarise_each(funs(sum),sales,homesteads,total_acres)

colnames(patents.tab) <- c("Date","Sales", "Homesteads", "Total Acres")

patents.tab$Date <- as.POSIXct(patents.tab$Date,format="%m/%d/%Y",tz="UTC")

patents.time <- ggplot(patents.tab, aes(x=Date,y=Sales)) + 
  geom_line(aes(colour='Sales')) +
  geom_line(aes(y=Homesteads,colour='Homesteads')) +
  coord_cartesian(xlim=as.POSIXct(c("01/01/1800","12/31/1950"), format="%m/%d/%Y",tz="UTC")) +
  scale_y_continuous(name="Number of land patents", labels = scales::comma) +
  xlab("") +
  scale_colour_manual(name="Patent type",
                      values=c(Sales="red", Homesteads="blue"))

ggsave(paste0(results.directory,"plots/patents-time.png"), patents.time, width=11, height=8.5)

# By time x region (sales)

cats.sums$date <- as.POSIXct(cats.sums$date,format="%m/%d/%Y",tz="UTC")

sales.state.time <- ggplot(cats.sums, aes( date, sales ,color=cat )) + 
  geom_line() +
  coord_cartesian(xlim=as.POSIXct(c("01/01/1800","12/31/1930"), format="%m/%d/%Y",tz="UTC")) +
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1866-06-01 06:00:00",tz="UTC"))), linetype=2) + # Southern HSA signed
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1876-06-01 06:00:00",tz="UTC"))), linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1889-03-01 06:00:00",tz="UTC"))), linetype=5) +  
  scale_y_continuous(name="Number of sales", labels = scales::comma) +
  xlab("") +
  scale_color_discrete("State type",
                       labels=c("Non-southern public land", "Southern public land"))

ggsave(paste0(results.directory,"plots/sales-state-time.png"), sales.state.time, width=11, height=8.5)

# By time x state (homesteads)

homesteads.state.time <- ggplot(cats.sums, aes( date, homesteads ,color=cat )) + 
  geom_line() +
  coord_cartesian(xlim=as.POSIXct(c("05/20/1862","12/31/1930"), format="%m/%d/%Y",tz="UTC")) +
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1866-06-01 06:00:00",tz="UTC"))), linetype=2) + # Southern HSA signed
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1876-06-01 06:00:00",tz="UTC"))), linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1889-03-01 06:00:00",tz="UTC"))), linetype=5) + 
  scale_y_continuous(name="Number of homesteads", labels = scales::comma) +
  xlab("") +
  scale_color_discrete("Group",
                       labels=c("Outside South", "South"))

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

acres.tab$cat <- ifelse(acres.tab$state_code %in% southern.pub, "Treated", "Control") 

colnames(acres.tab) <- c("Date","State", "Homesteads", "Sales", "cat")

# Create control and treated sums
cats.sums.acres <- acres.tab %>% 
  group_by(Date,cat) %>% 
  summarise_each(funs(sum),Sales,Homesteads) 

cats.sums.acres$Date <- as.POSIXct(cats.sums.acres$Date,format="%m/%d/%Y",tz="UTC")

cats.sums.acres.r <- reshape(data.frame(cats.sums.acres), idvar = "Date", timevar = "cat", direction = "wide")

cats.sums.acres.r[, 2:7][is.na(cats.sums.acres.r[, 2:7])] <- 0 # NAs to 0

sales.acres.time <- ggplot(cats.sums.acres.r, aes( Date, cumsum(Sales.Control))) + 
  geom_line(aes(colour='Control')) +
  geom_line(aes(y=cumsum(Sales.Treated),colour='Treated')) +
  coord_cartesian(xlim=as.POSIXct(c("01/01/1800","12/31/1930"), format="%m/%d/%Y",tz="UTC")) +
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1866-06-01 06:00:00",tz="UTC"))), linetype=2) + # Southern HSA signed
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1876-06-01 06:00:00",tz="UTC"))), linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1889-03-01 06:00:00",tz="UTC"))), linetype=5) +  
  scale_y_continuous(name="Cumulative total acres by cash entry", labels = scales::comma) +
  xlab("") +
  scale_color_discrete(name="State type",
                     #  values=c(Control="red", Treated="blue"),
                       labels=c("Non-southern public land", "Southern public land"))

ggsave(paste0(results.directory,"plots/sales-acres-time.png"), sales.acres.time, width=11, height=8.5)

# By time x state (homesteads)

homesteads.acres.time <- ggplot(cats.sums.acres.r, aes( Date, cumsum(Homesteads.Control))) + 
  geom_line(aes(colour='Control')) +
  geom_line(aes(y=cumsum(Homesteads.Treated),colour='Treated')) +
  coord_cartesian(xlim=as.POSIXct(c("01/01/1862","12/31/1930"), format="%m/%d/%Y",tz="UTC")) +
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1866-06-01 06:00:00",tz="UTC"))), linetype=2) + # Southern HSA signed
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1876-06-01 06:00:00",tz="UTC"))), linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1889-03-01 06:00:00",tz="UTC"))), linetype=5) +  
  scale_y_continuous(name="Cumulative total acres by homestead entry", labels = scales::comma) +
  xlab("") +
  scale_color_discrete(name="State type",
                       #  values=c(Control="red", Treated="blue"),
                       labels=c("Non-southern public land", "Southern public land"))

ggsave(paste0(results.directory,"plots/homesteads-acres-time.png"), homesteads.acres.time, width=11, height=8.5)

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
  summarise_each(funs(mean(., na.rm = TRUE)),rev.pc,exp.pc,ed.pc) 

# By time x region (sales)

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

ed.pc.state.time <- ggplot(cats.funds.plot, aes( year, ed.pc ,color=cat )) + 
  coord_cartesian(xlim=c(1800,1940)) +
  geom_smooth(span=0.3, se = FALSE) +
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="State government education spending, per-capita ($)") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/ed-pc-state-time.png"), ed.pc.state.time, width=11, height=8.5)


## Plot railroads time-series

rr.plot <- rr.inter

rr.plot$cat <- NA
rr.plot$cat[rr.plot$state %in% southern.pub] <- "Southern public land"
rr.plot$cat[rr.plot$state %in% southern.state] <- "Southern state land"
rr.plot$cat[rr.plot$state %in% setdiff(pub.states,southern.pub)] <- "Non-southern public land"
rr.plot$cat[rr.plot$state %in% setdiff(state.land.states,southern.state)] <- "Non-southern state land"

rr.plot$year <- rr.plot$InOpBy

cats.rr.plot <- rr.plot %>% 
  filter(!is.na(cat)) %>%  # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),access) 

# By time x region 

access.state.time <- ggplot(cats.rr.plot, aes( year, access ,color=cat )) + 
  geom_smooth(span=0.1, se = FALSE) +
  coord_cartesian(xlim=c(1826,1911)) +
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="Railroad access") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/access-state-time.png"), access.state.time, width=11, height=8.5)
