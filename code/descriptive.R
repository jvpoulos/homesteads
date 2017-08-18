#####################################
### Descriptive statistics         ###
#####################################

library(scales)
library(ggplot2)
library(dplyr)
library(reshape)
library(reshape2)

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