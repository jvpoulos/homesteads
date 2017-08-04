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
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1866-06-01 06:00:00",tz="UTC"))), linetype=4) + # Southern HSA signed
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1876-06-01 06:00:00",tz="UTC"))), linetype=4) + # Southern HSA repealed
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1888-06-01 06:00:00",tz="UTC"))), linetype=1) +  
  scale_y_continuous(name="Number of sales", labels = scales::comma) +
  xlab("") +
  scale_color_discrete("Group",
                       labels=c("Outside South", "South"))

ggsave(paste0(results.directory,"plots/sales-state-time.png"), sales.state.time, width=11, height=8.5)

# By time x state (homesteads)

homesteads.state.time <- ggplot(cats.sums, aes( date, homesteads ,color=cat )) + 
  geom_line() +
  coord_cartesian(xlim=as.POSIXct(c("05/20/1862","12/31/1930"), format="%m/%d/%Y",tz="UTC")) +
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1866-06-01 06:00:00",tz="UTC"))), linetype=4) + # Southern HSA signed
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1876-06-01 06:00:00",tz="UTC"))), linetype=4) + # Southern HSA repealed
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1888-06-01 06:00:00",tz="UTC"))), linetype=1) + 
  scale_y_continuous(name="Number of homesteads", labels = scales::comma) +
  xlab("") +
  scale_color_discrete("Group",
                       labels=c("Outside South", "South"))

ggsave(paste0(results.directory,"plots/homesteads-state-time.png"), homesteads.state.time, width=11, height=8.5)
