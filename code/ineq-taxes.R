############################################
# Fiscal capacity vs. inequality plots #
############################################

library(ggplot2)
library(reshape2)
library(dplyr)
library(mgcv)
library(wesanderson)

source(paste0(code.directory,'LmEq.R')) 

# Reshape taxpc

taxpc.m <- melt(taxpc[taxpc$state.abb %in% pub.states,][c("fips","year","taxpc1","taxpc2")], id.vars = c("fips","year"),
                              variable.name = "variable",
                              value.name = "value")

taxpc.m$year <- (taxpc.m$year %/% 10) * 10  # floor year to decade 

# get fips
fips.codes <- data.frame(read_excel(paste0(lr.data.directory,'US_FIPS_Codes.xls'), skip = 1))
fips.codes$FIPS.State <- as.numeric(fips.codes$FIPS.State)
fips.codes$FIPS.County <- as.numeric(fips.codes$FIPS.County)

fips.codes <- fips.codes[!duplicated(fips.codes$FIPS.State),][c("State","FIPS.State")]

census.ts.d <- merge(census.ts, fips.codes, by.x =c("state"), by.y=c("FIPS.State"), all.x=TRUE)

census.ts.d$state.abb <- setNames(state.abb, state.name)[census.ts.d$State]

# Create ineq. lags

TLag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}

census.ts.lag <- census.ts.d %>%
  filter(state.abb %in% pub.states) %>% # pub land state counties only
  group_by(fips) %>% 
  mutate(aland.gini.lag = TLag(aland.gini, 10, time = year))

# merge taxpc with county-level census data

ineq.taxpc <- merge(census.ts.lag,  
                  taxpc.m, 
                  by = c("fips","year"))

# Scatter plot (each point state/year observation) 

ineq.capacity <- ggplot(ineq.taxpc, aes(aland.gini.lag, value, color = factor(variable))) + 
  geom_point(alpha=0.5) +
  geom_smooth(data=subset(ineq.taxpc, variable=="taxpc1"),se=TRUE, colour=wes_palette("Darjeeling")[4],size=1) +
  annotate("text", x = 0.6, y = -0.5, label = LmEq(gam(value ~ aland.gini, data=subset(ineq.taxpc, variable=="taxpc1"))), colour=wes_palette("Darjeeling")[4], size = 4, parse=TRUE) +
  geom_smooth(data=subset(ineq.taxpc, variable=="taxpc2"),se=TRUE, colour=wes_palette("Darjeeling")[5],size=1) +
  annotate("text", x = 0.6, y = 5, label = LmEq(gam(value ~ aland.gini, data=subset(ineq.taxpc, variable=="taxpc2"))), colour=wes_palette("Darjeeling")[5], size = 4, parse=TRUE) +
  coord_cartesian(ylim=c(-2,6),
                  xlim=c(0.45,1)) +
  scale_colour_manual(name="Measure",
                      values=c(taxpc1=wes_palette("Darjeeling")[4], taxpc2=wes_palette("Darjeeling")[5]),
                      label=c("Tax1",
                              "Tax2")) +
  ggtitle("Land inequality vs. fiscal capacity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Log per-capita taxes") +
  xlab("Lagged land inequality") 

ggsave(paste0(results.directory,"plots/ineq-taxpc.png"), ineq.capacity, width=11, height=8.5)