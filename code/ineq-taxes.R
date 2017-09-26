############################################
# State capacity vs. railroad access plots #
############################################

library(ggplot2)
library(reshape2)
library(dplyr)
library(mgcv)
library(wesanderson)

source(paste0(code.directory,'LmEq.R')) 

# Reshape taxpc

taxpc.m <- melt(taxpc[c("fips","year","taxpc1","taxpc2")], id.vars = c("fips","year"),
                              variable.name = "variable",
                              value.name = "value")

taxpc.m$year <- (taxpc$year %/% 10) * 10  # floor year to decade 

# merge taxpc with county-level census data

ineq.taxpc <- merge(census.ts,  
                  taxpc.m, 
                  by = c("fips","year"))

# Scatter plot (each point state/year observation) 

ineq.capacity <- ggplot(ineq.taxpc, aes(aland.gini, value, color = factor(variable))) + 
  geom_point() +
  geom_smooth(data=subset(ineq.taxpc, variable=="taxpc1"),se=TRUE, colour=wes_palette("Darjeeling")[4],size=1) +
  annotate("text", x = 0.6, y = 25, label = LmEq(gam(value ~ aland.gini, data=subset(ineq.taxpc, variable=="taxpc1"))), colour=wes_palette("Darjeeling")[4], size = 4, parse=TRUE) +
  geom_smooth(data=subset(ineq.taxpc, variable=="taxpc2"),se=TRUE, colour=wes_palette("Darjeeling")[5],size=1) +
  annotate("text", x = 0.5, y = 30, label = LmEq(gam(value ~ aland.gini, data=subset(ineq.taxpc, variable=="taxpc2"))), colour=wes_palette("Darjeeling")[5], size = 4, parse=TRUE) +
  coord_cartesian(ylim=c(0,150),
                  xlim=c(0.4,1)) +
  scale_colour_manual(name="Measure",
                      values=c(taxpc1=wes_palette("Darjeeling")[4], taxpc2=wes_palette("Darjeeling")[5]),
                      label=c("Tax1",
                              "Tax2")) +
  ylab("Per-capita taxes") +
  xlab("Land inequality") 

ggsave(paste0(results.directory,"plots/ineq-taxpc.png"), ineq.capacity, width=11, height=8.5)