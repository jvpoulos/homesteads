############################################
# State capacity vs. land inequality plots #
############################################

library(ggplot2)
library(reshape2)
library(dplyr)
library(mgcv)
library(wesanderson)

source(paste0(code.directory,'LmEq.R')) 

# Reshape funds

funds.m <- melt(funds[c("state","year","rev.pc","exp.pc")], id.vars = c("state","year"),
                              variable.name = "category",
                              value.name = "value")

funds.m$year <- (funds$year %/% 10) * 10  # floor year to decade 

# merge funds with state-level census data (census.ts.state)

ineq.funds <- merge(census.ts.state,  
                  funds.m, 
                  by = c("state","year"))

# Scatter plot (each point state/year observation) 

ineq.capacity <- ggplot(ineq.funds, aes(aland.gini, value, color = factor(variable))) + 
  geom_point() +
  geom_smooth(data=subset(ineq.funds, variable=="rev.pc"),se=TRUE, colour=wes_palette("Darjeeling")[1],size=1) +
  annotate("text", x = 0.75, y = 7, label = LmEq(gam(value ~ aland.gini, data=subset(ineq.funds, variable=="rev.pc"))), colour=wes_palette("Darjeeling")[1], size = 4, parse=TRUE) +
  geom_smooth(data=subset(ineq.funds, variable=="exp.pc"),se=TRUE, colour=wes_palette("Darjeeling")[2],size=1) +
  annotate("text", x = 0.77, y = 5, label = LmEq(gam(value ~ aland.gini, data=subset(ineq.funds, variable=="exp.pc"))), colour=wes_palette("Darjeeling")[2], size = 4, parse=TRUE) +
  coord_cartesian(ylim=c(0,75)) +
  scale_colour_manual(name="Measure",
                      values=c(exp.pc=wes_palette("Darjeeling")[2], rev.pc=wes_palette("Darjeeling")[1]),
                      label=c("Per-capita revenues",
                              "Per-capita expenditures")) +
  ylab("Per-capita revenues and expenditures") +
  xlab("Land inequality") 

ggsave(paste0(results.directory,"plots/ineq-capacity.png"), ineq.capacity, width=11, height=8.5)