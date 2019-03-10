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
                              variable.name = "variable",
                              value.name = "value")

funds.m$value <- log(funds.m$value +.Machine$double.eps)
funds.m$year <- (funds.m$year %/% 10) * 10  # floor year to decade 

# Create ineq. lags

TLag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}

census.ts.lag <- census.ts.state %>%
  filter(state %in% pub.states) %>% # pub land state counties only
  group_by(state) %>% 
  mutate(aland.gini.lag = TLag(aland.gini, 10, time = year))

# merge funds with state-level census data (census.ts.state)

ineq.funds <- merge(census.ts.lag,  
                  funds.m, 
                  by = c("state","year"))

# Scatter plot (each point state/year observation) 

ineq.capacity <- ggplot(ineq.funds, aes(aland.gini, value, color = factor(variable))) + 
  geom_point(alpha=0.5) +
  geom_smooth(data=subset(ineq.funds, variable=="rev.pc"),se=TRUE, colour=wes_palette("Darjeeling1")[1],size=1) +
 # annotate("text", x = 0.79, y = 3, label = LmEq(gam(value ~ aland.gini, data=subset(ineq.funds, variable=="rev.pc"))), colour=wes_palette("Darjeeling1")[1], size = 4, parse=TRUE) +
  geom_smooth(data=subset(ineq.funds, variable=="exp.pc"),se=TRUE, colour=wes_palette("Darjeeling1")[2],size=1) +
 # annotate("text", x = 0.73, y =2, label = LmEq(gam(value ~ aland.gini, data=subset(ineq.funds, variable=="exp.pc"))), colour=wes_palette("Darjeeling1")[2], size = 4, parse=TRUE) +
  coord_cartesian(ylim=c(0,8),xlim=c(0.68,0.99)) +
  scale_colour_manual(name="Outcome",
                      values=c(exp.pc=wes_palette("Darjeeling1")[2], rev.pc=wes_palette("Darjeeling1")[1]),
                      label=c("Expenditure",
                              "Revenue")) +
  ggtitle("Land inequality vs. state government finances") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Log per-capita measures") +
  xlab("Lagged land inequality") 

ggsave(paste0(results.directory,"plots/ineq-capacity.png"), ineq.capacity, width=11, height=8.5)