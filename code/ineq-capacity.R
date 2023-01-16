############################################
# State capacity vs. land inequality plots #
############################################

library(ggplot2)
library(reshape2)
library(dplyr)
library(mgcv)
library(wesanderson)

source(paste0(code.directory,'utils.R')) 

# Reshape funds

funds.m <- melt(funds[c("state","year","rev.pc","exp.pc")], id.vars = c("state","year"),
                              variable.name = "variable",
                              value.name = "value")

funds.m$value <- log(funds.m$value +.Machine$double.eps)
funds.m$year <- (funds.m$year %/% 10) * 10  # floor year to decade 

# Create ineq. lags

census.ts.lag <- census.ts.state %>%
  group_by(state) %>% 
  mutate(aland.gini.lag = TLag(aland.gini, 10, time = year))

census.ts.lag$treated <- ifelse(census.ts.lag$state %in% pub.states,"Treated","Control")

# merge funds with state-level census data (census.ts.state)

ineq.funds <- merge(census.ts.lag,  
                  funds.m, 
                  by = c("state","year"))

# Scatter plot (each point state/year observation) 

ineq.capacity <- ggplot(ineq.funds, aes(aland.gini, value, color = factor(variable))) + 
  facet_wrap(factor(treated) ~.) +
  geom_point(alpha=0.5) +
  geom_smooth(data=subset(ineq.funds, variable=="rev.pc"),se=TRUE, colour=wes_palette("Darjeeling1")[1],size=1) +
  geom_smooth(data=subset(ineq.funds, variable=="exp.pc"),se=TRUE, colour=wes_palette("Darjeeling1")[2],size=1) +
 # coord_cartesian(ylim=c(-3,7)) +
  scale_colour_manual(name="Outcome",
                      values=c(exp.pc=wes_palette("Darjeeling1")[2], rev.pc=wes_palette("Darjeeling1")[1]),
                      label=c("Expenditure",
                              "Revenue")) +
  #ggtitle("Land inequality vs. state government finances") +
  ylab("Log per-capita expenditure or revenue (1928$)") +
  xlab("Lagged land inequality")  +
  theme(legend.position="right")+
  theme(plot.title = element_text(hjust = 0.5, family="serif", size=14)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(legend.key=element_blank()) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background

ggsave(paste0(results.directory,"plots/ineq-capacity.png"), ineq.capacity, scale=1.25)