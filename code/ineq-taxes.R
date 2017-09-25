############################################
# State capacity vs. railroad access plots #
############################################

library(ggplot2)
library(reshape2)
library(dplyr)
library(mgcv)
library(wesanderson)

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

LmEq <- function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            p.val = format(summary(m)[[4]][2], digits = 1));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(p)~"="~p.val,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(p)~"="~p.val,l)    
  }
  
  as.character(as.expression(eq));                 
}

ineq.capacity <- ggplot(ineq.funds, aes(aland.gini, value, color = factor(category))) + 
  geom_point() +
  geom_smooth(data=subset(ineq.funds, category=="rev.pc"),se=TRUE, colour=wes_palette("Darjeeling")[1],size=1) +
  annotate("text", x = 0.75, y = 7, label = LmEq(gam(value ~ aland.gini, data=subset(ineq.funds, category=="rev.pc"))), colour=wes_palette("Darjeeling")[1], size = 4, parse=TRUE) +
  geom_smooth(data=subset(ineq.funds, category=="exp.pc"),se=TRUE, colour=wes_palette("Darjeeling")[2],size=1) +
  annotate("text", x = 0.77, y = 5, label = LmEq(gam(value ~ aland.gini, data=subset(ineq.funds, category=="exp.pc"))), colour=wes_palette("Darjeeling")[2], size = 4, parse=TRUE) +
  coord_cartesian(ylim=c(0,75)) +
  scale_colour_manual(name="State capacity measure",
                      values=c(exp.pc=wes_palette("Darjeeling")[2], rev.pc=wes_palette("Darjeeling")[1]),
                      label=c("Per-capita revenue",
                              "Per-capita expenditure")) +
  ylab("State capacity") +
  xlab("Land inequality") 

ggsave(paste0(results.directory,"plots/ineq-capacity.png"), ineq.capacity, width=11, height=8.5)