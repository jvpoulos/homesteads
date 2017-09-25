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

# summarize RR data by state

rr.inter.m.state <- rr.inter.m %>% 
  group_by(state,InOpBy) %>% 
  summarise_each(funs(sum(., na.rm = TRUE)),cumulative.track,AREA_SQMI) %>% # state/year sums
  select(state,InOpBy,AREA_SQMI,cumulative.track)

rr.inter.m.state$track2 <- rr.inter.m.state$cumulative.track/rr.inter.m.state$AREA_SQMI # state ratio

# merge funds with rr data

rr.funds <- merge(rr.inter.m.state,  
                  funds.m, 
                  by.x =c("state","InOpBy"),
                  by.y = c("state","year"))

# Scatter plot (each point state/year observation) 

LmEq <- function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            p.val = format(summary(m)[[4]][2], digits = 2));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(p)~"="~p.val,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(p)~"="~p.val,l)    
  }
  
  as.character(as.expression(eq));                 
}

rr.capacity <- ggplot(rr.funds, aes(track2, value, color = factor(variable))) + 
  geom_point() +
  geom_smooth(data=subset(rr.funds, variable=="rev.pc"),se=TRUE, colour=wes_palette("FantasticFox")[3],size=1) +
  annotate("text", x = 0.27, y = 4.8, label = LmEq(gam(value ~ track2, data=subset(rr.funds, variable=="rev.pc"))), colour=wes_palette("FantasticFox")[3], size = 4, parse=TRUE) +
  geom_smooth(data=subset(rr.funds, variable=="exp.pc"),se=TRUE, colour=wes_palette("Moonrise3")[2],size=1) +
  annotate("text", x = 0.25, y = 0.2, label = LmEq(gam(value ~ track2, data=subset(rr.funds, variable=="exp.pc"))), colour=wes_palette("Moonrise3")[2], size = 4, parse=TRUE) +
  coord_cartesian(ylim=c(0,25)) +
  scale_colour_manual(name="State capacity measure",
                      values=c(exp.pc=wes_palette("Moonrise3")[2], rev.pc=wes_palette("FantasticFox")[3]),
                      label=c("Per-capita revenue",
                              "Per-capita expenditure")) +
  ylab("State capacity") +
  xlab("Railroad rack miles per square mile") 
 # ggtitle("Railroad access vs. state capacity, 1830-1911 (states)") 

ggsave(paste0(results.directory,"plots/rr-capacity.png"), rr.capacity, width=11, height=8.5)