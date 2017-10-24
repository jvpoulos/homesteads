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


# merge taxpc with rr data

rr.inter.m$fips <- as.numeric(as.character(rr.inter.m$FIPS))

rr.taxpc <- merge(rr.inter.m,  
                  taxpc.m, 
                  by.x =c("fips","InOpBy"),
                  by.y = c("fips","year"))

# Create state means
rr.taxpc.m <- rr.taxpc %>% 
  select(InOpBy, state, access, variable, value)  %>%
  group_by(InOpBy,state, variable) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

# Scatter plot (each point state/year observation) 

rr.taxpc.m.plot <- ggplot(rr.taxpc.m , aes(access, value, color = factor(variable))) + 
  geom_point() +
  geom_smooth(data=subset(rr.taxpc.m , variable=="taxpc1"),se=TRUE, colour=wes_palette("FantasticFox")[3],size=1) +
  annotate("text", x = 0.2, y = 1, label = LmEq(gam(value ~ access, data=subset(rr.taxpc.m , variable=="taxpc1"))), colour=wes_palette("FantasticFox")[3], size = 4, parse=TRUE) +
  geom_smooth(data=subset(rr.taxpc.m , variable=="taxpc2"),se=TRUE, colour=wes_palette("Moonrise3")[2],size=1) +
  annotate("text", x = 0.25, y = 1, label = LmEq(gam(value ~ access, data=subset(rr.taxpc.m , variable=="taxpc2"))), colour=wes_palette("Moonrise3")[2], size = 4, parse=TRUE) +
  coord_cartesian(ylim=c(-10,10),
                  xlim=c(0,0.3)) +
  scale_colour_manual(name="Measure",
                      values=c(taxpc2=wes_palette("Moonrise3")[2], taxpc1=wes_palette("FantasticFox")[3]),
                      label=c("Tax1",
                              "Tax2")) +
  ylab("Log per-capita taxes") +
  xlab("Railroad access") 

ggsave(paste0(results.directory,"plots/rr-taxpc.png"), rr.taxpc.plot , width=11, height=8.5)