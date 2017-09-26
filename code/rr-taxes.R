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

rr.taxpc <- rr.taxpc[rr.taxpc$track2<=1,] # share max is 1

# Scatter plot (each point state/year observation) 

rr.taxpc.plot <- ggplot(rr.taxpc, aes(track2, value, color = factor(variable))) + 
  geom_point() +
  geom_smooth(data=subset(rr.taxpc, variable=="taxpc1"),se=TRUE, colour=wes_palette("FantasticFox")[3],size=1) +
  annotate("text", x = 0.7, y = 4, label = LmEq(gam(value ~ track2, data=subset(rr.taxpc, variable=="taxpc1"))), colour=wes_palette("FantasticFox")[3], size = 4, parse=TRUE) +
  geom_smooth(data=subset(rr.taxpc, variable=="taxpc2"),se=TRUE, colour=wes_palette("Moonrise3")[2],size=1) +
  annotate("text", x = 0.75, y = 10, label = LmEq(gam(value ~ track2, data=subset(rr.taxpc, variable=="taxpc2"))), colour=wes_palette("Moonrise3")[2], size = 4, parse=TRUE) +
  coord_cartesian(ylim=c(0,45),
                  xlim=c(0,0.9)) +
  scale_colour_manual(name="Measure",
                      values=c(taxpc2=wes_palette("Moonrise3")[2], taxpc1=wes_palette("FantasticFox")[3]),
                      label=c("Tax1",
                              "Tax2")) +
  ylab("Per-capita taxes") +
  xlab("Railroad rack miles per square mile") 

ggsave(paste0(results.directory,"plots/rr-taxpc.png"), rr.taxpc.plot , width=11, height=8.5)